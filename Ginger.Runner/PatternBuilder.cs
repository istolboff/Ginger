using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using WordOrQuotation = Either<Word, Quotation>;
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    using ConcreteUnderstander = Func<ParsedSentence, MayBe<UnderstoodSentence>>;

    using static DomainApi;
    using static Either;
    using static MayBe;
    using static MeaningMetaModifiers;
    using static Prolog.Engine.Parsing.PrologParser;
    using static Impl;

    internal static class PatternBuilder
    {
        public static ConcreteUnderstander BuildPattern(
            string patternId, 
            AnnotatedSentence pattern,
            SentenceMeaning meaning,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            SentenceUnderstander sentenceUnderstander)
        {
            var allWordsUsedInMeaning = new HashSet<string>(
                meaning.Fold(
                    rules => rules.SelectMany(ListUsedWords), 
                    statements => statements.SelectMany(ListUsedWords)),
                RussianIgnoreCase);
            var (sentenceStructureChecker, pathesToWords) = BuildSentenceStructureChecker(
                patternId,
                pattern,
                allWordsUsedInMeaning,
                ImmutableStack.Create<int>(),
                new Dictionary<string, PathToWordBase>(RussianIgnoreCase),
                russianLexicon);
            
            return meaning.Fold(
                rules => 
                {
                    var ruleBuilders = rules
                        .ConvertAll(rule => 
                            MakeRuleBuilder(pattern.Sentence, rule, pathesToWords, grammarParser, sentenceUnderstander));
                    return BuildPatternCore(sentence => 
                        Sequence(ruleBuilders.ConvertAll(rb => rb(sentence)))
                            .Map(Left<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>));
                },
                statements =>
                {
                    var statementBuilders = statements
                        .ConvertAll(statement => 
                            MakeComplexTermBuilder(pattern.Sentence, statement, pathesToWords, grammarParser, sentenceUnderstander));
                    return BuildPatternCore(sentence => 
                        Sequence(statementBuilders.ConvertAll(sb => sb(sentence)))
                            .Map(Right<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>));
                });

            ConcreteUnderstander BuildPatternCore(
                Func<WordOrQuotation, MayBe<SentenceMeaning>> buildMeaning) 
            {
                PatternEstablished?.Invoke(patternId, pattern, meaning);
                return sentence => 
                    sentenceStructureChecker(new CheckableSentenceElement(sentence, sentence.SentenceSyntax)) switch
                    {
                        true => buildMeaning(sentence.SentenceSyntax)
                                    .Map(meaning1 => new UnderstoodSentence(sentence, patternId, meaning1)),
                        _ => None
                    };
            }
        }

        public static Exception PatternBuildingException(string message, bool invalidOperation = false) =>
            invalidOperation 
                ? new InvalidOperationException(message) 
                : new NotSupportedException(message);

        public static event Action<string, AnnotatedSentence, SentenceMeaning>? PatternEstablished;

        public static event Action<string, bool>? PatternRecognitionEvent;

        private static IEnumerable<string> ListUsedWords(Rule meaning) =>
            ListUsedWords(meaning.Conclusion).Concat(meaning.Premises.SelectMany(ListUsedWords));

        private static IEnumerable<string> ListUsedWords(ComplexTerm complexTerm) =>
            complexTerm.Functor.Name.SplitAtUpperCharacters()
            .Concat(complexTerm.Arguments.SelectMany(t =>
                t switch
                {
                    Atom atom => atom.Characters.SplitAtUpperCharacters(),
                    Variable variable => variable.Name.SplitAtUpperCharacters(),
                    ComplexTerm ct => ListUsedWords(ct),
                    _ => Enumerable.Empty<string>()
                }));

        private static (Func<CheckableSentenceElement, bool> Checker, PathesToWords Pathes)
            BuildSentenceStructureChecker(
                string patternId, 
                AnnotatedSentence pattern,
                IReadOnlySet<string> allWordsUsedInMeaning,
                ImmutableStack<int> wordIndexes,
                Dictionary<string, PathToWordBase> pathesToWords,
                IRussianLexicon russianLexicon)
        {
            var sentenceStructureChecker = 
                BuildSentenceStructureCheckerCore(pattern, allWordsUsedInMeaning, wordIndexes, pathesToWords, russianLexicon);
            return (
                    checkableElement => 
                    {
                        var result = sentenceStructureChecker(checkableElement);
                        LogChecking(result, $"Applying pattern {patternId} to '{checkableElement.Root.Sentence.Replace(Environment.NewLine, " ")}'");
                        return result;
                    }, 
                    new (pathesToWords)
                   );
        }

        private static Func<CheckableSentenceElement, bool>
            BuildSentenceStructureCheckerCore(
                AnnotatedSentence pattern,
                IReadOnlySet<string> allWordsUsedInMeaning,
                ImmutableStack<int> wordIndexes,
                Dictionary<string, PathToWordBase> pathesToWords,
                IRussianLexicon russianLexicon)
        {
            return pattern.Sentence.Fold(BuildWordChecker, BuildQuotationChecker);

            Func<CheckableSentenceElement, bool> BuildWordChecker(Word currentNode)
            {
                if (currentNode.LemmaVersions.Count > 1)
                {
                    var annotationVariants = string.Join(";", BuildDisambiguatingAnnotations(currentNode.LemmaVersions));
                    throw PatternBuildingException(
                            $"There are several lemma versions of the word '{currentNode.Content} ({currentNode.PositionInSentence})' in pattern '{pattern.Text}'. " +
                            $"You can annotate the word with one of the following variants {annotationVariants}, " + 
                            "or reformulate the pattern wording.", 
                            invalidOperation: true);
                }

                var lemmaVersion = currentNode.LemmaVersions.Single();
                var targetLemmaVersion = new TargetLemmaVersion(
                            lemmaVersion.PartOfSpeech, 
                            lemmaVersion.Characteristics);
                var isPunctuationSymbol = lemmaVersion.Lemma.IsOneOf(",", ":", ";");
                var currentNodeIsAnnotated = pattern.WordIsAnnotated(currentNode.Content, lemmaVersion);
                PathToWordBase? pathToExpectedWord = null;
                if (!isPunctuationSymbol && !pathesToWords.TryGetValue(lemmaVersion.Lemma, out pathToExpectedWord))
                {
                    pathesToWords.Add(lemmaVersion.Lemma, new PathToWord(wordIndexes, targetLemmaVersion));
                }

                var particularWordIsExpectedAtThisPlaceInSentence = 
                    isPunctuationSymbol || 
                    !allWordsUsedInMeaning.Contains(lemmaVersion.Lemma) ||
                    currentNodeIsAnnotated;

                var childCheckers = BuildChildCheckers(currentNode.Children);

                return checkableElement => 
                    checkableElement.Current.Fold(word => word, _ => default(Word)) is var currentWord &&
                    LogChecking(currentWord != null, log: "current element is not quotation") &&
                    LogChecking(log: currentWord!.Content) && 
                    LogChecking(
                        currentWord.Children.Count == childCheckers.Length, 
                        log: $"{childCheckers.Length} children expected") &&
                    LogChecking(
                        pathToExpectedWord == null ||
                        currentWord.LemmaVersions.Any(lv => lv.Lemma == pathToExpectedWord.GetWordFrom(checkableElement.Root.SentenceSyntax)),
                        log: $"expecting '{pathToExpectedWord?.GetWordFrom(checkableElement.Root.SentenceSyntax)}' at {checkableElement}") &&
                    LogChecking(
                        targetLemmaVersion.FindRelevantLemma(currentWord.LemmaVersions).HasValue, 
                        log: "Searching for relevant lemma. " + 
                            $"Expected: {targetLemmaVersion.PartOfSpeech}-{targetLemmaVersion.GrammarCharacteristics} ({lemmaVersion.Lemma}) " + 
                            $"Actual: {string.Join(";", currentWord.LemmaVersions.Select(lv => lv.PartOfSpeech + "-" + lv.Characteristics))} ({currentWord.Content})") &&
                    (
                    !particularWordIsExpectedAtThisPlaceInSentence || 
                    LogChecking(
                        currentWord.LemmaVersions.Any(
                            lv => lv.Lemma == lemmaVersion.Lemma && 
                            lv.Characteristics.CompatibleTo(lemmaVersion.Characteristics)), 
                        log: $"Particular lemma '{lemmaVersion}' is expected at {checkableElement}")
                    ) &&
                    childCheckers
                        .Zip(currentWord.Children)
                        .All(it => it.First(checkableElement with { Current = it.Second }));
            }

            Func<CheckableSentenceElement, bool> BuildQuotationChecker(Quotation quotation)
            {
                var childCheckers = BuildChildCheckers(quotation.Children);
                if (!pathesToWords.ContainsKey(quotation.Content))
                {
                    pathesToWords.Add(quotation.Content, new PathToQuotation(wordIndexes));
                }

                return checkableElement => 
                        checkableElement.Current.Fold(
                            _ => LogChecking(false, log: "current element is quotation"),
                            checkedQuotation => 
                                childCheckers
                                    .Zip(checkedQuotation.Children)
                                    .All(it => it.First(checkableElement with { Current = it.Second })));
            }

            Func<CheckableSentenceElement, bool>[] BuildChildCheckers(IReadOnlyList<WordOrQuotation> children)
            {
                var childCheckers = new Func<CheckableSentenceElement, bool>[children.Count];
                for (var i = 0; i != children.Count; ++i)
                {
                    childCheckers[i] = BuildSentenceStructureCheckerCore(
                                            pattern with { Sentence = children[i] }, 
                                            allWordsUsedInMeaning, 
                                            wordIndexes.Push(i), 
                                            pathesToWords,
                                            russianLexicon);
                }

                return childCheckers;
            }

            string BuildDisambiguatingAnnotations(IReadOnlyCollection<LemmaVersion> lemmaVersions) =>
                string.Join(
                    ", ", 
                    LemmaVersionDisambiguator.Create(lemmaVersions)
                        .ProposeDisambiguations(russianLexicon));
        }

        private static Func<WordOrQuotation, MayBe<Rule>> MakeRuleBuilder(
            WordOrQuotation pattern,
            Rule rule,
            PathesToWords words,
            IRussianGrammarParser grammarParser,
            SentenceUnderstander sentenceUnderstander)
        {
            var conclusionBuilder = MakeComplexTermBuilder(pattern, rule.Conclusion, words, grammarParser, sentenceUnderstander);
            var premiseBuilders = rule.Premises.ConvertAll(premise => MakeComplexTermBuilder(pattern, premise, words, grammarParser, sentenceUnderstander));
            return sentence => 
                    from conclusion in conclusionBuilder(sentence)
                    from premises in MayBe.Sequence(premiseBuilders.ConvertAll(pb => pb(sentence)))
                    select Rule(conclusion, premises);
        }

        private static Func<WordOrQuotation, MayBe<ComplexTerm>> MakeComplexTermBuilder(
            WordOrQuotation pattern, 
            ComplexTerm complexTerm,
            PathesToWords words,
            IRussianGrammarParser grammarParser,
            SentenceUnderstander sentenceUnderstander)
        {
            switch (complexTerm.Functor.Name)
            {
                case MeaningMetaModifiers.Understand:
                {
                    var singleArgument = complexTerm.Arguments.Single(
                        _ => true, 
                        _ => MetaModifierError(
                                $"Meta-modifier '{complexTerm.Functor.Name}' requires exactly one argument."));

                    var quoteTextInPatern = 
                        ((singleArgument as Atom) 
                            ?? throw MetaModifierError($"The only argument of {complexTerm.Functor.Name} should be an atom."))
                            .Characters;

                    return MakeUnderstander(
                                grammarParser, 
                                sentenceUnderstander, 
                                words.LocateWord(quoteTextInPatern));
                }

                default:
                {
                    var functorBuilder = MakeFunctorBuilder(complexTerm.Functor, words);
                    var argumentBuilders = complexTerm.Arguments.ConvertAll(
                                        arg => MakeTermBuilder(pattern, arg, words, grammarParser, sentenceUnderstander));
                    return sentence =>
                                MayBe
                                    .Sequence(argumentBuilders.ConvertAll(ab => ab(sentence)))
                                    .Map(arguments => AccomodateInlinedArguments(functorBuilder(sentence), arguments));
                }
            }
        }

        private static Func<WordOrQuotation, FunctorBase> MakeFunctorBuilder(
            FunctorBase functor,
            PathesToWords words)
        {
            if (BuiltinPrologFunctors.Contains(functor.Name) || IsMetaModifier(functor))
            {
                return _ => functor;
            }
            
            if (functor is Functor f)
            {
                var functorNameGetter = words.LocateWord(f.Name);
                return sentence => Functor(functorNameGetter(sentence), functor.Arity);
            }

            throw PatternBuildingException(
                $"Cannot handle functor '{functor.Name}' of type {functor.GetType().Name} in meanining pattern.");
        }

        private static Func<WordOrQuotation, MayBe<Term>> MakeTermBuilder(
            WordOrQuotation pattern, 
            Term term,
            PathesToWords words,
            IRussianGrammarParser grammarParser,
            SentenceUnderstander sentenceUnderstander)
        {
            if (term is Atom atom)
            {
                var atomNameGetter = words.LocateWord(atom.Characters);
                return sentence => Some<Term>(Atom(atomNameGetter(sentence)));
            }

            if (term is Prolog.Engine.Number number)
            {
                var numberValueGetter = words.LocateWord(number.Value.ToString(CultureInfo.CurrentCulture));
                return sentence => Some<Term>(Number(int.Parse(numberValueGetter(sentence), CultureInfo.CurrentCulture)));
            }

            if (term is Variable variable)
            {
                var variableNameGetter = words.LocateWord(variable.Name, capitalizeFirstWord: true);
                return sentence => Some<Term>(Variable(variableNameGetter(sentence)));
            }

            if (term is ComplexTerm complexTerm)
            {
                var complexTermBuilder = MakeComplexTermBuilder(pattern, complexTerm, words, grammarParser, sentenceUnderstander);
                return sentence => complexTermBuilder(sentence).Map(ct => ct as Term);
            }

            throw PatternBuildingException($"Term {term} is not supported.");
        }

        private static bool LogChecking(bool checkSucceeded = true, string? log = null)
        {
            PatternRecognitionEvent?.Invoke(log ?? string.Empty, checkSucceeded);
            return checkSucceeded;
        }

        internal static T LogChecking<T>(T extraInfo, string log, bool checkSucceeded = true)
        {
            PatternRecognitionEvent?.Invoke(string.Format(Russian, log, extraInfo), checkSucceeded);
            return extraInfo;
        }

        internal static readonly IReadOnlySet<string> BuiltinPrologFunctors = 
            new HashSet<string>(
                Builtin.Functors
                    .Concat(Builtin.Rules.Select(r => r.Conclusion.Functor))
                    .Select(f => f.Name));

        private record TargetLemmaVersion(PartOfSpeech? PartOfSpeech, GrammarCharacteristics GrammarCharacteristics)
        {
            public MayBe<LemmaVersion> FindRelevantLemma(IEnumerable<LemmaVersion> lemmaVersions) =>
                lemmaVersions.TryFirst(lm => 
                                PartOfSpeech == lm.PartOfSpeech &&
                                GrammarCharacteristics.CompatibleTo(lm.Characteristics));
        }

        private abstract record PathToWordBase(ImmutableStack<int> ChildrenIndexes)
        {
            public abstract string GetWordFrom(WordOrQuotation sentence);

            protected WordOrQuotation TravelToChild(WordOrQuotation sentence)
            {
                var currentNode = sentence;
                foreach (var childIndex in ChildrenIndexes.Reverse())
                {
                    currentNode = currentNode.Fold(word => word.Children, quotation => quotation.Children)[childIndex];
                }

                return currentNode;
            }
        }

#pragma warning disable CA1801 // Review unused parameters
        private sealed record PathToWord(ImmutableStack<int> ChildrenIndexes, TargetLemmaVersion LemmaVersion) : PathToWordBase(ChildrenIndexes)
#pragma warning restore CA1801
        {
            public override string GetWordFrom(WordOrQuotation sentence) =>
                TravelToChild(sentence).Fold(
                    word => LemmaVersion
                            .FindRelevantLemma(word.LemmaVersions)
                            .OrElse(() => throw PatternBuildingException(
                                $"Could not find {LemmaVersion.PartOfSpeech} lemma version " +
                                $"of type {LemmaVersion.GrammarCharacteristics.GetType().Name} " +
                                $"at element {word} in sentence {sentence}.",
                                invalidOperation: true))
                            .Lemma,
                    quotation => throw ProgramLogic.Error(
                        "Sentence structure checker for current pattern should have ensured that " + 
                        $"the SentenceElement at the path {string.Join(',', ChildrenIndexes)} is a regular word. " + 
                        $"In current senetence it's the {quotation}."));
        }

        private sealed record PathToQuotation(ImmutableStack<int> ChildrenIndexes) : PathToWordBase(ChildrenIndexes)
        {
            public override string GetWordFrom(WordOrQuotation sentence) =>
                TravelToChild(sentence).Fold(
                    word => throw ProgramLogic.Error(
                        "Sentence structure checker for current pattern should have ensured that " +
                        $"the SentenceElement at the path {string.Join(',', ChildrenIndexes)} is a quotation. " +
                        $"In current senetence it's the {word}."),
                    quotation => quotation.Content);
        }

        private record PathesToWords(IReadOnlyDictionary<string, PathToWordBase> Pathes)
        {
            public Func<WordOrQuotation, string> LocateWord(string text, bool capitalizeFirstWord = false) =>
                text.SplitAtUpperCharacters()
                    .Select(LocateSingleWord)
                    .AsImmutable()
                    .Apply(wordLocators => wordLocators.Count switch
                        {
                            0 => _ => string.Empty,
                            1 => wordLocators.Single(),
                            _ => sentenceElement => string.Join(
                                                        string.Empty, 
                                                        wordLocators.Select((wl, i) => 
                                                            i == 0 && !capitalizeFirstWord
                                                                ? wl(sentenceElement)
                                                                : Russian.TextInfo.ToTitleCase(wl(sentenceElement))))
                        });

            private Func<WordOrQuotation, string> LocateSingleWord(string word) =>
                Pathes
                    .TryFind(word)
                    .Map<Func<WordOrQuotation, string>>(p => p.GetWordFrom)
                    .OrElse(() => IsIntroducedVariable(word)
                        ? _ => word
                        : throw PatternBuildingException(
                            $"Could not find word {word} in the pattern sentence. " +
                            $"Only these words are present: [{string.Join(", ", Pathes.Keys)}]"));

            private static bool IsIntroducedVariable(string word) =>
                TryParseTerm(word).OrElse(() => Atom("a")) is Variable;
        }

        private record CheckableSentenceElement(ParsedSentence Root, WordOrQuotation Current);
   }
}