using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;
using SolarixGrammarEngineNET;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    using ConcreteUnderstander = Func<ParsedSentence, MayBe<(UnderstoodSentence UnderstoodSentence, int UnderstandingConfidenceLevel)>>;

    using static DomainApi;
    using static Either;
    using static MayBe;
    using static MeaningMetaModifiers;
    using static Prolog.Engine.Parsing.PrologParser;
    using static Impl;

    internal sealed record PatternWithMeaning(DisambiguatedSentence Pattern, SentenceMeaning Meaning);

    internal static class PatternBuilder
    {
        public static ConcreteUnderstander BuildPattern(
            string patternId, 
            PatternWithMeaning patternWithMeaning,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            SentenceUnderstander sentenceUnderstander)
        {
            var (pattern, meaning) = patternWithMeaning;

            var patternElements = pattern.SentenceStructure.IterateByPosition().ToArray();

            var pathesToWords = new PathesToWords(
                patternElements
                    .GroupBy(it => it.Word.Fold(word => word.LemmaVersion.Lemma, () => it.Content))
                    .ToDictionary(
                        g => g.Key,
                        g => g.Select(it => (it.PositionInSentence, it.Word.Map(word => word.LemmaVersion))).AsImmutable(),
                        RussianIgnoreCase),
                pattern.Sentence);

            var allWordsUsedInMeaning = new HashSet<string>(
                meaning.Fold(
                    rules => rules.SelectMany(ListUsedWords), 
                    statements => statements.SelectMany(ListUsedWords)),
                RussianIgnoreCase);

            var sentenceStructureChecker = BuildSentenceStructureChecker(
                patternId,
                patternElements,
                allWordsUsedInMeaning,
                russianLexicon);
            
            return meaning.Fold(
                rules => 
                {
                    var ruleBuilders = rules
                        .ConvertAll(rule => 
                            MakeRuleBuilder(pattern, rule, pathesToWords, grammarParser, sentenceUnderstander));
                    return BuildPatternCore(sentence => 
                        Sequence(ruleBuilders.ConvertAll(rb => rb(sentence)))
                            .Map(Left<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>));
                },
                statements =>
                {
                    var statementBuilders = statements
                        .ConvertAll(statement => 
                            MakeComplexTermBuilder(pattern, statement, pathesToWords, grammarParser, sentenceUnderstander));
                    return BuildPatternCore(sentence => 
                        Sequence(statementBuilders.ConvertAll(sb => sb(sentence)))
                            .Map(Right<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>));
                });

            ConcreteUnderstander BuildPatternCore(
                Func<ParsedSentence, MayBe<SentenceMeaning>> buildMeaning) 
            {
                PatternEstablished?.Invoke(patternId, pattern, meaning);
                return sentence => 
                    sentenceStructureChecker(sentence) switch
                    {
                        var sentenceStructureMatchingLevel when sentenceStructureMatchingLevel is not null => 
                            buildMeaning(sentence).Map(meaning1 => 
                                (
                                    new UnderstoodSentence(sentence, patternId, meaning1), 
                                    sentenceStructureMatchingLevel.Value
                                )),
                        _ => None
                    };
            }
        }

        public static event Action<string, DisambiguatedSentence, SentenceMeaning>? PatternEstablished;

        public static event Action<string, bool?>? PatternRecognitionEvent;

        private static Exception PatternBuildingException(string message, bool invalidOperation = false) =>
            invalidOperation 
                ? new InvalidOperationException(message) 
                : new NotSupportedException(message);

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

        private static Func<ParsedSentence, int?>
            BuildSentenceStructureChecker(
                string patternId, 
                IReadOnlyCollection<WordOrQuotation<DisambiguatedWord>> patternElements,
                IReadOnlySet<string> allWordsUsedInMeaning,
                IRussianLexicon russianLexicon)
        {
            var sentenceStructureChecker = 
                BuildSentenceStructureCheckerCore(patternElements, allWordsUsedInMeaning, russianLexicon);
            return parsedSentence => 
                    {
                        LogCheckingT(Unit.Instance, $"+++Pattern {patternId} will be checked against '{parsedSentence.Sentence}'");
                        var result = sentenceStructureChecker(parsedSentence);
                        LogCheckingT(Unit.Instance, $"---Applying pattern {patternId} to '{parsedSentence.Sentence}' {(result.HasValue ? "suceeded" : "failed")}");
                        return result;
                    };
        }

        private static Func<ParsedSentence, int?>
            BuildSentenceStructureCheckerCore(
                IReadOnlyCollection<WordOrQuotation<DisambiguatedWord>> patternElements,
                IReadOnlySet<string> allWordsUsedInMeaning, 
                IRussianLexicon russianLexicon)
        {
            var sentenceElementsCheckers = patternElements.ConvertAll(it => 
                        it.Word.Map(
                            word => Some(
                                new WordChecker(
                                    word.LemmaVersion, 
                                    ExpectParticularWord: word.IsFixed || 
                                                            !allWordsUsedInMeaning.Contains(word.LemmaVersion.Lemma)))));

            var repeatedWordsUsedInMeaning = patternElements
                    .Where(it => it.Word.HasValue && allWordsUsedInMeaning.Contains(it.Word.Value!.LemmaVersion.Lemma))
                    .GroupBy(it => it.Word.Value!.LemmaVersion.Lemma, Impl.RussianIgnoreCase)
                    .Where(g => g.HasMoreThanOneElement())
                    .Select(g => g.Select(w => w.PositionInSentence).AsImmutable())
                    .AsImmutable();

            return parsedSentence => 
            {
                var elements = parsedSentence.SentenceStructure.IterateByPosition().ToArray();
                if (!LogChecking(elements.Length == sentenceElementsCheckers.Count, "number of elements"))
                {
                    return null;
                }

                var result = sentenceElementsCheckers
                                .Zip(elements)
                                .AggregateWhile(
                                    (int?)0,
                                    (sentenceStructureMatchingLevel, it) =>
                                        sentenceStructureMatchingLevel + Check(it.First, it.Second, russianLexicon),
                                    sentenceStructureMatchingLevel => sentenceStructureMatchingLevel != null);

                return result != null && 
                        repeatedWordsUsedInMeaning.All(g => 
                                LogChecking(
                                    g.All(positionInSentence => !elements[positionInSentence].IsQuote),
                                    "1. checked sentence should contain words at all indexes") &&
                                LogChecking(
                                    g.Skip(1).Aggregate(
                                        new HashSet<string>(ListWordLemmas(elements[g.First()].Word.Value!), RussianIgnoreCase), 
                                        (lemmas, positionInSentence) => 
                                        {
                                            lemmas.IntersectWith(ListWordLemmas(elements[positionInSentence].Word.Value!));
                                            return lemmas;
                                        }).Any(),
                                    "2. for each groups of positions in sentence that contain the same word in pattern, " + 
                                        "there should be the same words in the checked sentence as well"))
                        ? result
                        : null;
            };

            static int? Check(
                MayBe<WordChecker> wordChecker, 
                WordOrQuotation<Word> actualWord, 
                IRussianLexicon russianLexicon) 
            =>
                wordChecker
                    .Map(checker => actualWord.Word.Fold(word => checker.Check(word, actualWord.Content, russianLexicon), () => null))
                    .OrElse(() => LogChecking(actualWord.IsQuote, "expected quotation") ? 0 : null);

            static IEnumerable<string> ListWordLemmas(Word word) =>
                word.LemmaVersions.Select(lv => lv.Lemma);
        }

        private sealed record WordChecker(LemmaVersion LemmaVersion, bool ExpectParticularWord)
        {
            public int? Check(Word word, string content, IRussianLexicon russianLexicon) => 
                (word.LemmaVersions.Any(CheckLemmaVersion) ? 0 : default(int?)) ?? 
                (word.LemmaVersions.Any(lv => CheckWordForms(lv, content, russianLexicon)) ? 1 : default(int?));

            private bool CheckLemmaVersion(LemmaVersion lv) =>
                ExpectParticularWord
                    ? LogChecking(
                            Impl.RussianIgnoreCase.Equals(lv.Lemma, LemmaVersion.Lemma), 
                            $"expected to see '{LemmaVersion.Lemma}', saw '{lv.Lemma}'")
                    : LogChecking(
                            lv.Characteristics.CompatibleTo(LemmaVersion.Characteristics),
                            $"{lv.Characteristics}({lv.Lemma}) is compatible to expected " + 
                            $"{LemmaVersion.Characteristics}({LemmaVersion.Lemma})");

            private bool CheckWordForms(LemmaVersion lv, string wordContent, IRussianLexicon russianLexicon) =>
                !ExpectParticularWord && 
                russianLexicon
                    .GenerateWordForms(
                        lv.EntryId, 
                        LemmaVersion.Characteristics.ToCoordIdStateIdPairArray(
                            (coordId, stateId) => coordId != GrammarEngineAPI.GENDER_ru
                                ? (coordId, stateId)
                                : default((int, int)?)))
                    .Any(wordForm => LogChecking(
                                        Impl.RussianIgnoreCase.Equals(wordForm, wordContent),
                                        $"whether word {wordContent} can be treated as {lv.Lemma} in form {LemmaVersion.Characteristics}"));
        }

        private static Func<ParsedSentence, MayBe<Rule>> MakeRuleBuilder(
            DisambiguatedSentence pattern,
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

        private static Func<ParsedSentence, MayBe<ComplexTerm>> MakeComplexTermBuilder(
            DisambiguatedSentence pattern, 
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

        private static Func<ParsedSentence, FunctorBase> MakeFunctorBuilder(
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

        private static Func<ParsedSentence, MayBe<Term>> MakeTermBuilder(
            DisambiguatedSentence pattern, 
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

        internal static T LogCheckingT<T>(T extraInfo, string log)
        {
            PatternRecognitionEvent?.Invoke(log, default);
            return extraInfo;
        }

        internal static readonly IReadOnlySet<string> BuiltinPrologFunctors = 
            new HashSet<string>(
                Builtin.Functors
                    .Concat(Builtin.Rules.Select(r => r.Conclusion.Functor))
                    .Select(f => f.Name));

        private record PathesToWords(
            IReadOnlyDictionary<string, IReadOnlyCollection<(int PositionInSentence, MayBe<LemmaVersion> LemmaVersion)>> Pathes,
            string Sentence)
        {
            public Func<ParsedSentence, string> LocateWord(string text, bool capitalizeFirstWord = false) =>
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

            private Func<ParsedSentence, string> LocateSingleWord(string word) =>
                Pathes
                    .TryFind(word)
                    .Map(value => 
                    {
                        var (positionInSentence, lemmaVersion) = value.First();
                        return lemmaVersion.HasValue
                            ? parsedSentence => parsedSentence.GetRelevantLemmaAt(positionInSentence, lemmaVersion.Value!, ReportException).Lemma
                            : new Func<ParsedSentence, string>(parsedSentence => parsedSentence.GetQuotationAt(positionInSentence, ReportException));
                        Exception ReportException(string message) => PatternBuildingException(message, invalidOperation: true);
                    })
                    .OrElse(() => IsIntroducedVariable(word)
                        ? _ => word
                        : throw PatternBuildingException(
                            $"Could not find word {word} in the pattern '{Sentence}'. " +
                            $"Only these words are present: [{string.Join(", ", Pathes.Keys)}]"));

            private static bool IsIntroducedVariable(string word) =>
                TryParseTerm(word).OrElse(() => Atom("a")) is Variable;
        }
   }
}