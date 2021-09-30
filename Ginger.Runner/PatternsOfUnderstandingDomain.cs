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
        public static IEnumerable<ConcreteUnderstander> BuildUnderstanders(
            string patternId, 
            PatternWithMeaning patternWithMeaning,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            SentenceUnderstander sentenceUnderstander)
        {
            var (pattern, meaning) = patternWithMeaning;

            var allWordsUsedInMeaning = new HashSet<string>(
                meaning.Fold(
                    rules => rules.SelectMany(ListUsedWords), 
                    statements => statements.SelectMany(ListUsedWords)),
                RussianIgnoreCase);

            var patternElements = pattern.SentenceStructure.IterateByPosition().AsImmutable();

            var pathesToWords = new PathesToWords(
                patternElements
                    .GroupBy(it => it.Word.Fold(word => word.LemmaVersion.Lemma, () => it.Content))
                    .ToDictionary(
                        g => g.Key,
                        g => g.Select(it => (it.PositionInSentence, it.Word.Map(word => word.LemmaVersion))).AsImmutable(),
                        RussianIgnoreCase),
                pattern.Sentence);

            var sentenceElementsCheckers = patternElements.ConvertAll(it => 
                        it.Word.Map(
                            word => new WordChecker(
                                    word.LemmaVersion, 
                                    ExpectParticularWord: word.IsFixed || 
                                                            !allWordsUsedInMeaning.Contains(word.LemmaVersion.Lemma))));

            var regularUnderstander = BuildRegularUnderstander();
            yield return regularUnderstander;

            var patternAllowsDroppingQuotes = 
                    patternElements.Any() && 
                    patternElements.All(element => element.Word.Map(w => w.IsFixed).OrElse(true));

            if (patternAllowsDroppingQuotes)
            {
                var sequencesOfExactWords = sentenceElementsCheckers.Split(
                    wordChecker => !wordChecker.HasValue, wordChecker => wordChecker.Value!);
                if (sequencesOfExactWords.Any())
                {
                    yield return BuildDroppingQuotesUnderstander(sequencesOfExactWords, regularUnderstander);
                }
            }

            ConcreteUnderstander BuildRegularUnderstander()
            {
                var sentenceStructureChecker = BuildRegularSentenceStructureChecker(
                    patternId,
                    patternElements,
                    allWordsUsedInMeaning,
                    sentenceElementsCheckers,
                    russianLexicon);
                
                return meaning.Fold(
                    rules => 
                    {
                        var ruleBuilders = rules
                            .ConvertAll(rule => 
                                MakeRuleBuilder(pattern, rule, pathesToWords, grammarParser, sentenceUnderstander));
                        return BuildPatternCore(
                            sentenceStructureChecker,
                            sentence => 
                                Sequence(ruleBuilders.ConvertAll(rb => rb(sentence)))
                                    .Map(Left<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>));
                    },
                    statements =>
                    {
                        var statementBuilders = statements
                            .ConvertAll(statement => 
                                MakeComplexTermBuilder(pattern, statement, pathesToWords, grammarParser, sentenceUnderstander));
                        return BuildPatternCore(
                            sentenceStructureChecker,
                            sentence => 
                                Sequence(statementBuilders.ConvertAll(sb => sb(sentence)))
                                    .Map(Right<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>));
                    });
            }

            ConcreteUnderstander BuildDroppingQuotesUnderstander(
                IReadOnlyCollection<IReadOnlyCollection<WordChecker>> sequencesOfExactWords, 
                ConcreteUnderstander quotedPhraseUnderstander)
            {
                var sentenceStructureChecker = BuildSentenceStructureCheckerDroppingQuotes(
                    patternId,
                    patternElements, 
                    sequencesOfExactWords, 
                    russianLexicon);

                return sentence =>
                    sentenceStructureChecker(sentence).Map(unquotedWordsRanges =>
                        quotedPhraseUnderstander(sentence.IntroduceQuotes(unquotedWordsRanges, grammarParser))
                        .Map(result => 
                            (
                             result.UnderstoodSentence with 
                                { 
                                    PatternId = result.UnderstoodSentence.PatternId + DroppedQuotes 
                                },
                             result.UnderstandingConfidenceLevel
                            )));
            }

            ConcreteUnderstander BuildPatternCore(
                Func<ParsedSentence, int?> sentenceStructureChecker,
                Func<ParsedSentence, MayBe<SentenceMeaning>> buildMeaning) 
            {
                PatternEstablished?.Invoke(patternId, pattern, meaning);
                return sentence => 
                    sentenceStructureChecker(sentence) switch
                    {
                        var sentenceStructureMatchingLevel when sentenceStructureMatchingLevel is not null => 
                            buildMeaning(sentence).Map(actualMeaning => 
                                (
                                    new UnderstoodSentence(sentence, patternId, actualMeaning), 
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
            BuildRegularSentenceStructureChecker(
                string patternId, 
                IReadOnlyCollection<WordOrQuotation<DisambiguatedWord>> patternElements,
                IReadOnlySet<string> allWordsUsedInMeaning,
                IReadOnlyCollection<MayBe<WordChecker>> sentenceElementsCheckers,
                IRussianLexicon russianLexicon)
        {
            var sentenceStructureChecker = 
                BuildSentenceStructureCheckerCore(patternElements, allWordsUsedInMeaning, sentenceElementsCheckers, russianLexicon);
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
                IReadOnlyCollection<MayBe<WordChecker>> sentenceElementsCheckers,
                IRussianLexicon russianLexicon)
        {
            var wordsUsedInMeaningTwiceOrMore = patternElements
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
                        wordsUsedInMeaningTwiceOrMore.All(g => 
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

            static IEnumerable<string> ListWordLemmas(Word word) =>
                word.LemmaVersions.Select(lv => lv.Lemma);
        }

        private static Func<ParsedSentence, MayBe<List<Range>>> BuildSentenceStructureCheckerDroppingQuotes(
            string patternId,
            IReadOnlyCollection<WordOrQuotation<DisambiguatedWord>> patternElements,
            IReadOnlyCollection<IReadOnlyCollection<WordChecker>> sequencesOfExactWords,
            IRussianLexicon russianLexicon)
        {
            return parsedSentence => 
            {
                LogCheckingT(Unit.Instance, $"+++Pattern {patternId}{DroppedQuotes} will be checked against '{parsedSentence.Sentence}'");

                var elements = parsedSentence.SentenceStructure.IterateByPosition().ToArray();
                var startsWithExactWords = !patternElements.First().IsQuote;
                var endsWithExactWords = !patternElements.Last().IsQuote;

                var matchedSequences = new List<Range>();
                var i = Index.FromStart(0);
                foreach (var sequenceOfExactWords in sequencesOfExactWords)
                {
                    var matchingSequenceRange = TryFindMatchingSequence(elements, sequenceOfExactWords, i);
                    if (matchingSequenceRange == null)
                    {
                        break;
                    }

                    i = Index.FromStart(matchingSequenceRange.Value.End.Value + 1);
                    matchedSequences.Add(matchingSequenceRange.Value);
                }

                var result = 
                        LogChecking(
                            matchedSequences.Count == sequencesOfExactWords.Count,
                            $"finding macthes for all {sequencesOfExactWords.Count} sequences of exact words") &&
                        LogChecking(
                           !startsWithExactWords || matchedSequences.First().Start.Equals(Index.Start),
                           "checking the first match's location") &&
                        LogChecking(
                           !endsWithExactWords || matchedSequences.Last().End.Equals(new Index(elements.Length)),
                           "checking the last match's location")
                        ? Some(matchedSequences)
                        : None;
                LogCheckingT(Unit.Instance, $"---Applying pattern {patternId}{DroppedQuotes} to '{parsedSentence.Sentence}' {(result.HasValue ? "suceeded" : "failed")}");
                return result;
            };

            Range? TryFindMatchingSequence(WordOrQuotation<Word>[] elements, IReadOnlyCollection<WordChecker> sequenceOfExactWords, Index start) =>
                Enumerable
                    .Range(start.Value, elements.Length - start.Value - sequenceOfExactWords.Count + 1)
                    .TryFirst(i => Enumerable.Range(0, sequenceOfExactWords.Count).All(
                        j => Check(Some(sequenceOfExactWords.ElementAt(j)), elements[i + j], russianLexicon) == 0))
                    .Map(i => new Range(Index.FromStart(i), Index.FromStart(i + sequenceOfExactWords.Count)))
                    .AsNullable();
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

        private static int? Check(
            MayBe<WordChecker> wordChecker, 
            WordOrQuotation<Word> actualWord, 
            IRussianLexicon russianLexicon) 
        =>
            wordChecker
                .Map(checker => actualWord.Word.Fold(word => checker.Check(word, actualWord.Content, russianLexicon), () => null))
                .OrElse(() => LogChecking(actualWord.IsQuote, "expected quotation") ? 0 : null);

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

        private const string DroppedQuotes = "-DroppedQuotes";

   }
}