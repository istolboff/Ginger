using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using SolarixGrammarEngineNET;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using UnderstandingAttemptOutcome = Either<FailedUnderstandingAttempt, UnderstandingResult>;
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    using MeaningBuildingRecipe = Either<IReadOnlyCollection<RuleBuilingRecipe>, IReadOnlyCollection<ComplexTermBuilingRecipe>>;
    using FunctorBuildingRecipe = Either<FunctorBase /* BuiltIn Functor */, (NameBuilingRecipe FunctorNameBuildingRecipe, int Arity) /* Regular Functor building recipe */>;

    using static DomainApi;
    using static Either;
    using static Impl;
    using static MayBe;
    using static MeaningMetaModifiers;
    using static PatternBuilder;
    using static Prolog.Engine.Parsing.PrologParser;

    internal sealed record FailedUnderstandingAttempt(MayBe<string> PatternId, UnderstandingFailureReason FailureReason);

    internal enum NumberMismatchReason
    {
        NumberOfSentenceElementsDiffer,
        NumberOfSequencesOfExactWordsDiffer
    }

    internal enum MetaUnderstandingFailure
    {
        ProducedRulesInsteadOfStatements
    }

    internal abstract record UnderstandingFailureReason;
    internal sealed record WrongNumberOfElements(int ExpectedNumber, int ActualNumber, NumberMismatchReason MismatchReason) : UnderstandingFailureReason;
    internal sealed record SentenceShouldContainWordAtIndexes(IReadOnlyCollection<int> indexes) : UnderstandingFailureReason;
    internal sealed record SentenceShouldContainSameWordAtTheseIndexes(IReadOnlyCollection<int> indexes, WordOrQuotation<Word>[] elements) : UnderstandingFailureReason;
    internal sealed record SentenceShouldStartStopWithExactWords(bool ShouldStart, bool ExactWords) : UnderstandingFailureReason;
    internal sealed record WordExpected(WordChecker Checker, WordOrQuotation<Word> ActualWord) : UnderstandingFailureReason;
    internal sealed record QuotationExpected(WordOrQuotation<Word> actualWord) : UnderstandingFailureReason;
    internal sealed record CheckLemmaVersionsFailed(LemmaVersion ExpectedLemmaVersion, Word ActualWord, string ActualWordContent, bool ExpectParticularWord) : UnderstandingFailureReason;
    internal sealed record MetaUnderstandingFailed(string quote, MetaUnderstandingFailure reason) : UnderstandingFailureReason;
    internal sealed record EmptyNameBuilder : UnderstandingFailureReason;
    
    internal sealed record MultipleUnderstandingFailureReasons(IReadOnlyCollection<UnderstandingFailureReason> Reasons) : UnderstandingFailureReason
    {
        public static UnderstandingFailureReason CreateFrom(IReadOnlyCollection<UnderstandingFailureReason> reasons) =>
            reasons.Count == 1 
                ? reasons.Single()
                : new MultipleUnderstandingFailureReasons(reasons);

        public static UnderstandingFailureReason CreateFrom(IReadOnlyCollection<FailedUnderstandingAttempt> failedAttempts) =>
            CreateFrom(failedAttempts.ConvertAll(it => it.FailureReason));
    }

    internal sealed record CheckPatternApi(string PatternId)
    {
        public CheckPatternApi WithSuffix(string suffix) =>
            this with { PatternId = PatternId + suffix };

        public Either<FailedUnderstandingAttempt, Unit> CheckNumberOfElements(int expectedNumber, int actualNumber, NumberMismatchReason mismatchReason) =>
            actualNumber switch 
            {
                var _ when actualNumber == expectedNumber => Right(Unit.Instance),
                _ => FailedAttempt(new WrongNumberOfElements(ExpectedNumber: expectedNumber, ActualNumber: actualNumber, MismatchReason: mismatchReason))
            };

        public Either<FailedUnderstandingAttempt, Unit> CheckThatThereAreWordsAtIndexes(
            IReadOnlyCollection<int> g,
            WordOrQuotation<Word>[] elements) 
        =>
            g.Where(positionInSentence => elements[positionInSentence].IsQuote).AsImmutable() switch
            {
                var positionsWithUnexpectedQuotes when positionsWithUnexpectedQuotes.Any() => 
                     FailedAttempt(new SentenceShouldContainWordAtIndexes(positionsWithUnexpectedQuotes)),
                _ => Right(Unit.Instance)
            };

        public Either<FailedUnderstandingAttempt, Unit> CheckThatThereIsTheSameWordAtIndexes(
            IReadOnlyCollection<int> g,
            WordOrQuotation<Word>[] elements)
        => g.Skip(1).Aggregate(
                ListWordLemmas(elements[g.First()].Word.Value!).ToHashSet(), 
                (lemmas, positionInSentence) => 
                {
                    lemmas.IntersectWith(ListWordLemmas(elements[positionInSentence].Word.Value!));
                    return lemmas;
                }).AsImmutable() switch
                {
                    var repeatedEntryId when !repeatedEntryId.Any() => 
                         FailedAttempt(new SentenceShouldContainSameWordAtTheseIndexes(g, elements)),
                    _ => Right(Unit.Instance)
                };

        public Either<FailedUnderstandingAttempt, Unit> CheckThatSentenceStartsAndFinishesWithOrWithoutExactWords(
            bool startsWithExactWords, 
            bool endsWithExactWords, 
            List<Range> matchedSequences,
            WordOrQuotation<Word>[] elements)
        =>
            matchedSequences switch
            {
                var _ when startsWithExactWords && !matchedSequences.First().Start.Equals(Index.Start) =>
                    FailedAttempt(new SentenceShouldStartStopWithExactWords(ShouldStart: true, ExactWords: startsWithExactWords)),
                var _ when endsWithExactWords && !matchedSequences.Last().End.Equals(new Index(elements.Length)) =>
                    FailedAttempt(new SentenceShouldStartStopWithExactWords(ShouldStart: false, ExactWords: endsWithExactWords)),
                _ => Right(Unit.Instance)
            };

        public Either<FailedUnderstandingAttempt, int> Check(
            MayBe<WordChecker> wordChecker,
            WordOrQuotation<Word> actualWord,
            IRussianLexicon russianLexicon)
        =>
            ((wordChecker.HasValue, actualWord.Word.HasValue) switch
            {
                (true, true) => wordChecker.Value!.Check(actualWord.Word.Value!, actualWord.Content, russianLexicon),
                (true, false) => Left(new WordExpected(wordChecker.Value!, actualWord) as UnderstandingFailureReason),
                (false, true) => Left(new QuotationExpected(actualWord) as UnderstandingFailureReason),
                (false, false) => Right(0)
            })
            .MapLeft((Func<UnderstandingFailureReason, FailedUnderstandingAttempt>)(r => 
                new FailedUnderstandingAttempt(Some(PatternId), r)));

        private syntacticshugar_EitherFromLeft<FailedUnderstandingAttempt> FailedAttempt(
            UnderstandingFailureReason reason) =>
            Left(new FailedUnderstandingAttempt(Some(PatternId), reason));

        private static IEnumerable<int> ListWordLemmas(Word word) =>
            word.LemmaVersions.Select(lv => lv.EntryId);
    }

    internal sealed record UnderstandingResult(UnderstoodSentence UnderstoodSentence, int UnderstandingConfidenceLevel);

    internal sealed record PatternWithMeaning(DisambiguatedSentence Pattern, SentenceMeaning Meaning);

    internal abstract record ConcreteUnderstander(CheckPatternApi PatternChecks)
    {
        public abstract UnderstandingAttemptOutcome Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder);
    }

#pragma warning disable CA1801 // Review unused parameters
    internal sealed record RegularConcreteUnderstander(
        CheckPatternApi PatternChecks,
        IReadOnlyCollection<MayBe<WordChecker>> SentenceElementsCheckers,
        IReadOnlyCollection<IReadOnlyCollection<int>> WordsUsedInMeaningTwiceOrMore,
        MeaningBuildingRecipe MeaningBuildingRecipe,
        IRussianLexicon RussianLexicon)
            : ConcreteUnderstander(PatternChecks)
#pragma warning restore CA1801
    {
        public override UnderstandingAttemptOutcome Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder)
        =>
            from confidenceLevel in CheckSentenceStructure(sentence)
            from meaning in meaningBuilder.BuildMeaning(MeaningBuildingRecipe, sentence)
            select new UnderstandingResult(new UnderstoodSentence(sentence, PatternChecks.PatternId, meaning), confidenceLevel);

        public static RegularConcreteUnderstander Create(
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

            var wordsUsedInMeaningTwiceOrMore = patternElements
                    .Where(it => it.Word.HasValue && allWordsUsedInMeaning.Contains(it.Word.Value!.LemmaVersion.Lemma))
                    .GroupBy(it => it.Word.Value!.LemmaVersion.Lemma, Impl.RussianIgnoreCase)
                    .Where(g => g.HasMoreThanOneElement())
                    .Select(g => g.Select(w => w.PositionInSentence).AsImmutable())
                    .AsImmutable();

            return new (
                    new (patternId),
                    sentenceElementsCheckers,
                    wordsUsedInMeaningTwiceOrMore,
                    meaning.Map2(
                        rules => rules.ConvertAll(rule => MakeRuleBuildingRecipe(pattern, rule, pathesToWords, grammarParser, sentenceUnderstander)),
                        statements => statements.ConvertAll(statement => MakeComplexTermBuildingRecipe(pattern, statement, pathesToWords, grammarParser, sentenceUnderstander))),
                    russianLexicon);
        }

        private Either<FailedUnderstandingAttempt, int> CheckSentenceStructure(ParsedSentence parsedSentence)
        {
            var elements = parsedSentence.SentenceStructure.IterateByPosition().ToArray();

            return 
                 from checkNumberOfElenmentsd in PatternChecks.CheckNumberOfElements(
                                    expectedNumber: SentenceElementsCheckers.Count, 
                                    actualNumber: elements.Length,
                                    NumberMismatchReason.NumberOfSentenceElementsDiffer)
                 from sentenceStructureMatchingLevel in SentenceElementsCheckers.Zip(elements).AggregateWhile(
                                    Right<FailedUnderstandingAttempt, int>(0),
                                    (accumulator, it) =>
                                        from sentenceStructureMatchingLevel in accumulator
                                        from currentElementLevel in PatternChecks.Check(it.First, it.Second, RussianLexicon)
                                        select sentenceStructureMatchingLevel + currentElementLevel,
                                    accumulator => accumulator.IsRight)
                  from checkThatThereAreNoQuotesAtSpecifiedIndexes in WordsUsedInMeaningTwiceOrMore.AggregateWhile(
                                    Right<FailedUnderstandingAttempt, Unit>(Unit.Instance),
                                    (_, g) => PatternChecks.CheckThatThereAreWordsAtIndexes(g, elements),
                                    accumulator => accumulator.IsRight)
                  from sameWordsInPatternMeanSameWordsInUnderstoodSentence in WordsUsedInMeaningTwiceOrMore.AggregateWhile(
                                    Right<FailedUnderstandingAttempt, Unit>(Unit.Instance),
                                    (_, g) => PatternChecks.CheckThatThereIsTheSameWordAtIndexes(g, elements),
                                    accumulator => accumulator.IsRight)
                  select sentenceStructureMatchingLevel;
        }        

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
    }
#pragma warning disable CA1801 // Review unused parameters
    internal sealed record DroppingQuotesUnderstander(
        RegularConcreteUnderstander WrappedUnderstander,
        IReadOnlyCollection<IReadOnlyCollection<WordChecker>> SequencesOfExactWords,
        IRussianGrammarParser GrammarParser,
        IRussianLexicon RussianLexicon)
            : ConcreteUnderstander(WrappedUnderstander.PatternChecks.WithSuffix("-DroppedQuotes"))
#pragma warning restore CA1801
    {
        public override UnderstandingAttemptOutcome Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder) =>
            from unquotedWordsRanges in CheckParsedSentence(sentence)
            from result in WrappedUnderstander.Understand(sentence.IntroduceQuotes(unquotedWordsRanges, GrammarParser), meaningBuilder)
            select result with { UnderstoodSentence = result.UnderstoodSentence with { PatternId = PatternChecks.PatternId } };

        public static MayBe<DroppingQuotesUnderstander> TryCreate(
            RegularConcreteUnderstander wrappedUnderstander,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon)
        {
            var sentenceElementsCheckers = wrappedUnderstander.SentenceElementsCheckers;

            var allowsDroppingQuotes =
                sentenceElementsCheckers.Any() &&
                sentenceElementsCheckers.All(checker => 
                    checker.Map(it => it.ExpectParticularWord).OrElse(true));

            if (!allowsDroppingQuotes)
            {
                return None;
            }
            
            var sequencesOfExactWords = sentenceElementsCheckers
                    .Split(wordChecker => !wordChecker.HasValue, wordChecker => (wordChecker.Value!));

            return sequencesOfExactWords.Any()
                ? Some(new DroppingQuotesUnderstander(wrappedUnderstander, sequencesOfExactWords, grammarParser, russianLexicon))
                : None;
        }

        private bool StartsWithExactWords =>
            WrappedUnderstander.SentenceElementsCheckers.First().HasValue;

        private bool EndsWithExactWords =>
            WrappedUnderstander.SentenceElementsCheckers.Last().HasValue;

        private Either<FailedUnderstandingAttempt, List<Range>> CheckParsedSentence(ParsedSentence parsedSentence)
        {
            var elements = parsedSentence.SentenceStructure.IterateByPosition().ToArray();

            var matchedSequences = new List<Range>();
            var i = Index.FromStart(0);
            foreach (var sequenceOfExactWords in SequencesOfExactWords)
            {
                var matchingSequenceRange = TryFindMatchingSequence(elements, sequenceOfExactWords, i);
                if (matchingSequenceRange == null)
                {
                    break;
                }

                i = Index.FromStart(matchingSequenceRange.Value.End.Value + 1);
                matchedSequences.Add(matchingSequenceRange.Value);
            }

            return from checkNumberOfSequencesofExactWords in PatternChecks.CheckNumberOfElements(
                                                                expectedNumber: SequencesOfExactWords.Count, 
                                                                actualNumber: matchedSequences.Count,
                                                                mismatchReason: NumberMismatchReason.NumberOfSequencesOfExactWordsDiffer)
                   from checkThatSentenceStartsAndFinishesWithOrWithoutExactWords in PatternChecks.
                                                            CheckThatSentenceStartsAndFinishesWithOrWithoutExactWords(
                                                                startsWithExactWords: StartsWithExactWords, 
                                                                endsWithExactWords: EndsWithExactWords, 
                                                                matchedSequences,
                                                                elements)
                   select matchedSequences;
        }

        private Range? TryFindMatchingSequence(
            WordOrQuotation<Word>[] elements,
            IReadOnlyCollection<WordChecker> sequenceOfExactWords,
            Index start)
        =>
            Enumerable
                .Range(start.Value, elements.Length - start.Value - sequenceOfExactWords.Count + 1)
                .TryFirst(i => 
                    Enumerable
                        .Range(0, sequenceOfExactWords.Count)
                        .All(j => PatternChecks.Check(Some(sequenceOfExactWords.ElementAt(j)), elements[i + j], RussianLexicon).IsRight))
                .Map(i => new Range(Index.FromStart(i), Index.FromStart(i + sequenceOfExactWords.Count)))
                .AsNullable();
    }

    internal static class PatternBuilder
    {
        public static IEnumerable<ConcreteUnderstander> BuildUnderstanders(
            string patternId, 
            PatternWithMeaning patternWithMeaning,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            SentenceUnderstander sentenceUnderstander)
        {
            var regularUnderstander = RegularConcreteUnderstander.Create(
                patternId,
                patternWithMeaning,
                grammarParser,
                russianLexicon,
                sentenceUnderstander);

            PatternEstablished?.Invoke(patternId, patternWithMeaning.Pattern, patternWithMeaning.Meaning);
            yield return regularUnderstander;

            var droppingQuotesUnderstander = DroppingQuotesUnderstander.TryCreate(regularUnderstander, grammarParser, russianLexicon);
            if (droppingQuotesUnderstander.HasValue)
            {
                yield return droppingQuotesUnderstander.Value!;
            }
        }

        public static event Action<string, DisambiguatedSentence, SentenceMeaning>? PatternEstablished;

        public static event Action<string, bool?>? PatternRecognitionEvent;

        internal static Exception PatternBuildingException(string message, bool invalidOperation = false) =>
            invalidOperation 
                ? new InvalidOperationException(message) 
                : new NotSupportedException(message);

        internal static RuleBuilingRecipe MakeRuleBuildingRecipe(
            DisambiguatedSentence pattern,
            Rule rule,
            PathesToWords words,
            IRussianGrammarParser grammarParser,
            SentenceUnderstander sentenceUnderstander)
        =>
            new (
                MakeComplexTermBuildingRecipe(pattern, rule.Conclusion, words, grammarParser, sentenceUnderstander),
                rule.Premises.ConvertAll(premise =>
                    MakeComplexTermBuildingRecipe(pattern, premise, words, grammarParser, sentenceUnderstander)));

        internal static ComplexTermBuilingRecipe MakeComplexTermBuildingRecipe(
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

                    return new (Right(new UnderstanderBuilingRecipe(words.LocateWord(quoteTextInPatern))));
                }

                default:
                {
                    var functorRecipe = MakeFunctorBuildingRecipe(complexTerm.Functor, words);
                    var argumentRecipies = complexTerm.Arguments.ConvertAll(
                                        arg => MakeTermBuildingRecipe(pattern, arg, words, grammarParser, sentenceUnderstander));
                    return new (Left(new RegularComplexTermBuilingRecipe(functorRecipe, argumentRecipies)));
                }
            }
        }

        private static FunctorBuildingRecipe MakeFunctorBuildingRecipe(
            FunctorBase functor,
            PathesToWords words)
        =>
            functor switch
            {
                var _ when BuiltinPrologFunctors.Contains(functor.Name) || IsMetaModifier(functor) => Left(functor),
                Functor f => Right((words.LocateWord(f.Name), f.Arity)),
                _ => throw PatternBuildingException(
                        $"Cannot handle functor '{functor.Name}' of type {functor.GetType().Name} in meanining pattern.")
            };

        private static TermBuilingRecipe MakeTermBuildingRecipe(
            DisambiguatedSentence pattern, 
            Term term,
            PathesToWords words,
            IRussianGrammarParser grammarParser,
            SentenceUnderstander sentenceUnderstander)
        =>
            term switch 
            {
                Atom atom => 
                    new AtomBuilingRecipe(words.LocateWord(atom.Characters)),
                Prolog.Engine.Number number => 
                    new NumberBuilingRecipe(words.LocateWord(number.Value.ToString(CultureInfo.CurrentCulture))),
                Variable variable => 
                    new VariableBuilingRecipe(words.LocateWord(variable.Name, capitalizeFirstWord: true)),
                ComplexTerm complexTerm =>
                    MakeComplexTermBuildingRecipe(pattern, complexTerm, words, grammarParser, sentenceUnderstander),
                _ => 
                    throw PatternBuildingException($"Term {term} is not supported.")
            };

        internal static bool LogChecking(bool checkSucceeded = true, string? log = null)
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

    }

    internal record PathesToWords(
        IReadOnlyDictionary<string, IReadOnlyCollection<(int PositionInSentence, MayBe<LemmaVersion> LemmaVersion)>> Pathes,
        string Sentence)
    {
        public NameBuilingRecipe LocateWord(string text, bool capitalizeFirstWord = false) =>
            new (
                text.SplitAtUpperCharacters().Select(LocateSingleWord).AsImmutable(),
                capitalizeFirstWord);

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

    internal sealed record WordChecker(LemmaVersion LemmaVersion, bool ExpectParticularWord)
    {
        public Either<UnderstandingFailureReason, int> Check(Word word, string content, IRussianLexicon russianLexicon) => 
            word.LemmaVersions switch
            {
                var lemmaVersions when lemmaVersions.Any(CheckLemmaVersion) => Right(0),
                var lemmaVersions when lemmaVersions.Any(lv => CheckWordForms(lv, content, russianLexicon)) => Right(1),
                _ => Left(new CheckLemmaVersionsFailed(LemmaVersion, word, content, ExpectParticularWord) as UnderstandingFailureReason)
            };

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
}