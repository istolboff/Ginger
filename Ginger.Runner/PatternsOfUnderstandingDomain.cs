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
    using UnderstandingResult = MayBe<(UnderstoodSentence UnderstoodSentence, int UnderstandingConfidenceLevel)>;
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

    internal sealed record PatternWithMeaning(DisambiguatedSentence Pattern, SentenceMeaning Meaning);

    internal abstract record ConcreteUnderstander
    {
        public abstract UnderstandingResult Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder);
    }

    internal sealed record RegularConcreteUnderstander(
        string PatternId,
        IReadOnlyCollection<MayBe<WordChecker>> SentenceElementsCheckers,
        IReadOnlyCollection<IReadOnlyCollection<int>> WordsUsedInMeaningTwiceOrMore,
        MeaningBuildingRecipe MeaningBuildingRecipe,
        IRussianLexicon RussianLexicon)
            : ConcreteUnderstander
    {
        public override UnderstandingResult Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder)
        =>
            from confidenceLevel in CheckSentenceStructure(sentence)
            from meaning in meaningBuilder.BuildMeaning(MeaningBuildingRecipe, sentence)
            select (new UnderstoodSentence(sentence, PatternId, meaning), confidenceLevel);

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
                    patternId,
                    sentenceElementsCheckers,
                    wordsUsedInMeaningTwiceOrMore,
                    meaning.Map2(
                        rules => rules.ConvertAll(rule => MakeRuleBuildingRecipe(pattern, rule, pathesToWords, grammarParser, sentenceUnderstander)),
                        statements => statements.ConvertAll(statement => MakeComplexTermBuildingRecipe(pattern, statement, pathesToWords, grammarParser, sentenceUnderstander))),
                    russianLexicon);
        }

        private int? CheckSentenceStructure(ParsedSentence parsedSentence)
        {
            LogCheckingT(Unit.Instance, $"+++Pattern {PatternId} will be checked against '{parsedSentence.Sentence}'");
            var result = CheckSentenceStructureCore(parsedSentence);
            LogCheckingT(Unit.Instance, $"---Applying pattern {PatternId} to '{parsedSentence.Sentence}' {(result.HasValue ? "suceeded" : "failed")}");
            return result;
        }

        private int? CheckSentenceStructureCore(ParsedSentence parsedSentence)
        {
            var elements = parsedSentence.SentenceStructure.IterateByPosition().ToArray();

            if (!LogChecking(elements.Length == SentenceElementsCheckers.Count, "number of elements"))
            {
                return null;
            }

            var result = SentenceElementsCheckers
                            .Zip(elements)
                            .AggregateWhile(
                                (int?)0,
                                (sentenceStructureMatchingLevel, it) =>
                                    sentenceStructureMatchingLevel + Check(it.First, it.Second, RussianLexicon),
                                sentenceStructureMatchingLevel => sentenceStructureMatchingLevel != null);

            return result != null && 
                    WordsUsedInMeaningTwiceOrMore.All(g => 
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

            static IEnumerable<string> ListWordLemmas(Word word) =>
                word.LemmaVersions.Select(lv => lv.Lemma);
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

    internal sealed record DroppingQuotesUnderstander(
        RegularConcreteUnderstander WrappedUnderstander,
        IReadOnlyCollection<IReadOnlyCollection<WordChecker>> SequencesOfExactWords,
        IRussianGrammarParser GrammarParser,
        IRussianLexicon RussianLexicon)
            : ConcreteUnderstander
    {
        public override UnderstandingResult Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder) =>
            from unquotedWordsRanges in CheckParsedSentence(sentence)
            from result in WrappedUnderstander.Understand(sentence.IntroduceQuotes(unquotedWordsRanges, GrammarParser), meaningBuilder)
            select (result.UnderstoodSentence with { PatternId = PatternId }, result.UnderstandingConfidenceLevel);

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
        
        private string PatternId => 
            $"{WrappedUnderstander.PatternId}-DroppedQuotes";

        private bool StartsWithExactWords =>
            WrappedUnderstander.SentenceElementsCheckers.First().HasValue;

        private bool EndsWithExactWords =>
            WrappedUnderstander.SentenceElementsCheckers.Last().HasValue;

        private MayBe<List<Range>> CheckParsedSentence(ParsedSentence parsedSentence)
        {
            LogCheckingT(Unit.Instance, $"+++Pattern {PatternId} will be checked against '{parsedSentence.Sentence}'");

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

            var result = 
                    LogChecking(
                        matchedSequences.Count == SequencesOfExactWords.Count,
                        $"finding macthes for all {SequencesOfExactWords.Count} sequences of exact words") &&
                    LogChecking(
                        !StartsWithExactWords || matchedSequences.First().Start.Equals(Index.Start),
                        "checking the first match's location") &&
                    LogChecking(
                        !EndsWithExactWords || matchedSequences.Last().End.Equals(new Index(elements.Length)),
                        "checking the last match's location")
                    ? Some(matchedSequences)
                    : None;
            LogCheckingT(Unit.Instance, $"---Applying pattern {PatternId} to '{parsedSentence.Sentence}' {(result.HasValue ? "suceeded" : "failed")}");
            return result;
        }

        private Range? TryFindMatchingSequence(
            WordOrQuotation<Word>[] elements, 
            IReadOnlyCollection<WordChecker> sequenceOfExactWords, 
            Index start) 
        =>
            Enumerable
                .Range(start.Value, elements.Length - start.Value - sequenceOfExactWords.Count + 1)
                .TryFirst(i => Enumerable.Range(0, sequenceOfExactWords.Count).All(
                    j => Check(Some(sequenceOfExactWords.ElementAt(j)), elements[i + j], RussianLexicon) == 0))
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

        internal static int? Check(
            MayBe<WordChecker> wordChecker, 
            WordOrQuotation<Word> actualWord, 
            IRussianLexicon russianLexicon) 
        =>
            wordChecker
                .Map(checker => actualWord.Word.Fold(word => checker.Check(word, actualWord.Content, russianLexicon), () => null))
                .OrElse(() => LogChecking(actualWord.IsQuote, "expected quotation") ? 0 : null);

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
}