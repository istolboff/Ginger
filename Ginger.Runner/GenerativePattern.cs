using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;
using System.Text.RegularExpressions;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;

    using static DomainApi;

    internal sealed record GenerativePattern(string PatternId, string PatternText, SentenceMeaning Meaning)
    {
        public IEnumerable<PatternWithMeaning> GenerateConcretePatterns(
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon) 
        =>
            FixedWordAlternativesPattern.Generate(PatternText, Meaning, russianLexicon)
                .SelectMany(it => ReplicatableWordPattern.Generate(it.Pattern, it.Meaning, grammarParser, russianLexicon))
                .Select(it => it with { Meaning = MeaningMetaModifiers.Preprocess(it.Meaning) });

        
        public static event Action<string>? PatternBuildingEvent;

        private static class ReplicatableWordPattern
        {
            public static IEnumerable<PatternWithMeaning> Generate(
                string patternText,
                SentenceMeaning meaning,
                IRussianGrammarParser grammarParser,
                IRussianLexicon russianLexicon) 
            {
                var matchCollection = ReplicatableWordRegex.Matches(patternText);
                if (!matchCollection.Any())
                {
                    return PlainPattern();
                }

                var replicatableWordsIndexes = CalculateIndexesOfReplicatableWordInPattern(
                    AnnotatedGrammar.RemoveAnnotations(DisambiguatedPattern.Create(patternText, russianLexicon)).PlainText);

                var replicatableWordElements = (
                        from matchWithIndex in matchCollection.Select((match, matchIndex) => (match, matchIndex))
                        let matchedGroups = matchWithIndex.match.Groups
                        let word = matchedGroups[1].Value
                        let wordLocation = new SubstringLocation(matchedGroups[1].Index, matchedGroups[1].Length)
                        let lemmaDisambiguator = matchedGroups[2].Success 
                                ? LemmaVersionDisambiguatorDefinition.Create(matchedGroups[2].Value, russianLexicon)
                                : null
                        let generationHint = ParseHint(matchedGroups[3].Value)
                        let generationHintLocation = new SubstringLocation(matchedGroups[3].Index, matchedGroups[3].Length)
                        let generationElement = new GenerationElementLocation(wordLocation, generationHintLocation)
                        select new 
                        { 
                            word, 
                            generationHint, 
                            generationElement, 
                            lemmaDisambiguator, 
                            indexInParsedSentence = replicatableWordsIndexes[matchWithIndex.matchIndex] 
                        }
                    ).ToArray();

                var patternWithRemovedGenerationHints = AdjustText(
                            patternText,
                            replicatableWordElements
                                .Select(ge => new StringAdjustingOperation(ge.generationElement.HintLocation, string.Empty)))
                        .Trim();
                
                var parsedPattern = grammarParser.ParseAnnotated(
                        DisambiguatedPattern.Create(patternWithRemovedGenerationHints, russianLexicon))
                    .Sentence;

                if (!parsedPattern.IsLeft) 
                {   
                    // it's a quote, hence it can't be a generative pattern
                    return PlainPattern();
                }

                PatternBuildingEvent?.Invoke(new 
                    { 
                        ParsedPattern = string.Join(", ", parsedPattern.IterateWordsDepthFirst().Select(w => $"{w.Content}({w.PositionInSentence})"))
                    }.ToString()!);
                    
                if (replicatableWordElements
                        .Where(ge => ge.generationHint == GenerationHint.Replicatable)
                        .Any(ge => ge.lemmaDisambiguator != null))
                {
                    // you can't do something like this: очки его (вин.,ср.) {и проч.} красят
                    throw Failure(
                        "Lemma version disambiguators for replicatable words are not supported. Please reformulate the pattern.");
                }

                var replicatableWords = (
                            from ge in replicatableWordElements
                            where ge.generationHint == GenerationHint.Replicatable
                            let wordElement = parsedPattern.LocateWord(ge.indexInParsedSentence)
                            select new ReplicatableWord(
                                ge.generationElement,
                                GetSingleLemmaVersion(wordElement, patternWithRemovedGenerationHints, ge.generationHint))
                        ).AsImmutable();

                var pluralitySensitiveElements = (
                        from ge in replicatableWordElements
                        where ge.generationHint == GenerationHint.PluralitySensitive
                        let wordElement = parsedPattern.LocateWord(ge.indexInParsedSentence)
                        select new PluralitySensitiveElement(
                            ge.generationElement,
                            EnsureMasculine(
                                GetSingleLemmaVersion(
                                    wordElement, 
                                    patternWithRemovedGenerationHints, 
                                    ge.generationHint,
                                    ignoreGender: true),
                                ge.word),
                            ge.lemmaDisambiguator)
                                
                        ).AsImmutable();

                var rawResult = replicatableWords.Count switch
                {
                    // only {мн.} elements, no {и проч.}
                    0 => new PatternWithMeaning[]
                        {
                            new (patternWithRemovedGenerationHints, meaning),
                            new (
                                AdjustText(
                                    patternText,
                                    pluralitySensitiveElements.Select(it => it.BuildPatternModifier(russianLexicon))),
                                meaning)
                        },

                    // single {и проч.} element and {мн.} elements
                    1 => Enumerable
                                .Range(1, MaximumNumberOfElementsInEnumerations)
                                .Select(elementsNumber => 
                                {
                                    var replicatableWord = replicatableWords.Single();
                                    var concretePatternText = elementsNumber == 1 
                                        ? patternWithRemovedGenerationHints
                                        : AdjustText(
                                            patternText,
                                            new[] { replicatableWord.BuildPatternModifier(elementsNumber, russianLexicon) }
                                                .Concat(pluralitySensitiveElements.Select(it => it.BuildPatternModifier(russianLexicon))));

                                    var concretePatternMeaning = meaning.Fold2(
                                        rules => 
                                            Enumerable
                                                .Range(1, elementsNumber)
                                                .SelectMany(i => i == 1 
                                                            ? rules 
                                                            : replicatableWord.AdjustMeaning(rules, i))
                                                .AsImmutable(),
                                        statements => 
                                            Enumerable
                                                .Range(1, elementsNumber)
                                                .SelectMany(i => i == 1 
                                                            ? statements
                                                            : replicatableWord.AdjustMeaning(statements, i))
                                                .AsImmutable());

                                    return new PatternWithMeaning(concretePatternText, concretePatternMeaning);
                                }),
                    _ => throw Failure(
                        $"Only single replicatable word marked with {ReplicatableHintText} is supported. " + 
                        $"In pattern '{patternText}' there are {replicatableWords.Count}.")
                    };

                return rawResult.Select(JoinMultipleRulesIntoSingleRuleIfConclusionIsTheSameForAllRules);
                
                PatternWithMeaning[] PlainPattern() =>
                    new[] { new PatternWithMeaning(patternText, meaning) };

                GenerationHint ParseHint(string hintText) =>
                    hintText switch
                    {
                        ReplicatableHintText => GenerationHint.Replicatable,
                        PluralSensitivityHintText => GenerationHint.PluralitySensitive,
                        _ => throw Failure($"Unknown generation hint: '{hintText}'")
                    };

                LemmaVersion GetSingleLemmaVersion(
                    Word wordElement,
                    string phrase,
                    GenerationHint hintType,
                    bool ignoreGender = false) 
                =>
                    (!ignoreGender || wordElement.LemmaVersions.Count == 1
                        ? wordElement.LemmaVersions
                        : wordElement.LemmaVersions
                            .Where(lv => (lv.Characteristics.TryGetGender() ?? Gender.Мужской) == Gender.Мужской))
                    .Single(
                        _ => true,
                        _ => Failure(
                                $"{HintToString(hintType)} '{wordElement.Content}' should have a single lemma version in the phrase '{phrase}'. " +
                                "Please reformulate the pattern's text."));

                string HintToString(GenerationHint hintType) =>
                    hintType switch
                    {
                        GenerationHint.Replicatable => "replicatable word",
                        GenerationHint.PluralitySensitive => "plurality sensitive word",
                        _ => throw Failure($"Unsupported hint type {hintType}")
                    };

                LemmaVersion EnsureMasculine(LemmaVersion lemmaVersion, string word) =>
                    (lemmaVersion.Characteristics.TryGetGender() ?? Gender.Мужской) == Gender.Мужской
                        ? lemmaVersion
                        : throw Failure(
                            $"'{word}{PluralSensitivityHintText}': Слова, переходящие во множественное число при репликации, " +
                            "всегда должны иметь мужской род");

                int[] CalculateIndexesOfReplicatableWordInPattern(string patternText)
                {
                    var replicatableWordSites = ReplicatableWordRegex.Matches(patternText);

                    var hintRemovers = replicatableWordSites.ConvertAll(
                        site => 
                            new StringAdjustingOperation(
                                new SubstringLocation(site.Index, site.Length), 
                                site.Groups[1].Value));

                    var patternWithRemovedHints = AdjustText(patternText, hintRemovers);
                    var patternTokens = grammarParser.Tokenize(patternWithRemovedHints);
                    var result = 
                           (from n in Enumerable.Range(0, hintRemovers.Count)
                            let nFirstHintsRemoved = AdjustText(patternText, hintRemovers.Take(n))
                            let tokens = grammarParser.Tokenize(nFirstHintsRemoved)
                            select IndexOfFirstDifference(patternTokens, tokens)
                                    .Map(i => i > 0 ? i - 1 : throw ProgramLogic.Error(
                                        "Impossible situation - pattern with all hints removed differs from the " + 
                                        $"pattern where only first {n} hints were removed in the very first token."))
                                    .OrElse(() => throw ProgramLogic.Error(
                                        $"Could not find the index of the word '{hintRemovers.ElementAt(n).NewContent}' " + 
                                        "in sentence that is marked as replicatable.")))
                            .ToArray();

                    PatternBuildingEvent?.Invoke(new 
                        { 
                            patternText,
                            PatternTokens = string.Join(", ", patternTokens.Select((w, i) => $"{w}({i})")),
                            IndexesOfReplicatableWordsInPattern = 
                                string.Join(
                                    ", ", 
                                    hintRemovers.Zip(result, (hr, i) => $"{hr.NewContent}({i})"))
                        }.ToString()!);

                    return result;
                }

                static MayBe<int> IndexOfFirstDifference(IEnumerable<string> first, IEnumerable<string> second) =>
                    first.Zip(second)
                        .Concat(new[] { (First: "-", Second: "+") }) // taking care of situation when lengths of first and second are diffreent
                        .Select((p, i) => (p.First, p.Second, i))
                        .TryFirst(it => it.First != it.Second)
                        .Map(it => it.i);

                static PatternWithMeaning JoinMultipleRulesIntoSingleRuleIfConclusionIsTheSameForAllRules(
                    PatternWithMeaning patternWithMeaning) 
                =>
                    patternWithMeaning with 
                    { 
                        Meaning = patternWithMeaning.Meaning.MapLeft(
                            rules => rules.Count > 1 && rules.Select(r => r.Conclusion).Distinct().Count() == 1
                                ? new[] 
                                    { 
                                        Rule(
                                            rules.First().Conclusion, 
                                            from premisIndex in Enumerable.Range(0, rules.First().Premises.Count)
                                            from differentPremisesAtIndex in rules.Select(r => r.Premises.ElementAt(premisIndex)).Distinct()
                                            select differentPremisesAtIndex)
                                    }
                                : rules)
                    };
            }

            private static string AdjustText(string text, IEnumerable<StringAdjustingOperation> operations) =>
                operations
                    .OrderByDescending(operation => operation.Location.Start)
                    .Aggregate(text, (s, ao) => ao.ApplyTo(s));

            private static readonly Regex ReplicatableWordRegex = new (@"([а-яА-Я]+)\s*(\([^\)]+\))?\s*({[^}]+})", RegexOptions.Compiled);

            private const string ReplicatableHintText = "{и проч.}";
            private const string PluralSensitivityHintText = "{мн.}";

            private const int MaximumNumberOfElementsInEnumerations = 4;
        }

        private static class FixedWordAlternativesPattern
        {
            public static IEnumerable<PatternWithMeaning> Generate(
                string patternText,
                SentenceMeaning meaning,
                IRussianLexicon russianLexicon)
            {
                var matchCollection = FixedWordAlternativesRegex.Matches(patternText);
                if (!matchCollection.Any())
                {
                    return new[] { new PatternWithMeaning(patternText, meaning) };
                }
                
                var fixedWordAlternativesElements = matchCollection
                        .Select(m => 
                            new 
                            {
                                Location = new SubstringLocation(m.Groups[0].Index, m.Groups[0].Length),
                                Alternatives = m.Groups[1].Value.Split(',').Select(s => s.Trim()).AsImmutable()
                            })
                        .Take(2)
                        .ToArray();

                if (fixedWordAlternativesElements.Length == 0)
                {
                    return new[] { new PatternWithMeaning(patternText, meaning) };
                }

                if (fixedWordAlternativesElements.Length > 1)
                {
                    throw Failure("Only single ∥(...,...) generative construction is supported.");
                }

                var alternativeInfo = fixedWordAlternativesElements.Single();

                return alternativeInfo.Alternatives.Select(
                    word => new PatternWithMeaning(
                            Pattern: patternText
                                .Remove(alternativeInfo.Location.Start, alternativeInfo.Location.Length)
                                .Insert(alternativeInfo.Location.Start, word),
                            Meaning: ReplaceWordInMeaning(
                                        meaning, 
                                        wordToBeReplaced: "∥", 
                                        wordToReplaceWith: russianLexicon.GetNeutralForm(word))));
            }

            private static readonly Regex FixedWordAlternativesRegex = new (@"∥\(([^)]+)\)", RegexOptions.Compiled);
        }

        internal record PatternWithMeaning(string Pattern, SentenceMeaning Meaning);

        private static InvalidOperationException Failure(string message) => new (message);

        private static SentenceMeaning ReplaceWordInMeaning(
            SentenceMeaning meaning, 
            string wordToBeReplaced,
            string wordToReplaceWith)
        => 
            meaning.Fold2(
                rules => ReplaceWordInMeaning(
                                rules, 
                                wordToBeReplaced: wordToBeReplaced, 
                                wordToReplaceWith: wordToReplaceWith),
                statements => ReplaceWordInMeaning(
                                statements, 
                                wordToBeReplaced: wordToBeReplaced, 
                                wordToReplaceWith: wordToReplaceWith));

        private static IReadOnlyCollection<Rule> ReplaceWordInMeaning(
            IReadOnlyCollection<Rule> meaning,
            string wordToBeReplaced,
            string wordToReplaceWith)
        {
            return meaning.Select(AdjustRule).AsImmutable();

            Rule AdjustRule(Rule rule) =>
                Rule(
                    ReplaceWordInMeaning(
                        new[] { rule.Conclusion }, 
                        wordToBeReplaced: wordToBeReplaced, 
                        wordToReplaceWith: wordToReplaceWith)
                    .Single(), 
                    ReplaceWordInMeaning(
                        rule.Premises, 
                        wordToBeReplaced: wordToBeReplaced, 
                        wordToReplaceWith: wordToReplaceWith));
        }

        private static IReadOnlyCollection<ComplexTerm> ReplaceWordInMeaning(
            IReadOnlyCollection<ComplexTerm> meaning,
            string wordToBeReplaced,
            string wordToReplaceWith)
        {
            return meaning.Select(AdjustComplexTerm).AsImmutable();

            ComplexTerm AdjustComplexTerm(ComplexTerm complexTerm) =>
                complexTerm with { Arguments = new (complexTerm.Arguments.Select(AdjustTerm)) };

            Term AdjustTerm(Term term) =>
                term switch
                {
                    Atom atom when atom.Characters.Equals(
                        wordToBeReplaced,
                        StringComparison.OrdinalIgnoreCase) => Atom(wordToReplaceWith),
                    ComplexTerm ct => AdjustComplexTerm(ct),
                    _ => term
                };
        }

        private record ReplicatableWord(GenerationElementLocation GenerationElement, LemmaVersion WordLemma)
        {
            public StringAdjustingOperation BuildPatternModifier(int numberOfReplicas, IRussianLexicon russianLexicon) =>
                new (
                    new SubstringLocation(
                        GenerationElement.WordLocation.End, 
                        GenerationElement.HintLocation.End - GenerationElement.WordLocation.End),
                    numberOfReplicas switch
                    {
                        2 => $" и {GetElement(2, russianLexicon)}",
                        _ => ", " + string.Join(", ", GetElements(numberOfReplicas - 1, russianLexicon)) + 
                             $" и {GetElement(numberOfReplicas, russianLexicon)}"
                    });

            public IReadOnlyCollection<Rule> AdjustMeaning(IReadOnlyCollection<Rule> singleElementMeaning, int nthReplication) =>
                ReplaceWordInMeaning(
                    singleElementMeaning, 
                    wordToBeReplaced: WordLemma.Lemma, 
                    wordToReplaceWith: GetElement(nthReplication));

            public IReadOnlyCollection<ComplexTerm> AdjustMeaning(
                IReadOnlyCollection<ComplexTerm> singleElementMeaning, 
                int nthReplication)
            => 
                ReplaceWordInMeaning(
                    singleElementMeaning, 
                    wordToBeReplaced: WordLemma.Lemma, 
                    wordToReplaceWith: GetElement(nthReplication));

            private IEnumerable<string> GetElements(int n, IRussianLexicon russianLexicon) =>
                Enumerable.Range(2, n - 1).Select(i => GetElement(i, russianLexicon));

            private string GetElement(int i, IRussianLexicon russianLexicon) =>
                russianLexicon.GenerateWordForm(GetElement(i), WordLemma.PartOfSpeech, WordLemma.Characteristics);

            private string GetElement(int i) =>
                WordsForReplication.TryGetValue(WordLemma.PartOfSpeech ?? PartOfSpeech.Существительное, out var words)
                    ? (i - 2 < words.Length 
                        ? words[i - 2] 
                        : throw ProgramLogic.Error($"WordsForReplication[{WordLemma.PartOfSpeech}] does not have enough alternatives"))
                    : throw ProgramLogic.Error(
                        $"Do not have words of {WordLemma.PartOfSpeech} to generate enumerations in patterns.");

            private static readonly IReadOnlyDictionary<PartOfSpeech, string[]> WordsForReplication = 
                new Dictionary<PartOfSpeech, string[]>
                {
                    [PartOfSpeech.Существительное] = new[] { "летчик", "наводчик", "поэт" },
                    [PartOfSpeech.Прилагательное] = new[] { "красный", "длинный", "теплый" }
                };
        }

        private record PluralitySensitiveElement(
            GenerationElementLocation GenerationElement, 
            LemmaVersion WordLemma,
            LemmaVersionDisambiguatorDefinition? DisambiguatorDefinition)
        {
            public StringAdjustingOperation BuildPatternModifier(IRussianLexicon russianLexicon)
            {
                var adjustingOperation = GenerationElement.MakeAdjustingOperation(
                            russianLexicon.GetPluralForm(WordLemma), 
                            GenerationElementPart.WholeSpot);
                return DisambiguatorDefinition == null
                            ? adjustingOperation
                            : adjustingOperation.InsertAtEnd(DisambiguatorDefinition.Remove(typeof(Gender)).Definition);
            }
        }

        private record GenerationElementLocation(SubstringLocation WordLocation, SubstringLocation HintLocation)
        {
            public StringAdjustingOperation MakeAdjustingOperation(string newContent, GenerationElementPart elementPart) =>
                new (
                    elementPart switch 
                    {
                        GenerationElementPart.WholeSpot => WordLocation with { Length = HintLocation.End - WordLocation.Start },
                        GenerationElementPart.GenerationHint => HintLocation,
                        _ => throw ProgramLogic.Error($"Unsupported GenerationElementPart {elementPart}")
                    },
                    newContent);
                
        }

        private record SubstringLocation(int Start, int Length)
        {
            public int End => Start + Length;
        }

        private record StringAdjustingOperation(SubstringLocation Location, string NewContent)
        {
            public StringAdjustingOperation InsertAtEnd(string extraContent) =>
                this with { NewContent = NewContent + extraContent };

            public string ApplyTo(string text) =>
                text
                    .Remove(Location.Start, Location.Length)
                    .Insert(Location.Start, NewContent);
        }

        private enum GenerationHint
        {
            Replicatable,
            PluralitySensitive
        }
 
        private enum GenerationElementPart
        {
            WholeSpot,
            GenerationHint
        }
   }
}