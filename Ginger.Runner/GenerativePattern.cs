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
        public IEnumerable<(PatternWithMeaning PatternWithMeaning, string PatternId)> GenerateConcretePatterns(
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon) 
        =>
            FixedWordAlternativesPattern.Generate(PatternText, Meaning, russianLexicon)
                .SelectMany(it => ReplicatableWordPattern.Generate(
                                    grammarParser.ParseAnnotatedPreservingQuotes(it.Pattern, russianLexicon), 
                                    it.Meaning, 
                                    grammarParser, 
                                    russianLexicon)
                                  .Select((patternWithMeaning, numberOfReplicas) => 
                                            new 
                                            { 
                                                patternWithMeaning, 
                                                it.IdSuffix, 
                                                numberOfReplicas
                                            }))
                .Select(it => 
                    (
                        PatternWithMeaning: it.patternWithMeaning, 
                        PatternId: PatternId + 
                                    it.IdSuffix + 
                                    (it.numberOfReplicas > 0 ? $"-{it.numberOfReplicas}" : string.Empty)
                    ));

        private static class ReplicatableWordPattern
        {
            public static IEnumerable<PatternWithMeaning> Generate(
                AnnotatedSentence pattern,
                SentenceMeaning meaning,
                IRussianGrammarParser grammarParser,
                IRussianLexicon russianLexicon) 
            {
                var plainPattern = PlainPattern().ToImmutable();

                if (
                    !IsReplicatable(pattern) ||
                    pattern.SentenceStructure.IsQuote // a quote can't be a generative pattern
                    )
                {
                    return plainPattern;
                }

                var replicatableWord = pattern.SentenceStructure
                    .IterateWordsDepthFirst()
                    .TrySingle(
                        w => w.Annotations.GenerationHint == GenerationHint.Replicatable,
                        replicatableWords => throw Failure(
                            $"Only single replicatable word marked with {NaturalLanguageSentenceMarkup.ReplicatableHintText} is supported. " + 
                            $"In pattern '{pattern.Sentence}' there are {replicatableWords.Count}."));

                return plainPattern
                        .Concat(
                            replicatableWord
                                    .Map(word => 
                                        Enumerable
                                            .Range(2, MaximumNumberOfElementsInEnumerations - 1)
                                            .Select(numberOfReplicas => 
                                            {
                                                var concretePattern = ReplicateWordInSentence(pattern, numberOfReplicas);
                                                var concretePatternMeaning = meaning
                                                    .Map2(
                                                        rules => 
                                                            Enumerable
                                                                .Range(1, numberOfReplicas)
                                                                .SelectMany(i => i == 1 
                                                                            ? rules
                                                                            : AdjustRulesMeaning(word, rules, i, russianLexicon))
                                                                .AsImmutable(),
                                                        statements => 
                                                            Enumerable
                                                                .Range(1, numberOfReplicas)
                                                                .SelectMany(i => i == 1 
                                                                            ? statements
                                                                            : AdjustStatementsMeaning(word, statements, i, russianLexicon))
                                                                .AsImmutable());

                                                return new PatternWithMeaning(
                                                            concretePattern.Disambiguate(russianLexicon, enforceLemmaVersions: true), 
                                                            concretePatternMeaning);
                                            }))
                                    .OrElse(() => 
                                            new PatternWithMeaning(
                                                pattern
                                                    .Transform(
                                                        word => word.Annotations.GenerationHint.Map(_ => MakePlural(word)).OrElse(word.ToString()), 
                                                        grammarParser,
                                                        russianLexicon)
                                                    .Disambiguate(russianLexicon, enforceLemmaVersions: true),
                                                meaning
                                            )
                                            .ToImmutable()))
                        .Select(patternWithMeaning => patternWithMeaning with 
                                {
                                    Meaning = patternWithMeaning.Meaning.Map2(
                                        JoinMultipleRulesIntoSingleRuleIfConclusionIsTheSameForAllRules,
                                        RemoveDuplicatedComplexTerms)
                                });

                PatternWithMeaning PlainPattern() =>
                    new (pattern.Disambiguate(russianLexicon), meaning);

                AnnotatedSentence ReplicateWordInSentence(
                    AnnotatedSentence sentence,
                    int numberOfReplicas)
                => 
                    sentence.Transform(word =>
                        !word.Annotations.GenerationHint.HasValue
                            ? word.ToString()
                            : word.Annotations.GenerationHint.Value switch 
                                {
                                    GenerationHint.Replicatable => ListReplicas(word, numberOfReplicas),
                                    GenerationHint.PluralitySensitive => MakePlural(word),
                                    _ => throw ProgramLogic.Error($"Unsupported GenerationHint.{word.Annotations.GenerationHint}.")
                                },
                        grammarParser,
                        russianLexicon);

                string ListReplicas(AnnotatedWord word, int numberOfReplicas) =>
                    numberOfReplicas switch
                        {
                            2 => $"{word.Content} и {GetNthReplicaFor(word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure), 2, russianLexicon)}",
                            _ => word.Content + ", " + 
                                string.Join(
                                    ", ", 
                                    Enumerable
                                        .Range(2, numberOfReplicas - 2)
                                        .Select(i => GetNthReplicaFor(word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure), i, russianLexicon))) + 
                                $" и {GetNthReplicaFor(word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure), numberOfReplicas, russianLexicon)}"
                        };

                string MakePlural(AnnotatedWord word) =>
                    (word with 
                    { 
                        Content = russianLexicon.GetPluralForm(
                                    EnsureMasculine(
                                            word.GetSingleLemmaVersion(
                                                _ => Failure(
                                                        $"'{word.Content}' should have a single lemma version in the phrase '{pattern.Sentence}'. " +
                                                        "Please reformulate the pattern's text."),  
                                                ignoreGender: true),
                                            word.Content)),
                        Annotations = word.Annotations with 
                                        { 
                                            LemmaVersionPicker = word.Annotations.LemmaVersionPicker.Map(
                                                                            picker => picker.ForPluralForm()) 
                                        }
                    }).ToString();

                static IReadOnlyCollection<Rule> JoinMultipleRulesIntoSingleRuleIfConclusionIsTheSameForAllRules(
                    IReadOnlyCollection<Rule> rules) 
                =>
                    rules.Count > 1 && rules.Select(r => r.Conclusion).Distinct().Count() == 1
                        ? new[] 
                            { 
                                Rule(
                                    rules.First().Conclusion, 
                                    from premisIndex in Enumerable.Range(0, rules.First().Premises.Count)
                                    from differentPremisesAtIndex in rules.Select(r => r.Premises.ElementAt(premisIndex)).Distinct()
                                    select differentPremisesAtIndex)
                            }
                        : rules;

                static IReadOnlyCollection<ComplexTerm> RemoveDuplicatedComplexTerms(
                    IReadOnlyCollection<ComplexTerm> statements) 
                => // remove duplicated complex terms, and put repeated ones the righter the more frequent they are
                    statements
                        .Select((statement, index) => new { statement, index })
                        .GroupBy(it => it.statement)
                        .Select(g => new { g.Key, Count = g.Count(), FirstIndex = g.Min(it => it.index) })
                        .GroupBy(it => it.Count)
                        .OrderBy(it => it.Key)
                        .Select(g => new { g.Key, items = g.OrderBy(it => it.FirstIndex) })
                        .SelectMany(it => it.items.Select(it1 => it1.Key))
                        .AsImmutable();
            }

            private static bool IsReplicatable(AnnotatedSentence sentence) =>
                sentence.SentenceStructure.IterateWordsDepthFirst().Any(w => w.Annotations.GenerationHint.HasValue);

            static IReadOnlyCollection<Rule> AdjustRulesMeaning(
                AnnotatedWord word,
                IReadOnlyCollection<Rule> singleElementMeaning, 
                int nthReplication,
                IRussianLexicon russianLexicon) 
            => 
                ReplaceWordInMeaning(
                    singleElementMeaning, 
                    wordToBeReplaced: word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure).Lemma, 
                    wordToReplaceWith: GetNthReplicaFor(word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure), nthReplication));


            static IReadOnlyCollection<ComplexTerm> AdjustStatementsMeaning(
                AnnotatedWord word,
                IReadOnlyCollection<ComplexTerm> singleElementMeaning, 
                int nthReplication,
                IRussianLexicon russianLexicon)
            => 
                ReplaceWordInMeaning(
                    singleElementMeaning, 
                    wordToBeReplaced: word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure).Lemma, 
                    wordToReplaceWith: GetNthReplicaFor(word.GetDisambiguatedLemmaVersion(russianLexicon, enforceLemmaVersions: true, Failure), nthReplication));

            private static LemmaVersion EnsureMasculine(LemmaVersion lemmaVersion, string word) =>
                (lemmaVersion.Characteristics.TryGetGender() ?? Gender.Мужской) == Gender.Мужской
                    ? lemmaVersion
                    : throw Failure(
                        $"'{word}{NaturalLanguageSentenceMarkup.PluralSensitivityHintText}': Слова, переходящие во множественное число при репликации, " +
                        "всегда должны иметь мужской род");

            private static string GetNthReplicaFor(LemmaVersion wordLemma, int i, IRussianLexicon russianLexicon) =>
                russianLexicon.GenerateWordForm(GetNthReplicaFor(wordLemma, i), wordLemma.PartOfSpeech, wordLemma.Characteristics);

            private static string GetNthReplicaFor(LemmaVersion wordLemma, int i) =>
                WordsForReplication.TryGetValue(wordLemma.PartOfSpeech ?? PartOfSpeech.Существительное, out var words)
                    ? (i - 2 < words.Length 
                        ? words[i - 2] 
                        : throw ProgramLogic.Error($"WordsForReplication[{wordLemma.PartOfSpeech}] does not have enough alternatives"))
                    : throw ProgramLogic.Error(
                        $"Do not have words of {wordLemma.PartOfSpeech} to generate enumerations in patterns.");

            private static readonly IReadOnlyDictionary<PartOfSpeech, string[]> WordsForReplication = 
                new Dictionary<PartOfSpeech, string[]>
                {
                    [PartOfSpeech.Существительное] = new[] { "летчик", "наводчик", "поэт" },
                    [PartOfSpeech.Прилагательное] = new[] { "красный", "длинный", "теплый" }
                };

            private const int MaximumNumberOfElementsInEnumerations = 4;
        }

        private static class FixedWordAlternativesPattern
        {
            public static IEnumerable<(string IdSuffix, string Pattern, SentenceMeaning Meaning)> Generate(
                string patternText,
                SentenceMeaning meaning,
                IRussianLexicon russianLexicon)
            {
                var matchCollection = FixedWordAlternativesRegex.Matches(patternText);
                if (!matchCollection.Any())
                {
                    return (string.Empty, patternText, meaning).ToImmutable();
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
                    return (string.Empty, patternText, meaning).ToImmutable();
                }

                if (fixedWordAlternativesElements.Length > 1)
                {
                    throw Failure("Only single ∥(...,...) generative construction is supported.");
                }

                var alternativeInfo = fixedWordAlternativesElements.Single();

                return alternativeInfo.Alternatives.Select(
                    word => (
                            IdSuffix: $"-{word}",
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

        private static InvalidOperationException Failure(string message) => new (message);

        private static SentenceMeaning ReplaceWordInMeaning(
            SentenceMeaning meaning, 
            string wordToBeReplaced,
            string wordToReplaceWith)
        => 
            meaning.Map2(
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
   }
}