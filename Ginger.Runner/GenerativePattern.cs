using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    using MeaningBuildingRecipe = Either<IReadOnlyCollection<RuleBuildingRecipe>, IReadOnlyCollection<ComplexTermBuildingRecipe>>;

    using static DomainApi;
    using static MayBe;
    using static TextManipulation;

    internal sealed record GenerativePattern(
        string PatternId, 
        string PatternText, 
        SentenceMeaning Meaning,
        bool BuiltIndirectly)
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
                                    russianLexicon,
                                    BuiltIndirectly)
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


        private static InvalidOperationException Failure(string message) => new (message);

        private static class ReplicatableWordPattern
        {
            public static IEnumerable<PatternWithMeaning> Generate(
                AnnotatedSentence pattern,
                SentenceMeaning meaning,
                IRussianGrammarParser grammarParser,
                IRussianLexicon russianLexicon,
                bool builtIndirectly) 
            {
                var plainPattern = PlainPattern();

                if (
                    !IsReplicatable(pattern) ||
                    pattern.SentenceStructure.IsQuote // a quote can't be a generative pattern
                    )
                {
                    return plainPattern.ToImmutable();
                }

                var replicatableWord = pattern.SentenceStructure
                    .IterateDepthFirst()
                    .TrySingle(
                        w => w.Word.HasValue && w.Word.Value!.Annotations.GenerationHint == GenerationHint.Replicatable,
                        replicatableWords => throw Failure(
                            $"Only single replicatable word marked with {NaturalLanguageSentenceMarkup.ReplicatableHintText} is supported. " + 
                            $"In pattern '{pattern.Sentence}' there are {replicatableWords.Count}."));

                if (!replicatableWord.HasValue)
                {
                    return new[] 
                    { 
                        plainPattern,
                        new PatternWithMeaning(
                                Pattern: pattern.Transform(
                                        word => word.Annotations.GenerationHint
                                                    .Map(_ => MakePlural(word))
                                                    .OrElse(word.ToString()), 
                                        grammarParser,
                                        russianLexicon)
                                    .Disambiguate(russianLexicon, enforceLemmaVersions: true),
                                Meaning: meaning,
                                builtIndirectly) 
                    };
                }

                var meaningAdjustor = new ReplicatedWordMeaningGenerator(
                                            meaning, 
                                            plainPattern.BuildRecipe(grammarParser),
                                            replicatableWord.Value!,
                                            russianLexicon);

                return plainPattern.ToImmutable()
                        .Concat(
                            Enumerable
                                .Range(2, MaximumNumberOfElementsInEnumerations - 1)
                                .Select(numberOfReplicas => 
                                    new PatternWithMeaning(
                                        Pattern: ReplicateWordInSentence(pattern, numberOfReplicas)
                                            .Disambiguate(russianLexicon, enforceLemmaVersions: true), 
                                        Meaning: meaningAdjustor.GenerateMeaning(numberOfReplicas),
                                        builtIndirectly)));

                PatternWithMeaning PlainPattern() =>
                    new (pattern.Disambiguate(russianLexicon), meaning, builtIndirectly);

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
            }

            public static string GetNthReplicaFor(LemmaVersion wordLemma, int i, IRussianLexicon russianLexicon) =>
                russianLexicon.GenerateWordForm(GetNthReplicaFor(wordLemma, i), wordLemma.PartOfSpeech, wordLemma.Characteristics);

            private static bool IsReplicatable(AnnotatedSentence sentence) =>
                sentence.SentenceStructure.IterateWordsDepthFirst().Any(w => w.Annotations.GenerationHint.HasValue);

            private static LemmaVersion EnsureMasculine(LemmaVersion lemmaVersion, string word) =>
                (lemmaVersion.Characteristics.TryGetGender() ?? Gender.Мужской) == Gender.Мужской
                    ? lemmaVersion
                    : throw Failure(
                        $"'{word}{NaturalLanguageSentenceMarkup.PluralSensitivityHintText}': Слова, переходящие во множественное число при репликации, " +
                        "всегда должны иметь мужской род");

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

            private static readonly Regex FixedWordAlternativesRegex = new (@"∥\(([^)]+)\)", RegexOptions.Compiled);
        }

        private sealed class ReplicatedWordMeaningGenerator
        {
            private readonly SentenceMeaning _meaning;
            private readonly IRussianLexicon _russianLexicon;
            private readonly IReadOnlyDictionary<ComplexTerm, SiteAffectedByReplication> _sitesAffectedByReplication;
            private readonly LemmaVersion _replicatableWordLemma;

            public ReplicatedWordMeaningGenerator(
                SentenceMeaning meaning,
                MeaningBuildingRecipe meaningRecipe,
                WordOrQuotation<AnnotatedWord> replicatableWord,
                IRussianLexicon russianLexicon)
            {
                ProgramLogic.Check(replicatableWord.Word.HasValue);

                _meaning = meaning;
                _russianLexicon = russianLexicon;

                _sitesAffectedByReplication = FindSitesAffectedByReplication(
                                                meaningRecipe,
                                                replicatableWord.PositionInSentence,
                                                meaning);

                _replicatableWordLemma = replicatableWord.Word.Value!.GetDisambiguatedLemmaVersion(
                                                russianLexicon, 
                                                enforceLemmaVersions: true, 
                                                Failure);
            }

            public SentenceMeaning GenerateMeaning(int numberOfReplicas)
            {
                return _meaning.Map2(
                    rules => rules
                        .SelectMany(rule => 
                            ReplicateRelevantComplexTerms(rule.Conclusion.ToImmutable(), numberOfReplicas)
                                .Select(conclusion => 
                                    Rule(
                                        conclusion, 
                                        ReplicateRelevantComplexTerms(rule.Premises, numberOfReplicas))))
                        .AsImmutable(),
                    statements => 
                        ReplicateRelevantComplexTerms(statements, numberOfReplicas)
                        .AsImmutable());
            }

            private IEnumerable<ComplexTerm> ReplicateRelevantComplexTerms(
                IEnumerable<ComplexTerm> topLevelComplexTerms, 
                int numberOfReplicas)
            =>
                topLevelComplexTerms
                    .SelectMany(complexTerm =>
                        _sitesAffectedByReplication.TryFind(complexTerm)
                            .Map(affected => Replicate(complexTerm, affected, numberOfReplicas))
                            .OrElse(complexTerm.ToImmutable()));

            private IEnumerable<ComplexTerm> Replicate(
                ComplexTerm topLevelComplexTerm,
                SiteAffectedByReplication affected,
                int numberOfReplicas)
            =>
                affected.Replicate(GenerateReplicatedAtoms(numberOfReplicas)) switch
                {
                    var result when ReferenceEquals(topLevelComplexTerm, affected.Site) => result,
                    var replicas => replicas.Select(replica => 
                                affected.Substitute(topLevelComplexTerm, replica))
                };

            private IEnumerable<Atom> GenerateReplicatedAtoms(int numberOfReplicas)
            {
                return Enumerable
                    .Range(1, numberOfReplicas - 1)
                    .Select(nthReplication => 
                        Atom(
                            ReplicatableWordPattern.GetNthReplicaFor(
                                _replicatableWordLemma, 
                                nthReplication + 1,
                                _russianLexicon)));
            }

            private static IReadOnlyDictionary<ComplexTerm, SiteAffectedByReplication> FindSitesAffectedByReplication(
                MeaningBuildingRecipe meaningRecipe, 
                int replicatableWordPosition,
                SentenceMeaning meaning)
            {
                ProgramLogic.Check(meaning.IsLeft == meaningRecipe.IsLeft);
                
                var topLevelSitesWithRecipies = meaningRecipe.IsLeft
                      ? from pair in meaningRecipe.Left!.ZipStrictly(meaning.Left!)
                        let ruleRecipe = pair.First
                        let rule = pair.Second
                        from topLevel in new[] { (Recipe: ruleRecipe.ConclusionBuildingRecipe, Site: rule.Conclusion) }
                                            .Concat(ruleRecipe.PremiseBuildingRecipies.ZipStrictly(rule.Premises)
                                                        .Select(it => (Recipe: it.First, Site: it.Second)))
                        select topLevel
                      : meaningRecipe.Right!.ZipStrictly(meaning.Right!).Select(it => (Recipe: it.First, Site: it.Second));

                return (from topLevel in topLevelSitesWithRecipies
                        let affectedSite = TryLocateAffectedSite(topLevel.Recipe, topLevel.Site, replicatableWordPosition)
                        where affectedSite.HasValue
                        select new { topLevel.Site, AffectedSite = affectedSite.Value! })
                       .ToDictionary(it => it.Site, it => it.AffectedSite!);
                        
                static MayBe<SiteAffectedByReplication> TryLocateAffectedSite(
                    ComplexTermBuildingRecipe recipe, 
                    ComplexTerm complexTerm, 
                    int wordPosition)
                =>
                  recipe.ConcreteBuilder.Fold(
                        regularRecipe => 
                            (from pair in regularRecipe.ArgumentBuildingRecipies.ZipStrictly(complexTerm.Arguments)
                            select pair switch 
                                    {
                                        (AtomBuildingRecipe atomRecipe, var term) when term is not Prolog.Engine.Atom
                                        =>
                                            throw ProgramLogic.Error(
                                                $"AtomBuildingRecipe {Print(atomRecipe)} should correspond to an Atom in meaning, " + 
                                                $"but it corresponds to {Print(term)}"),

                                        (AtomBuildingRecipe atomRecipe, Atom atom) when
                                            atomRecipe.AtomContentBuilder.NameComponentGetters.Count == 1 &&
                                            atomRecipe.AtomContentBuilder.NameComponentGetters.Single().PositionInSentence == wordPosition
                                        =>
                                            Some(PatternBuilder.LogCheckingT(new SiteAffectedByReplication(complexTerm, atom))),

                                        (ComplexTermBuildingRecipe complexTermRecipe, var term) when term is not Prolog.Engine.ComplexTerm
                                        =>
                                            throw ProgramLogic.Error(
                                                $"ComplexTermBuildingRecipe {Print(complexTermRecipe)} should correspond to a ComplexTerm in meaning, " + 
                                                $"but it corresponds to {Print(term)}"),
                                        (ComplexTermBuildingRecipe complexTermRecipe, ComplexTerm complexTerm1)
                                        =>
                                            TryLocateAffectedSite(complexTermRecipe, complexTerm1, wordPosition),

                                        _ => None
                                    })
                                .TryFirst(affectedSite => affectedSite.HasValue),
                        _ => None
                    );
            }
        }

        private record SiteAffectedByReplication(ComplexTerm Site, Term ReplicatedArgument)
        {
            public IEnumerable<ComplexTerm> Replicate(IEnumerable<Atom> replicatedAtoms) =>
                Site.IsList()
                    ? List(IterableList(Site).Concat(replicatedAtoms).Reverse()).ToImmutable()
                    : Site.ToImmutable().Concat(
                        replicatedAtoms.Select(replica => 
                            Site with 
                            { 
                                Arguments = new (
                                    Site.Arguments.Select(arg =>
                                        ReferenceEquals(arg, ReplicatedArgument) ? replica : arg))
                            }));

            public ComplexTerm Substitute(ComplexTerm inComplexTerm, ComplexTerm withReplica)
            {
                return AdjustComplexTerm(inComplexTerm);

                ComplexTerm AdjustComplexTerm(ComplexTerm complexTerm) =>
                    complexTerm with { Arguments = new (complexTerm.Arguments.Select(AdjustTerm)) };

                Term AdjustTerm(Term term) =>
                    term switch
                    {
                        ComplexTerm ct when ReferenceEquals(ct, Site) => withReplica,
                        ComplexTerm ct => AdjustComplexTerm(ct),
                        _ => term
                    };
            }
        }
    }
}