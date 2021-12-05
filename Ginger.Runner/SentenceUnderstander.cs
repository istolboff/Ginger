using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;
using Ginger.Runner.Solarix;
using Prolog.Engine;
using System.Reflection;

namespace Ginger.Runner
{
    using MeaningWithRecipe = Either<IReadOnlyCollection<RuleWithRecipe>, IReadOnlyCollection<ComplexTermWithRecipe>>;
    using UnderstandingOutcome = Either<IReadOnlyCollection<FailedUnderstandingAttempt>, UnderstoodSentence>;
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    
    using static Either;
    using static MayBe;
    using static TextManipulation;

    internal readonly record struct RuleWithRecipe(Rule Rule, RuleBuildingRecipe Recipe);

    internal readonly record struct ComplexTermWithRecipe(ComplexTerm ComplexTerm, MayBe<ComplexTermBuildingRecipe> Recipe);

    internal readonly record struct UnderstoodSentence(
        ParsedSentence Sentence, 
        string PatternId,
        MeaningWithRecipe MeaningWithRecipe);

    internal sealed class SentenceUnderstander
    {
        private SentenceUnderstander(IReadOnlyCollection<ConcretePatternOfUnderstanding> phraseTypeUnderstanders)
        {
            _phraseTypeUnderstanders = phraseTypeUnderstanders;
        }

        public UnderstandingOutcome Understand(ParsedSentence sentence, MeaningBuilder meaningBuilder) =>
            Understand(sentence, _phraseTypeUnderstanders, meaningBuilder);

        public static SentenceUnderstander LoadFromEmbeddedResources(
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            MeaningBuilder meaningBuilder) 
        =>
            LoadFromPatterns(
                    TextInput.ReadFromEmbeddedResource(
                        "Ginger.Runner.SentenceUnderstandingRules.txt", 
                        Assembly.GetExecutingAssembly()),
                grammarParser,
                russianLexicon,
                meaningBuilder);

        public static SentenceUnderstander LoadFromPatterns(
            TextInput patternsText,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            MeaningBuilder meaningBuilder) 
        {
            var concreteUnderstanders = new List<ConcretePatternOfUnderstanding>();
            var result = new SentenceUnderstander(concreteUnderstanders);
            var generativePatterns = 
                GenerativePatternParser
                    .ParsePatterns(patternsText)
                    .Select(parsedPattern =>
                        new GenerativePattern(
                            parsedPattern.PatternId,
                            parsedPattern.PatternText,
                            Meaning: parsedPattern.Meaning.Fold(
                                meaning => 
                                    meaning,
                                intermediateMeaning => 
                                    UnderstandIntermediateMeaning(
                                        intermediateMeaning, 
                                        parsedPattern.PatternId)),
                            BuiltIndirectly: parsedPattern.Meaning.IsRight));

            foreach (var generativePattern in /* essentially lazy */generativePatterns)
            {
                var concretePatterns = generativePattern
                                        .GenerateConcretePatterns(grammarParser, russianLexicon)
                                        .AsImmutable();
                
                var ambiguouslyUnderstoodPatterns = (
                    from it in concretePatterns
                    let pattern = it.PatternWithMeaning.Pattern
                    let understanding = Understand(
                                            pattern.AsParsedSentence(), 
                                            concreteUnderstanders,
                                            meaningBuilder)
                    where understanding.IsRight
                    select new
                    { 
                        Pattern = pattern, 
                        NewPatternId = generativePattern.PatternId, 
                        ExistingPatternId = understanding.Right!.PatternId 
                    }
                ).AsImmutable();

                if (ambiguouslyUnderstoodPatterns.Any())
                {
                    throw new InvalidOperationException(
                        "The following understanding pattern(s) introduce ambiguity: " + Environment.NewLine +
                        string.Join(
                            Environment.NewLine, 
                            ambiguouslyUnderstoodPatterns.Select(
                                aup => $"   text '{aup.Pattern.Sentence}' from pattern '{aup.NewPatternId}' " + 
                                       $"was recognized by the pattern '{aup.ExistingPatternId}'")));
                }

                concreteUnderstanders.AddRange(concretePatterns
                    .SelectMany(concretePattern => PatternBuilder.BuildUnderstanders(
                                    concretePattern.PatternId,
                                    concretePattern.PatternWithMeaning,
                                    grammarParser,
                                    russianLexicon)));
            }

            return result;

            SentenceMeaning UnderstandIntermediateMeaning(
                IntermediateMeaningSentence intermediateMeaning,
                string patternId)
            =>
                result
                    .Understand(
                        intermediateMeaning.ToParsedSentence(grammarParser), 
                        intermediateMeaning.Enrich(meaningBuilder))
                    .Fold(
                        failedAttempts => throw new InvalidOperationException(
                                $"Failed to understand meaning '{intermediateMeaning}' in " +
                                $"pattern {patternId}" +
                                Environment.NewLine +
                                Print(failedAttempts)),
                        understoodSentence =>
                                understoodSentence.MeaningWithRecipe.Map2(
                                    rules => rules.ConvertAll(r => r.Rule),
                                    statements => statements.ConvertAll(ct => ct.ComplexTerm)));
        }

        private static UnderstandingOutcome Understand(
            ParsedSentence sentence, 
            IEnumerable<ConcretePatternOfUnderstanding> phraseTypeUnderstanders,
            MeaningBuilder meaningBuilder) 
        =>
            phraseTypeUnderstanders
                .Select(phraseTypeUnderstander => phraseTypeUnderstander.Understand(sentence, meaningBuilder))
                .AggregateWhile(
                    (Failures: new List<FailedUnderstandingAttempt>(), UnderstandingResult: MakeNone<SuccessfulUnderstanding>()),
                    (state, understandingAttemptOutcome) =>
                        understandingAttemptOutcome switch
                        {
                            (_, var understandingResult, false) =>
                                (
                                    state.Failures,
                                    state.UnderstandingResult.Map(v => v.UnderstandingConfidenceLevel).OrElse(int.MaxValue) > understandingResult!.UnderstandingConfidenceLevel
                                        ? Some(understandingResult)
                                        : state.UnderstandingResult
                                ),
                            (var failedUnderstandingAttempt, _, true) => 
                                (state.Failures.AddAndReturnSelf(failedUnderstandingAttempt!), state.UnderstandingResult)
                        },
                    state => state.UnderstandingResult.Map(v => v.UnderstandingConfidenceLevel).OrElse(int.MaxValue) != 0)
                switch
                {
                    (var failedAttempts, (_, false)) => Left(failedAttempts as IReadOnlyCollection<FailedUnderstandingAttempt>),
                    (_, (var result, true)) => Right(result!.UnderstoodSentence)
                };

        private readonly IReadOnlyCollection<ConcretePatternOfUnderstanding> _phraseTypeUnderstanders;
    }
}