using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using MeaningWithRecipe = Either<IReadOnlyCollection<RuleWithRecipe>, IReadOnlyCollection<ComplexTermWithRecipe>>;
    
    using static Either;
    using static MakeCompilerHappy;
    using static MayBe;
    using static MonadicParsing;
    using static PrologParser;
    using static TextParsingPrimitives;

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
                ParsePatterns(ReadEmbeddedResource("Ginger.Runner.SentenceUnderstandingRules.txt")),
                grammarParser,
                russianLexicon,
                meaningBuilder);

        public static SentenceUnderstander LoadFromPatterns(
            IEnumerable<GenerativePattern> generativePatterns,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon,
            MeaningBuilder meaningBuilder) 
        {
            var concreteUnderstanders = new List<ConcretePatternOfUnderstanding>();
            var result = new SentenceUnderstander(concreteUnderstanders);

            foreach (var generativePattern in generativePatterns)
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
                    (var failedAttempts, (_, false)) => 
                        Left(
                            new UnderstandingFailure(
                                Left(sentence), 
                                failedAttempts as IReadOnlyCollection<FailedUnderstandingAttempt>)),
                    (_, (var result, true)) => 
                        Right(result!.UnderstoodSentence)
                };

        private static TextInput ReadEmbeddedResource(string name)
        {
            var stream = Assembly
                            .GetExecutingAssembly()
                            .GetManifestResourceStream(name);
            using var reader = new StreamReader(SuppressCa1062(stream));
            return new (reader.ReadToEnd());
        }

        private static IEnumerable<GenerativePattern> ParsePatterns(TextInput rulesText)
        {
            var patternId = Tracer.Trace(
                from unused in Lexem("pattern-")
                from id in Repeat(Expect(ch => char.IsLetterOrDigit(ch) || ch == '_' || ch == '-'))
                from unused1 in Lexem(":").Then(Eol)
                select string.Join(string.Empty, id),
                "patternId");

            var meaning = Tracer.Trace(
                Either(PrologParsers.ProgramParser, PrologParsers.PremisesGroupParser),
                "meaning");

            var singlePattern = Tracer.Trace(
                from id in patternId
                from generativePattern in ReadTill("::=")
                from generativeMeaning in meaning
                select new GenerativePattern(id, generativePattern, generativeMeaning),
                "singlePattern");

            var patterns = Tracer.Trace(Repeat(singlePattern), "patterns");

            return patterns(rulesText).Fold(
                        parsingError => throw ParsingError($"{parsingError.Text} at {parsingError.Location.Position}"),
                        result => result.Value);
        }

        private readonly IReadOnlyCollection<ConcretePatternOfUnderstanding> _phraseTypeUnderstanders;
    }
}