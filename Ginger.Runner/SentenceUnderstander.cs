using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;
using Ginger.Runner.Solarix;
using Prolog.Engine;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    using UnderstandingOutcome = Either<IReadOnlyCollection<FailedUnderstandingAttempt>, UnderstoodSentence>;
    
    using static Either;
    using static MakeCompilerHappy;
    using static MayBe;
    using static MonadicParsing;
    using static PrologParser;
    using static TextParsingPrimitives;

    internal sealed record UnderstoodSentence(ParsedSentence Sentence, string PatternId, SentenceMeaning Meaning);

    internal sealed class SentenceUnderstander
    {
        private SentenceUnderstander(
            IReadOnlyCollection<ConcreteUnderstander> phraseTypeUnderstanders,
            IRussianGrammarParser russianGrammarParser)
        {
            _phraseTypeUnderstanders = phraseTypeUnderstanders;
            _grammarParser = russianGrammarParser;
        }

        public UnderstandingOutcome Understand(ParsedSentence sentence) =>
            Understand(sentence, _phraseTypeUnderstanders);

        public static SentenceUnderstander LoadFromEmbeddedResources(
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon) 
        =>
            LoadFromPatterns(
                ParsePatterns(ReadEmbeddedResource("Ginger.Runner.SentenceUnderstandingRules.txt")),
                grammarParser,
                russianLexicon);

        public static SentenceUnderstander LoadFromPatterns(
            IEnumerable<GenerativePattern> generativePatterns,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon) 
        {
            var concreteUnderstanders = new List<ConcreteUnderstander>();
            var result = new SentenceUnderstander(concreteUnderstanders, grammarParser);

            foreach (var generativePattern in generativePatterns)
            {
                var concretePatterns = generativePattern
                                        .GenerateConcretePatterns(grammarParser, russianLexicon)
                                        .AsImmutable();
                
                var ambiguouslyUnderstoodPatterns = (
                    from it in concretePatterns
                    let pattern = it.PatternWithMeaning.Pattern
                    let understanding = result.Understand(pattern.AsParsedSentence(), concreteUnderstanders)
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
                                    russianLexicon,
                                    result)));
            }

            return result;
        }

        private UnderstandingOutcome Understand(
            ParsedSentence sentence, 
            IEnumerable<ConcreteUnderstander> phraseTypeUnderstanders) 
        =>
            phraseTypeUnderstanders
                .Select(phraseTypeUnderstander => phraseTypeUnderstander.Understand(sentence, MeaningBuilder.Create(this, _grammarParser)))
                .AggregateWhile(
                    (Failures: new List<FailedUnderstandingAttempt>(), UnderstandingResult: MakeNone<UnderstandingResult>()),
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

        private readonly IReadOnlyCollection<ConcreteUnderstander> _phraseTypeUnderstanders;
        private readonly IRussianGrammarParser _grammarParser;
    }
}