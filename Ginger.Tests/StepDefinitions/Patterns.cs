using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TechTalk.SpecFlow;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;
using Ginger.Runner;
using Ginger.Runner.Solarix;

namespace Ginger.Tests.StepDefinitions
{
    using static Either;
    using static MakeCompilerHappy;
    using static MonadicParsing;
    using static TextParsingPrimitives;
    using static TextManipulation;
    using static PrologParser;

    [Binding]
#pragma warning disable CA1812 // Your class is an internal class that is apparently never instantiated on Derived class
    internal sealed class Patterns
#pragma warning restore CA1812
    {
        public Patterns(            
            ScenarioContext scenarioContext,
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon) 
        {
            _scenarioContext = scenarioContext;
            _grammarParser = grammarParser;
            _russianLexicon = russianLexicon;
        }
            
        [Given("the following Patterns")]
        public void DefinePatterns(Table patterns)
        {
            SentenceUnderstander = SentenceUnderstander.LoadFromPatterns(
                from r in patterns.GetMultilineRows()
                select new GenerativePattern(r["Id"], r["Pattern"], ParseMeaning(r["Meaning"])),
                _grammarParser,
                _russianLexicon,
                CreateMeaningBuilder());
        }

        [When("these SUT entities are defined")]
        public void DefineSutEntities(Table entityDefinitions)
        {
            SutDescriptionBuilder = new SutSpecificationBuilder(_grammarParser, SentenceUnderstander);
            foreach (var r in entityDefinitions.GetMultilineRows())
            {
                SutDescriptionBuilder.DefineEntity(r["Entity Definition"]);
            }
        }

        [Then("the following understandings should be possible")]
        public void CheckUnderstandings(Table understandings)
        {
            var situations = understandings.GetMultilineRows()
                .Select(r => new 
                            { 
                                Sentence = r["Sentence"], 
                                ExpectedMeaning = r["Meaning"],
                                RecognizedWithPattern = r.TryFind("Recognized with Pattern")
                            });
            
            var wrongUnderstandings = (
                    from situation in situations
                    let expectedMeaning = ParseMeaning(situation.ExpectedMeaning)
                    let understoodSentence = TryToUnderstand(situation.Sentence)
                    where !understoodSentence
                            .Fold(
                                _ => false,
                                r => MeaningsAreEqual(expectedMeaning, GetMeaning(r)) && 
                                      situation.RecognizedWithPattern.Map(patternId => patternId == r.PatternId).OrElse(true))
                    select new 
                    { 
                        situation.Sentence,
                        ExpectedMeaning = Print(expectedMeaning),
                        ActualMeaning = PrintUnderstoodSentence(understoodSentence),
                        ExpectedPatternId = situation.RecognizedWithPattern,
                        ActualPatternId = understoodSentence.Fold(_ => "n/a", r => r.PatternId)
                    }
                ).AsImmutable();

            Assert.IsFalse(
                wrongUnderstandings.Any(),
                "The following sentences were processed incorrectly:" + Environment.NewLine + 
                string.Join(Environment.NewLine, wrongUnderstandings));

            static string PrintUnderstoodSentence(UnderstandingOutcome understoodSentence) =>
                understoodSentence.Fold(
                    understandingFailure => $"Understanding failed:{Environment.NewLine}" + 
                                Print(
                                    understandingFailure.FailedAttempts
                                        .Where(failure => failure.FailureReason is not WrongNumberOfElements),
                                    Environment.NewLine),
                    r => Print(GetMeaning(r)));
        }

        [Then("the following sentences should fail to be understood")]
        public void SentencesShouldFailToBeUnderstood(Table sentences)
        {
            var unexpectedUnderstandings = (
                    from sentence in sentences.GetMultilineRows().Select(r => r["Sentence"])
                    let understanding = TryToUnderstand(sentence)
                    where understanding.IsRight
                    let understoodSentence = understanding.Right!
                    select new
                    {
                        Sentence = sentence,
                        understoodSentence.PatternId,
                        Meaning = Print(GetMeaning(understoodSentence))
                    }
                ).AsImmutable();

            Assert.IsFalse(
                unexpectedUnderstandings.Any(),
                "The following sentences were successfully understood while they shouldn't have:" + Environment.NewLine +
                string.Join(Environment.NewLine, unexpectedUnderstandings));
        }

        [Then("the following concrete patterns should be generated from a given generative patterns")]
        public void CheckApplicationOfGenerativePatterns(Table cases)
        {
            var invalidGenerations = (
                    from singleCase in cases.GetMultilineRows()
                    let generativePatternText = StreamlineText(singleCase["Generative Pattern"])
                    let expectedConcretePatterns = StreamlineText(singleCase["Resulting Concrete Patterns"])
                                                        .Split(';', StringSplitOptions.RemoveEmptyEntries)
                                                        .Select(s => s.Trim())
                    let actualConcretePatterns = MakeGenerativePattern(generativePatternText)
                                                    .GenerateConcretePatterns(_grammarParser, _russianLexicon)
                                                    .Select(it => it.PatternWithMeaning.Pattern.Sentence)
                    where !expectedConcretePatterns.SequenceEqual(actualConcretePatterns, Impl.RussianIgnoreCase)
                    select new 
                    { 
                        GenerativePatternText = generativePatternText + Environment.NewLine, 
                        IncorrectPatterns = Environment.NewLine + string.Join(
                                                                    Environment.NewLine + Environment.NewLine, 
                                                                    expectedConcretePatterns
                                                                        .Zip(actualConcretePatterns)
                                                                        .Where(it => !Impl.RussianIgnoreCase.Equals(it.First, it.Second))
                                                                        .Select(it => $" +  !{it.First}!{Environment.NewLine} -  !{it.Second}!"))
                    }
                ).AsImmutable();

            Assert.IsFalse(
                invalidGenerations.Any(),
                "The following generative patterns produced invalid concrete pattern text " + Environment.NewLine + 
                string.Join(Environment.NewLine, invalidGenerations));

            string StreamlineText(string text) =>
                text.Replace(Environment.NewLine, " ").Trim();
        }

        [Then("Parsing of the following generative patterns should fail")]
        public void CheckExpectedParsingFailures(Table situations)
        {
            var invalidSituations = (
                from situation in situations.GetMultilineRows(singleLines: true)
                let patternText = situation["Pattern"]
                let expectedParsingError = situation["Failure reason"]
                let actualParsingError = GetParsingError(patternText)
                where !actualParsingError.Contains(expectedParsingError, StringComparison.OrdinalIgnoreCase)
                select new { patternText, expectedParsingError, actualParsingError }
                ).AsImmutable();

            Assert.IsFalse(
                invalidSituations.Any(),
                "The parsing of the following generative patterns was supposed to fail with a particural error message, " + 
                "but those patterns were either successfully parsed or the parsing error was not as expected:" + Environment.NewLine +
                string.Join(Environment.NewLine, invalidSituations));

            string GetParsingError(string patternText)
            {
                try
                {
                    SuppressCa1806(
                        SentenceUnderstander.LoadFromPatterns(
                            new GenerativePattern[] { new ("unimportant", patternText, Left(Immutable.Empty<Rule>())) },
                            _grammarParser,
                            _russianLexicon,
                            CreateMeaningBuilder()));
                    return string.Empty;
                }
                catch (InvalidOperationException exception)
                {
                    return exception.Message;
                }
            }
        }

        internal static SentenceMeaning ParseMeaning(string meaning) =>
            Either(
                WholeInput(PrologParsers.ProgramParser), 
                WholeInput(PrologParsers.PremisesGroupParser))
                    (new TextInput(meaning))
            .Fold(
                parsingError => throw new InvalidOperationException(parsingError.Text),
                meaning1 => meaning1.Value);

        internal SentenceUnderstander SentenceUnderstander
        {
            get => _scenarioContext.Get<SentenceUnderstander>();
            private set => _scenarioContext.Set(value);
        }

        private SutSpecificationBuilder? SutDescriptionBuilder
        {
            get => _scenarioContext.TryGetValue<SutSpecificationBuilder>(out var v) ? v : null;
            set => _scenarioContext.Set(value);
        }

        private UnderstandingOutcome TryToUnderstand(string sentence) =>
            SentenceUnderstander.Understand(_grammarParser.ParsePreservingQuotes(sentence), CreateMeaningBuilder());

        private MeaningBuilder CreateMeaningBuilder() =>
            MeaningBuilder.Create(TryToUnderstand);

        private static bool MeaningsAreEqual(SentenceMeaning expectedMeaning, SentenceMeaning actualMeaning) =>
            expectedMeaning.Fold(
                expectedRules => actualMeaning.Fold(expectedRules.SequenceEqual, _ => false),
                expectedStatements => actualMeaning.Fold(_ => false, expectedStatements.SequenceEqual));

        private static GenerativePattern MakeGenerativePattern(string generativePatternText) =>
            new (
                "unimportant", 
                generativePatternText, 
                new SentenceMeaning(Array.Empty<Rule>(), default, IsLeft: true));

        private static SentenceMeaning GetMeaning(UnderstoodSentence understoodSentence) =>
            understoodSentence.MeaningWithRecipe.Map2(
                rules => rules.ConvertAll(r => r.Rule),
                statements => statements.ConvertAll(s => s.ComplexTerm));

        private readonly ScenarioContext _scenarioContext;
        private readonly IRussianGrammarParser _grammarParser;
        private readonly IRussianLexicon _russianLexicon;
    }
}