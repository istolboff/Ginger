using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TechTalk.SpecFlow;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;
using Ginger.Runner;
using Ginger.Runner.Solarix;

namespace Ginger.Tests.StepDefinitions
{
    using static MonadicParsing;
    using static TextParsingPrimitives;
    using static Prolog.Tests.VerboseReporting;
    using static MakeCompilerHappy;

    [Binding]
#pragma warning disable CA1812 // Your class is an internal class that is apparently never instantiated on Derived class
    internal sealed class InternalFunctionalityTestSteps
#pragma warning restore CA1812
    {
        public InternalFunctionalityTestSteps(
            IRussianGrammarParser grammarParser,
            IRussianLexicon russianLexicon)
        =>
            (_grammarParser, _russianLexicon) = (grammarParser, russianLexicon);
        

        [Then("All methods where it's important to mention all classes derived from GrammarCharacteristics should do it correctly")]
        public void TestGrammarCharacteristicsHierarchyDependentMethods()
        {
            SuppressCa1806(this);
            var grammarCharacteristicsType = typeof(GrammarCharacteristics);
            var allGrammarCharacteristicsTypes = grammarCharacteristicsType.Assembly
                        .GetTypes()
                        .Where(t => t != grammarCharacteristicsType && t.IsAssignableTo(grammarCharacteristicsType))
                        .AsImmutable();

            var unlistedGrammarCharacteristicTypes = allGrammarCharacteristicsTypes
                                .Except(Impl.GrammarCharacteristicsTypes)
                                .OrderBy(t => t.FullName)
                                .AsImmutable();
            Assert.IsFalse(
                unlistedGrammarCharacteristicTypes.Any(),
                $"Impl.GrammarCharacteristicsTypes does not list the following classes that derive from {grammarCharacteristicsType.FullName}: " + 
                string.Join(", ", unlistedGrammarCharacteristicTypes));

            foreach (var instance in Impl.GrammarCharacteristicsInstances)
            {
                instance.TryGetNumber(); // same as above
                instance.TryGetGender(); // will throw if not cases are checked
            }
        }

        [Then("the following variants should be proposed by the disambiguation API")]
        public void TestLemmaVersionDisambiguations(Table situations)
        {
            var invalidSituations = (
                from situation in situations.Rows
                let sentence = situation["Sentence with ambiguous lemma versions"]
                let expectedDisambiguation = situation["Proposed disambiguation"].Split(";").Select(s => s.Trim()).AsImmutable()
                let actualDisambiguation = (from word in _grammarParser.ParsePreservingQuotes(sentence).SentenceSyntax.IterateWordsDepthFirst()
                                            where word.LemmaVersions.Count > 1
                                            let disambiguator = LemmaVersionDisambiguator.Create(word.LemmaVersions)
                                            select disambiguator.ProposeDisambiguations(_russianLexicon)
                                           ).Single()
                where !expectedDisambiguation.SequenceEqual(actualDisambiguation)
                select new 
                { 
                    sentence, 
                    ExpectedDisambiguation = Dump(expectedDisambiguation), 
                    ActualDisambiguation = Dump(actualDisambiguation) 
                }
            ).AsImmutable();

            Assert.IsFalse(
                invalidSituations.Any(),
                "The following disambiguations were produced incorrectly:" + Environment.NewLine +
                string.Join(Environment.NewLine, invalidSituations));
        }

        [Then("disambiguation annotation should be applied correctly")]
        public void TestDisambiguationAnnotationParsing(Table situations)
        {
            var invalidSituations = (
                from situation in situations.Rows
                let annotatedText = situation["Sentence with disambiguation annotation"]
                let ambiguousWord = situation["Ambiguous word"]
                let expectedLemmaVersion = ExpectedLemmaVersion.Parse(situation["Parsed Grammar Characteristics"])
                let parsedText = _grammarParser
                                    .ParsePreservingQuotes(
                                        DisambiguatedPattern.Create(annotatedText, _russianLexicon)
                                    ).SentenceSyntax
                let parsedAmbiguousWord = parsedText.Left!
                                            .LocateWord(
                                                ambiguousWord,
                                                errorText => new InvalidOperationException(errorText))
                where parsedAmbiguousWord.LemmaVersions.Count != 1 || 
                      !expectedLemmaVersion.Check(parsedAmbiguousWord.LemmaVersions.Single())
                select new { annotatedText, expectedLemmaVersion, parsedAmbiguousWord.LemmaVersions }
                ).AsImmutable();

            Assert.IsFalse(
                invalidSituations.Any(),
                "Disambiguation annotations were processed incorrectly in the following cases:" + Environment.NewLine +
                string.Join(Environment.NewLine, invalidSituations));
        }

        private readonly IRussianGrammarParser _grammarParser;
        private readonly IRussianLexicon _russianLexicon;

        private record ExpectedLemmaVersion(
            Type GrammarCharacterisitcsType, 
            IReadOnlyCollection<CoordinateStateChecker> StateCheckers)
        {
            public bool Check(LemmaVersion lemmaVersion) =>
                GrammarCharacterisitcsType == lemmaVersion.Characteristics.GetType() &&
                StateCheckers.All(checker => checker.Check(lemmaVersion.Characteristics));

            public static ExpectedLemmaVersion Parse(string grammarCharacteristics)
            {
                var identifier = Tracer.Trace(
                                    Lexem(char.IsLetter, char.IsLetter), 
                                    "identifier");

                var stateChecker = Tracer.Trace(
                    from coordinateTypeName in identifier
                    from unused in Lexem(":")
                    from stateName in identifier
                    select new CoordinateStateChecker(FindSolarixType(coordinateTypeName), stateName),
                    "stateChecker");

                var expectedLemmaVersion = Tracer.Trace(
                    from grammarCharacterisitcsTypeName in identifier
                    from unused in Lexem("{")
                    from stateCheckers in Repeat(stateChecker, Lexem(","), atLeastOnce: true)
                    from unused1 in Lexem("}")
                    select new ExpectedLemmaVersion(
                            FindSolarixType(grammarCharacterisitcsTypeName),
                            stateCheckers),
                    "expectedLemmaVersion");

                return expectedLemmaVersion(new TextInput(grammarCharacteristics, 0)).Right!.Value;
            }

            private static Type FindSolarixType(string typeName) => 
                Type.GetType($"Ginger.Runner.Solarix.{typeName}, Ginger.Runner")!;
        }

        private record CoordinateStateChecker(Type CoordinateType, string StateName)
        {
            public bool Check(GrammarCharacteristics characteristics) =>
                StateName == characteristics
                                .GetType()
                                .GetProperties()
                                .Single(p => p.PropertyType.RemoveNullability() == CoordinateType)
                                .GetValue(characteristics)
                                ?.ToString();
        }
    }
}