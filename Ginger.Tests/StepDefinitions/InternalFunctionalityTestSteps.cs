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
    using static MakeCompilerHappy;
    using static MayBe;
    using static MonadicParsing;
    using static TextParsingPrimitives;
    using static Prolog.Engine.PrettyPrinting;

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
                let actualDisambiguation = (from word in _grammarParser.ParsePreservingQuotes(sentence).SentenceStructure.IterateWordsDepthFirst()
                                            where word.LemmaVersions.Count > 1
                                            let disambiguator = LemmaVersionDisambiguator.Create(word.LemmaVersions)
                                            select disambiguator.ProposeDisambiguations(_russianLexicon)
                                           ).Single()
                where !expectedDisambiguation.SequenceEqual(actualDisambiguation)
                select new 
                { 
                    sentence, 
                    ExpectedDisambiguation = Print(expectedDisambiguation), 
                    ActualDisambiguation = Print(actualDisambiguation) 
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
                                    .ParseAnnotatedPreservingQuotes(annotatedText, _russianLexicon)
                                    .Disambiguate(_russianLexicon)
                                    .SentenceStructure
                let parsedAmbiguousWord = parsedText
                                            .IterateDepthFirst()
                                            .Where(it => !it.IsQuote)
                                            .Single(word => Impl.RussianIgnoreCase.Equals(word.Content, ambiguousWord))
                                            .Word.Value!
                where !expectedLemmaVersion.Check(parsedAmbiguousWord.LemmaVersion)
                select new { annotatedText, expectedLemmaVersion, parsedAmbiguousWord.LemmaVersion }
                ).AsImmutable();

            Assert.IsFalse(
                invalidSituations.Any(),
                "Disambiguation annotations were processed incorrectly in the following cases:" + Environment.NewLine +
                string.Join(Environment.NewLine, invalidSituations));
        }

        [Then("the following text markups should be parsed correctly")]
        public void TestTextMarkupParsing(Table situations)
        {
            var invalidSituations = (
                from situation in situations.GetMultilineRows()
                let text = situation["Text"]
                let actualParsingResult = _grammarParser.ParseMarkup(text)
                let expectedParsingResult = situation["Parsing Results"].Split(Environment.NewLine).ConvertAll(BuildWord)
                where !expectedParsingResult.SequenceEqual(actualParsingResult)
                select new 
                { 
                    Text = text.Replace(Environment.NewLine, " "), 
                    Expected = Print(expectedParsingResult), 
                    Actual = Print(actualParsingResult) 
                })
                .AsImmutable();

            Assert.IsFalse(
                invalidSituations.Any(),
                "Markup was parsed incorrectly in the following cases:" + Environment.NewLine +
                string.Join(Environment.NewLine, invalidSituations));

            static MarkedupWord BuildWord(string wordDescriptor) =>
                wordDescriptor
                    // ��� true PluralitySensitive [���,���]
                    .Split(' ', 4, StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                    .Apply(components => 
                        new MarkedupWord(
                            Content: components[0],
                            IsFixed: bool.Parse(components[1]),
                            GenerationHint: components[2] switch
                                {
                                    "null" => None,
                                    _ => Some((GenerationHint)Enum.Parse(typeof(GenerationHint), components[2]))
                                },
                            DisambiguatingCoordinates: components[3].Trim('[', ']') switch
                                {
                                    "" => None,
                                    var coordinates => Some<IReadOnlyCollection<string>>(
                                                            new StructuralEquatableArray<string>(
                                                                coordinates.Split(
                                                                    ',', 
                                                                    StringSplitOptions.RemoveEmptyEntries | 
                                                                    StringSplitOptions.TrimEntries)))
                                }
                        ));
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

                return expectedLemmaVersion(new TextInput(grammarCharacteristics)).Right.Value;
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