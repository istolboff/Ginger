using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TechTalk.SpecFlow;
using Prolog.Engine;
using Ginger.Runner;
using Ginger.Runner.Solarix;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Tests.StepDefinitions
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;

    using static Either;
    using static Prolog.Engine.Parsing.PrologParser;
    using static Prolog.Tests.VerboseReporting;
    using static BuisnessRuleChecker;

    [Binding]
#pragma warning disable CA1812 // Your class is an internal class that is apparently never instantiated on Derived class    
    internal sealed class Sut
#pragma warning restore CA1812
    {
        public Sut(
            ScenarioContext scenarioContext,
            IRussianGrammarParser grammarParser,
            Patterns patterns)
        {
            _scenarioContext = scenarioContext;
            _grammarParser = grammarParser;
            _patterns = patterns;
        }

        public SutSpecification SutSpecification
        {
            get => (SutSpecification)_scenarioContext[nameof(SutSpecification)];
            set => _scenarioContext[nameof(SutSpecification)] = value;
        }

        [Given("SUT is described as follows")]
        public void SutIsDescribedAs(Table description)
        {
            SutSpecification = ParseTextDescripton(description);
        }

        [Then(@"the following components of SUT description should be generated")]
        public void ComponentsOfSutDescriptionShouldBeGenerated(Table table)
        {
            var expectedComponentsGroupedByTheirTypes = table
                    .GetMultilineRows()
                    .GroupBy(row => row["Type"])
                    .ToDictionary(
                        g => g.Key,
                        g => g
                              .Select(row => MakeSentenceMeaningComparable(Patterns.ParseMeaning(row["Prolog Rules"])))
                              .AsImmutable());
            
            var actualComponents = expectedComponentsGroupedByTheirTypes
                    .Keys
                    .ToDictionary(
                        componentType => componentType,
                        componentType => ComponentsOfType(SutSpecification, componentType));

            var missingComponentsGroupedByTheirTypes = 
                    (from it in expectedComponentsGroupedByTheirTypes
                    let actualComponentsOfType = actualComponents.TryFind(it.Key).OrElse(Immutable.Empty<Rule>()) 
                    let missingComponentsOfType = it.Value.Except(
                                                        actualComponentsOfType.Select(
                                                            c => MakeSentenceMeaningComparable(Left(c.ToImmutable()))))
                                                    .AsImmutable()
                    where missingComponentsOfType.Any()
                    select new { Type = it.Key, MissingComponents = missingComponentsOfType }
                    ).AsImmutable();

            Assert.IsFalse(
                missingComponentsGroupedByTheirTypes.Any(),
                "Missing the following expected SUT definition components:" + 
                Environment.NewLine +
                string.Join(
                    Environment.NewLine, 
                    missingComponentsGroupedByTheirTypes.Select(it => 
                        $"[{it.Type}] =>" +
                        Environment.NewLine +
                        string.Join(
                            Environment.NewLine, 
                            it.MissingComponents.Select(c => "   " + Dump(c))))) +
                Environment.NewLine +
                "Only following components were generated:" +
                Environment.NewLine +
                string.Join(
                    Environment.NewLine,
                    actualComponents.Select(it =>
                        $"[{it.Key}] =>" +
                        Environment.NewLine +
                        string.Join(
                            Environment.NewLine, 
                            it.Value.Select(c => "   " + Dump(c))))));

            static IReadOnlyCollection<Rule> ComponentsOfType(SutSpecification sutSpecification, string type) =>
                type switch 
                {
                    "Воздействие" => sutSpecification.Effects,
                    _ => throw new InvalidOperationException($"Cannot get SUT specification components of type '{type}'")
                };

            static SentenceMeaning MakeSentenceMeaningComparable(SentenceMeaning sentenceMeaning) =>
                sentenceMeaning.Fold2(
                    rules => new StructuralEquatableArray<Rule>(rules) as IReadOnlyCollection<Rule>,
                    statements => new StructuralEquatableArray<ComplexTerm>(statements) as IReadOnlyCollection<ComplexTerm>);
        }

        [Then("the following scenarios should be generated")]
        public void ScenariosShouldBeGenerated(Table expectedScenarios)
        {
            var es = 
                from row in expectedScenarios.GetMultilineRows()
                from scenario in ParseTerm(row["Scenario Steps"])
                                    .CastToList<ComplexTerm>()
                                    .Select(route => new TestScenario(row["Expected Outcome"], new (route)))
                group scenario by scenario.ExpectedOutcome into g
                select g;
            var generatedScenarios = GenerateTestScenarios(SutSpecification);
            Assert.IsTrue(
                es.OrderBy(s => s.Key).Select(g => g)
                    .SequenceEqual(generatedScenarios.OrderBy(s => s.Key).Select(g => g)));
        }

        private SutSpecification ParseTextDescripton(Table description)
        {
            var sutDescriptionBuilder = new SutSpecificationBuilder(_grammarParser, _patterns.SentenceUnderstander);
            foreach (var row in description.GetMultilineRows())
            {
                switch (row["Type"])
                {
                    case "Сущность": sutDescriptionBuilder.DefineEntity(row["Phrasing"]); break;
                    case "Воздействие": sutDescriptionBuilder.DefineEffect(row["Phrasing"]); break;
                    case "Граничное условие": sutDescriptionBuilder.DefineBoundaryCondition(row["Phrasing"]); break;
                    case "Правило поведения": sutDescriptionBuilder.DefineBusinessRule(row["Phrasing"]); break;
                    default: throw new InvalidOperationException($"Unsupported type '{row["Type"]}' of SUT description fragment");
                }
            }

            return sutDescriptionBuilder.BuildDescription();
        }

        private readonly ScenarioContext _scenarioContext;
        private readonly IRussianGrammarParser _grammarParser;
        private readonly Patterns _patterns;
    }
}