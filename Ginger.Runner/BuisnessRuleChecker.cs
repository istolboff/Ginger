using System.Collections.Generic;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using static DomainApi;
    using static Prolog.Engine.Parsing.PrologParser;
    using static PrettyPrinting;

    public static class BuisnessRuleChecker
    {
        public static IReadOnlyCollection<IGrouping<string, TestScenario>> GenerateTestScenarios(
            SutSpecification sutDescription) =>
           (from businessRule in sutDescription.BusinessRules
            let query = ParseQuery($@"
                    начальноеСостояние(InitialState)
                    , FinalStateName = '{Print(businessRule.Outcome)}'
                    , конечноеСостояние(FinalStateName, FinalState)
                    , solve(InitialState, FinalStateName, FinalState, Solution)
                    , dictionaryValues(Solution, Route)")
            from proof in Proof.Find(sutDescription.BuildProgram(), query)
            let scenario = new TestScenario(
                Print(businessRule.Outcome),
                new (proof.Instantiations[Variable("Route")].CastToList<ComplexTerm>()))
            group scenario by scenario.ExpectedOutcome into g
            select g)
            .AsImmutable();
    }
}