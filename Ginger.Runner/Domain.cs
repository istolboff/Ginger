using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using static DomainApi;

    public sealed record SetDefinition(Rule Definition, Func<Term, ComplexTerm> BuildMembershipPredicate);

    public sealed record BusinessRule(
        ComplexTerm Outcome,
        IReadOnlyCollection<ComplexTerm> DesiredSutState,
        IReadOnlyCollection<ComplexTerm> ExtraPremises)
    {
        public Rule ToPrologRule() =>
            Rule(ComplexTerm(Functor("конечноеСостояние", 2), Outcome, List(DesiredSutState.Reverse())), ExtraPremises);
    }

    public sealed record InitialState(IReadOnlyCollection<ComplexTerm> StateComponents)
    {
        public Rule ToPrologRule() =>
            Rule(ComplexTerm(Functor("начальноеСостояние", 1), List(StateComponents.Reverse())));

        public static InitialState DeconstructRule(Rule rule)
        {
            if (!rule.IsFact)
            {
                throw new InvalidOperationException(
                    $"Rule {rule} can not be used as an Initial State description. " +
                    "Please specify either fact or list of complex terms");
            }

            return new (rule.Conclusion.Arguments.Cast<ComplexTerm>().AsImmutable());
        }
    }

    public sealed record SutSpecification(
        IReadOnlyCollection<SetDefinition> EntityDefinitions,
        IReadOnlyCollection<Rule> Effects,
        IReadOnlyCollection<BusinessRule> BusinessRules,
        IReadOnlyCollection<InitialState> InitialStates)
    {
        public IReadOnlyCollection<Rule> BuildProgram() =>
            EntityDefinitions.Select(it => it.Definition)
                .Concat(Effects)
                .Concat(BusinessRules.Select(br => br.ToPrologRule()))
                .Concat(InitialStates.Select(s => s.ToPrologRule()))
                .AsImmutable();
    }

    public sealed record TestScenario(
        string ExpectedOutcome, 
        StructuralEquatableArray<ComplexTerm> Steps);
}