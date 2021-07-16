using System.Collections.Generic;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;

    using static DomainApi;

    internal sealed record UnderstoodSentence(ParsedSentence Sentence, string PatternId, SentenceMeaning Meaning);

    public sealed record BusinessRule(
        ComplexTerm Outcome,
        IReadOnlyCollection<ComplexTerm> DesiredSutState,
        IReadOnlyCollection<ComplexTerm> ExtraPremises)
    {
        public Rule ToPrologRule() =>
            Rule(ComplexTerm(Functor("конечноеСостояние", 2), Outcome, List(DesiredSutState.Reverse())), ExtraPremises);
    }

    public sealed record SutSpecification(
        IReadOnlyCollection<ComplexTerm> EntityDefinitions,
        IReadOnlyCollection<Rule> Effects,
        IReadOnlyCollection<BusinessRule> BusinessRules,
        object InitialState)
    {
        public IReadOnlyCollection<Rule> BuildProgram() =>
            EntityDefinitions.Select(Fact)
                .Concat(Effects)
                .Concat(BusinessRules.Select(br => br.ToPrologRule()))
                .AsImmutable();
    }

    public sealed record TestScenario(
        string ExpectedOutcome, 
        StructuralEquatableArray<ComplexTerm> Steps);
}