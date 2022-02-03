using System;
using System.Collections.Generic;
using System.Linq;
using Ginger.Runner.Solarix;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using ComplexTermAdjuster = Either<IReadOnlyCollection<FailedUnderstandingAttempt>, Func<ComplexTerm, ComplexTerm>>;
    
    using static DomainApi;
    using static Either;
    using static MayBe;
    using static MeaningBuilder;
    using static MetaVariable;
    using static PrettyPrinting;

    internal static class MetaVariable
    {
        public const string Name = "@variable";

        public static MeaningBuilder Enrich(MeaningBuilder originalMeaningBuilder) =>
            originalMeaningBuilder with
            {
                BuildComplexTerm = (meaningBuilder, complexTermRecipe, parsedSentence) =>
                    originalMeaningBuilder
                            .BuildComplexTerm(meaningBuilder, complexTermRecipe, parsedSentence)
                            .Map(complexTerm => 
                                complexTerm with 
                                { 
                                    Arguments = new (complexTerm.Arguments.ConvertAll(Postprocess)) 
                                })
            };

        public static IEnumerable<string> GetArgumentAtoms(ComplexTerm ct) => 
            ct.Arguments
                .Cast<Atom>(_ => 
                    new InvalidOperationException(
                        $"Invalid use of {ct.Functor.Name} meta-modifier in {Print(ct)}. " +
                        "It can only accepts atoms as its arguments."))
                .Select(a => a.Characters);

        public static Term Postprocess(Term term) => 
            term switch 
            {
                ComplexTerm ct when ct.Functor.Name == Name =>
                    Variable(GetArgumentAtoms(ct).BuildIdentifier(Impl.Russian.TextInfo)),
                _ => term
            };
    }

    internal sealed record MetaCallFirstParameterShouldBeMetaVariable(ComplexTerm CallDetails) 
        : UnderstandingFailureReason;

    internal static class MetaCall
    {
        public const string Name = "@call";

        public static MeaningBuilder Enrich(MeaningBuilder originalMeaningBuilder) =>
            originalMeaningBuilder with
            {
                BuildComplexTerm = (meaningBuilder, complexTermRecipe, parsedSentence) =>
                    complexTermRecipe.ConcreteBuilder switch
                    {
                        (((var functorBase, _, true), _), _, true) when functorBase!.Name == Name =>
                            from complexTerm in originalMeaningBuilder.BuildComplexTerm(meaningBuilder, complexTermRecipe, parsedSentence)
                            from checkedComplexTerm in EnsureFirstArgumentIsVariableMetaModifier(complexTerm)
                            let functorName = GetArgumentAtoms(checkedComplexTerm)
                                                .BuildIdentifier(Impl.Russian.TextInfo, useCamelCase: true)
                            select ComplexTerm(
                                        Functor(functorName, complexTerm.Arguments.Count - 1),
                                        complexTerm.Arguments.Skip(1).Select(Postprocess).AsImmutable()),
                        _ => originalMeaningBuilder
                                .BuildComplexTerm(meaningBuilder, complexTermRecipe, parsedSentence)
                    }
            };

        private static Either<FailedUnderstandingAttempt, ComplexTerm> EnsureFirstArgumentIsVariableMetaModifier(
            ComplexTerm ct) 
        =>
            ct.Arguments switch
            {
                var arguments when arguments.Count > 0 && 
                                   arguments[0] is ComplexTerm firstArgument && 
                                   firstArgument.Functor.Name == MetaVariable.Name
                    => Right(firstArgument),
                _ => Left(new FailedUnderstandingAttempt(None, new MetaCallFirstParameterShouldBeMetaVariable(ct)))
            };
    }

    internal enum MetaUnderstandFailureReason
    { 
        MetaUnderstandExpectsSingleArgument,
        MetaUnderstandExpectsSingleArgumentOfTypeAtom,
        UnderstandingProducedRulesInsteadOfComplexTerms,
        IncompatibleNumberOfArgumentsAfterInlining,
        UnderstandingOfTopmostElementProducedSeveralComplexTermsInsteadOfSingleOne
    }

    internal sealed record MetaUnderstandFailure(
        MetaUnderstandFailureReason Reason, 
        Either<ComplexTerm, string> CallDetails) 
        : UnderstandingFailureReason;

    internal static class MetaUnderstand
    {
        public const string Name = "@understand";

        public static MeaningBuilder Enrich(
            MeaningBuilder originalMeaningBuilder, 
            Func<string, UnderstandingOutcome> understandSentence)
        =>
            originalMeaningBuilder with
            {
                BuildMeaning = (meaningBuilder, meaningBuildingRecipe, parsedSentence) =>
                    from meaning in originalMeaningBuilder.BuildMeaning(meaningBuilder, meaningBuildingRecipe, parsedSentence)
                    from adjustedMeaning in LiftFailures(
                        meaning.Map2(
                            rulesWithRecipe => GatherBuiltElements(
                                rulesWithRecipe.ConvertAll(it => Postprocess(it, understandSentence))),
                            statementsWithRecipe => Postprocess(statementsWithRecipe, understandSentence)))
                    select adjustedMeaning
            };

        public static InvalidOperationException MetaModifierError(string message) =>  new (message);

        private static Either<FailedUnderstandingAttempt, RuleWithRecipe> Postprocess(
            RuleWithRecipe ruleWithRecipe,
            Func<string, UnderstandingOutcome> understandSentence) 
        => 
            from conclusion in PostprocessTopmost(ruleWithRecipe.Rule.Conclusion, understandSentence)
            from premises in PostprocessTopmost(ruleWithRecipe.Rule.Premises, understandSentence)
            select ruleWithRecipe with { Rule = Rule(conclusion, premises) };

        private static Either<FailedUnderstandingAttempt, IReadOnlyCollection<ComplexTermWithRecipe>> Postprocess(
            IReadOnlyCollection<ComplexTermWithRecipe> statementsWithRecipe,
            Func<string, UnderstandingOutcome> understandSentence)
        =>
            PostprocessTopmost(statementsWithRecipe.ConvertAll(swr => swr.ComplexTerm), understandSentence)
                .Map(adjustedStatements => 
                    adjustedStatements
                        .Zip(statementsWithRecipe
                                .Select(swr => swr.Recipe)
                                .Concat(Enumerable
                                        .Range(default, int.MaxValue)
                                        .Select(_ => MakeNone<ComplexTermBuildingRecipe>())))
                        .Select(it => new ComplexTermWithRecipe(it.First, it.Second))
                    .AsImmutable());

        private static IReadOnlyCollection<ComplexTerm> CastToComplexTerms(IReadOnlyCollection<Term> terms) =>
            terms
                .Cast<ComplexTerm>(
                    wrongObject => ProgramLogic.Error(
                        $"{Name} metafunction is supposed to always produce nothing but ComplexTerms, " + 
                        $"yet somehow it generated {wrongObject}"))
                .AsImmutable();

            
        private static Either<FailedUnderstandingAttempt, ComplexTerm> PostprocessTopmost(
            ComplexTerm complexTerm,
            Func<string, UnderstandingOutcome> understandSentence) 
        =>
            Postprocess(complexTerm, understandSentence) switch
            {
                (var adjustedComplexTerm, _, true) => Right(adjustedComplexTerm!),
                (_, (var failedAttempts, _, true), false) => Left(failedAttempts!.First()),
                (_, (_, var complexTermAdjuster, false), false) => Left(
                    new FailedUnderstandingAttempt(
                        None,
                        new MetaUnderstandFailure(
                            MetaUnderstandFailureReason.UnderstandingOfTopmostElementProducedSeveralComplexTermsInsteadOfSingleOne,
                            Right($"Processing all {Name} calls in {Print(complexTerm)} produced several ComplexTerms instead of one: " +
                                   Environment.NewLine + Print(complexTermAdjuster!(ComplexTerm(Functor("placeholder")))) + Environment.NewLine +
                                   "It is impossible to set several of them in a Rule's conclusion."))))
            };

        private static Either<FailedUnderstandingAttempt, IReadOnlyCollection<ComplexTerm>> PostprocessTopmost(
            IReadOnlyCollection<ComplexTerm> complexTerms,
            Func<string, UnderstandingOutcome> understandSentence)
        {
            var statementsHolder = ComplexTerm(
                Functor("I" + Guid.NewGuid().ToString("N"), complexTerms.Count), complexTerms);
            return Postprocess(statementsHolder, understandSentence) switch
            {
                (var adjustedComplexTerm, _, true) => 
                    Right(CastToComplexTerms(adjustedComplexTerm!.Arguments)),
                (_, (_, var adjuster, false), false) =>
                    Right(CastToComplexTerms(adjuster!.Invoke(statementsHolder).Arguments)),
                (_, (var failedAttempts, _, true), false) => 
                    Left(failedAttempts!.First())
            };
        }

        private static Either<ComplexTerm, ComplexTermAdjuster> Postprocess(
            ComplexTerm complexTerm,
            Func<string, UnderstandingOutcome> understandSentence)
        {
            if (complexTerm.Functor.Name != Name)
            {
                var (arguments, adjusters) = complexTerm.Arguments
                        .Aggregate(
                            (
                                Terms: new List<Term>(),
                                Adjusters: new List<ComplexTermAdjuster>()
                            ),
                            (accumulator, term) => term switch
                            {
                                ComplexTerm ct => 
                                    Postprocess(ct, understandSentence)
                                        .Fold(
                                            ct1 => (accumulator.Terms.AddAndReturnSelf(ct1 as Term), accumulator.Adjusters),
                                            parentAdjustor => (accumulator.Terms, accumulator.Adjusters.AddAndReturnSelf(parentAdjustor))),
                                _ => 
                                    (accumulator.Terms.AddAndReturnSelf(term), accumulator.Adjusters),
                            });

                return adjusters
                        .AggregateWhile(
                            Right<IReadOnlyCollection<FailedUnderstandingAttempt>, ComplexTerm>(
                                complexTerm with { Arguments = new (arguments) }),
                            (result, adustComplexTerm) => 
                                from ct in result
                                from ctAdjuster in adustComplexTerm
                                select ctAdjuster(ct),
                            result => result.IsRight)
                        .Fold(Fail, Left<ComplexTerm, ComplexTermAdjuster>);
            }

            return TryUnderstand(complexTerm, understandSentence) switch
                {
                    (var understandingFailures, _, true) => 
                        Fail(understandingFailures!),
                    (_, var (parsedSentence, _, meaningWithRecipe), false) =>
                        TryUnwrapMeaning(meaningWithRecipe, parsedSentence)
                };

            static Either<ComplexTerm, ComplexTermAdjuster> Fail(
                IReadOnlyCollection<FailedUnderstandingAttempt> understandingFailures) 
            =>
                Right<ComplexTerm, ComplexTermAdjuster>(Left(understandingFailures));

            static UnderstandingOutcome TryUnderstand(
                ComplexTerm complexTerm, 
                Func<string, UnderstandingOutcome> understandSentence) 
            =>
#pragma warning disable CA1826 // Do not use Enumerable methods on indexable collections          
                complexTerm.Arguments.FirstOrDefault() switch
#pragma warning restore CA1826
                {
                    var firstArgument when (complexTerm.Arguments.Count != 1) || (firstArgument is not Prolog.Engine.Atom) =>
                        Left(
                            FailedAttempt(
                                new MetaUnderstandFailure(
                                    complexTerm.Arguments.Count != 1 
                                        ? MetaUnderstandFailureReason.MetaUnderstandExpectsSingleArgument
                                        : MetaUnderstandFailureReason.MetaUnderstandExpectsSingleArgumentOfTypeAtom,
                                    Left(complexTerm)))),
                    Atom atom => understandSentence(atom.Characters),
                    _ => throw ProgramLogic.ThisSwitchPathSeemsToBeUnreachable
                };

            static Either<ComplexTerm, ComplexTermAdjuster> TryUnwrapMeaning(
                Either<IReadOnlyCollection<RuleWithRecipe>, IReadOnlyCollection<ComplexTermWithRecipe>> meaningWithRecipe,
                ParsedSentence parsedSentence)
            =>
                meaningWithRecipe switch
                {
                    (var rulesWithRecipe, _, true) => 
                        Fail(
                            FailedAttempt(
                                new MetaUnderstandFailure(
                                    MetaUnderstandFailureReason.UnderstandingProducedRulesInsteadOfComplexTerms,
                                    Right($"'{parsedSentence.Sentence}' => {Print(rulesWithRecipe)}")))),
                    (_, var statementsWithRecipe, false) when statementsWithRecipe!.Count == 1 =>
                        Left(statementsWithRecipe.Single().ComplexTerm),
                    (_, var statementsWithRecipe, false) => 
                        Right<ComplexTerm, ComplexTermAdjuster>(
                            Right<IReadOnlyCollection<FailedUnderstandingAttempt>, Func<ComplexTerm, ComplexTerm>>(
                                parent => 
                                    AdjustParent(
                                        parent, 
                                        statementsWithRecipe!.ConvertAll(swr => swr.ComplexTerm))))
                };

            static ComplexTerm AdjustParent(
                ComplexTerm parent,
                IReadOnlyCollection<ComplexTerm> extraArguments)
            =>
                parent.Functor switch 
                {
                    var functor when functor == Builtin.DotFunctor =>
                        List(SkipFirstMetaUnderstander(parent.Arguments).SkipLast(1).Concat(extraArguments).Reverse()),
                    var functor =>
                        parent with 
                        { 
                            Functor = functor with { Arity = functor.Arity + extraArguments.Count - 1 },
                            Arguments = new (SkipFirstMetaUnderstander(parent.Arguments).Concat(extraArguments))
                        }
                };

            static IEnumerable<Term> SkipFirstMetaUnderstander(IEnumerable<Term> arguments)
            {
                var metaUnderstanderSkipped = false;
                foreach (var term in arguments)
                {
                    if (!metaUnderstanderSkipped &&
                        term is ComplexTerm complexTerm && 
                        complexTerm.Functor.Name == Name)
                    {
                        metaUnderstanderSkipped = true;
                        continue;
                    }

                    yield return term;
                }
            }
        }

        private static IReadOnlyCollection<FailedUnderstandingAttempt> FailedAttempt(MetaUnderstandFailure failure) =>
            new FailedUnderstandingAttempt(None, failure).ToImmutable();
    }
}