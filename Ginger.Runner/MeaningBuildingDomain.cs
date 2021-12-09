using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Ginger.Runner.Solarix;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using MeaningWithRecipe = Either<IReadOnlyCollection<RuleWithRecipe>, IReadOnlyCollection<ComplexTermWithRecipe>>;
    using FunctorBuildingRecipe = Either<FunctorBase /* BuiltIn Functor */, (NameBuildingRecipe FunctorNameBuildingRecipe, int Arity) /* Regular Functor building recipe */>;
    using MeaningBuildingRecipe = Either<IReadOnlyCollection<RuleBuildingRecipe>, IReadOnlyCollection<ComplexTermBuildingRecipe>>;
    using UnderstandingOutcome = Either<IReadOnlyCollection<FailedUnderstandingAttempt>, UnderstoodSentence>;

    using static DomainApi;
    using static Either;
    using static Impl;
    using static MayBe;

    internal sealed record RuleBuildingRecipe(
        ComplexTermBuildingRecipe ConclusionBuildingRecipe, 
        IReadOnlyCollection<ComplexTermBuildingRecipe> PremiseBuildingRecipies);

    internal sealed record ComplexTermBuildingRecipe(
        Either<RegularComplexTermBuildingRecipe, UnderstanderBuildingRecipe> ConcreteBuilder) 
        : TermBuildingRecipe
    {
        public MayBe<NameBuildingRecipe> FunctorNameRecipe => 
            ConcreteBuilder.Fold(
                regularBuilder => regularBuilder.FunctorBuildingRecipe.Fold(
                    _ => None, functorRecipe => Some(functorRecipe.FunctorNameBuildingRecipe)),
                _ => None);
    }

    internal sealed record RegularComplexTermBuildingRecipe(
        FunctorBuildingRecipe FunctorBuildingRecipe, 
        StructuralEquatableArray<TermBuildingRecipe> ArgumentBuildingRecipies);

    internal sealed record UnderstanderBuildingRecipe(NameComponentBuildingRecipe QuoteLocator);

    internal abstract record TermBuildingRecipe;

    internal sealed record AtomBuildingRecipe(NameBuildingRecipe AtomContentBuilder) : TermBuildingRecipe;

    internal sealed record NumberBuildingRecipe(NameBuildingRecipe NumberTextRepresentationBuilder) : TermBuildingRecipe;

    internal sealed record VariableBuildingRecipe(NameBuildingRecipe VariableNameBuilder) : TermBuildingRecipe;

    internal sealed record NameBuildingRecipe(
        IReadOnlyCollection<NameComponentBuildingRecipe> NameComponentGetters,
        bool CapitalizeFirstWord)
    {
        public MayBe<WordOrQuotation<DisambiguatedWord>> TryLocateCompleteSubTreeOfNameElements(ParsedSentence sentence) =>
            from wordPositions in LiftOptionality(NameComponentGetters.ConvertAll(componentRecipe => componentRecipe.PositionInSentence))
            from subTree in sentence.TryLocateCompleteSubTreeOfSentenceElements(wordPositions)
            from mappedSubTree in subTree.TryMap((element, word) =>
                from nameComponent in NameComponentGetters.TryFirst(it => 
                        it.PositionInSentence == element.PositionInSentence && 
                        it.DisambiguatedLemmaVersion.HasValue)
                from patternLemmaVersion in nameComponent.DisambiguatedLemmaVersion
                from disambiguatedLemma in word.LemmaVersions.TryFindRelevantLemma(patternLemmaVersion)
                select new DisambiguatedWord(disambiguatedLemma, false, word.LeafLinkType))
            select mappedSubTree;
    }

    internal sealed record NameComponentBuildingRecipe(
        int? PositionInSentence,
        MayBe<LemmaVersion> DisambiguatedLemmaVersion,
        Func<ParsedSentence, string> ComponentGetter);

    internal sealed record MeaningBuilder(
            Func<MeaningBuilder, MeaningBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, MeaningWithRecipe>> BuildMeaning,
            Func<MeaningBuilder, IReadOnlyCollection<RuleBuildingRecipe>, ParsedSentence, Either<FailedUnderstandingAttempt, IReadOnlyCollection<RuleWithRecipe>>> BuildAllMeaningRules,
            Func<MeaningBuilder, IReadOnlyCollection<ComplexTermBuildingRecipe>, ParsedSentence, Either<FailedUnderstandingAttempt, IReadOnlyCollection<ComplexTermWithRecipe>>> BuildAllMeaningStatements,
            Func<MeaningBuilder, RuleBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Rule>> BuildRule,
            Func<MeaningBuilder, ComplexTermBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, ComplexTerm>> BuildComplexTerm,
            Func<MeaningBuilder, AtomBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Term>> BuildAtom,
            Func<MeaningBuilder, NumberBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Term>> BuildNumber,
            Func<MeaningBuilder, VariableBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Term>> BuildVariable,
            Func<MeaningBuilder, FunctorBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, FunctorBase>> BuildFunctor,
            Func<MeaningBuilder, NameBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, string>> BuildName)
    {
        public static MeaningBuilder Create(Func<string, UnderstandingOutcome> understandSentence)
        =>
            MetaUnderstand.Enrich(
                MetaVariable.Enrich(
                    MetaCall.Enrich(
                        new MeaningBuilder(
                            BuildMeaningCore,
                            BuildAllMeaningRulesCore, 
                            BuildAllMeaningStatementsCore, 
                            BuildRuleCore, 
                            BuildComplexTermCore, 
                            BuildAtomCore, 
                            BuildNumberCore, 
                            BuildVariableCore, 
                            BuildFunctorCore, 
                            BuildNameCore))),
                    understandSentence);

        public static Either<FailedUnderstandingAttempt, IReadOnlyCollection<T>> GatherBuiltElements<T>(
            IReadOnlyCollection<Either<FailedUnderstandingAttempt, T>> elements)
        =>
            elements.Where(e => e.IsLeft).Select(e => e.Left!.FailureReason).AsImmutable() switch
            {
                var failures when failures.Any() => 
                    Left(
                        new FailedUnderstandingAttempt(
                            None, 
                            MultipleUnderstandingFailureReasons.CreateFrom(failures))),
                _ => Right(elements.ConvertAll(e => e.Right!))
            };

        public static Either<FailedUnderstandingAttempt, Either<IReadOnlyCollection<TLeft>, IReadOnlyCollection<TRight>>> LiftFailures<TLeft, TRight>(
            Either<Either<FailedUnderstandingAttempt, IReadOnlyCollection<TLeft>>, Either<FailedUnderstandingAttempt, IReadOnlyCollection<TRight>>> outcome)
        =>
            outcome.Fold(
                left => left.Map(Left<IReadOnlyCollection<TLeft>, IReadOnlyCollection<TRight>>),
                right => right.Map(Right<IReadOnlyCollection<TLeft>, IReadOnlyCollection<TRight>>));

        private static Either<FailedUnderstandingAttempt, MeaningWithRecipe> BuildMeaningCore(
            MeaningBuilder @this, 
            MeaningBuildingRecipe meaningBuildingRecipe, 
            ParsedSentence sentence) 
        =>
            LiftFailures(
                meaningBuildingRecipe.Map2(
                ruleRecipes => @this.BuildAllMeaningRules(@this, ruleRecipes, sentence),
                statementRecipes => @this.BuildAllMeaningStatements(@this, statementRecipes, sentence)));

        private static Either<FailedUnderstandingAttempt, IReadOnlyCollection<RuleWithRecipe>> BuildAllMeaningRulesCore(
            MeaningBuilder @this, 
            IReadOnlyCollection<RuleBuildingRecipe> ruleRecipes,
            ParsedSentence sentence) 
        => 
        GatherBuiltElements(
          ruleRecipes.ConvertAll(recipe => 
             @this.BuildRule(@this, recipe, sentence)
             .Map(rule => new RuleWithRecipe(rule, recipe))));

        private static Either<FailedUnderstandingAttempt, IReadOnlyCollection<ComplexTermWithRecipe>> BuildAllMeaningStatementsCore(
            MeaningBuilder @this, 
            IReadOnlyCollection<ComplexTermBuildingRecipe> statementRecipes, 
            ParsedSentence sentence)
        =>
         GatherBuiltElements(
              statementRecipes.ConvertAll(recipe => 
                 @this.BuildComplexTerm(@this, recipe, sentence)
                 .Map(complexTerm => new ComplexTermWithRecipe(complexTerm, Some(recipe)))));

        private static Either<FailedUnderstandingAttempt, Rule> BuildRuleCore(MeaningBuilder @this, RuleBuildingRecipe recipe, ParsedSentence sentence) 
        =>
            from conclusion in @this.BuildComplexTerm(@this, recipe.ConclusionBuildingRecipe, sentence)
            from premises in GatherBuiltElements(recipe.PremiseBuildingRecipies.ConvertAll(r => @this.BuildComplexTerm(@this, r, sentence)))
            select Rule(conclusion, premises);

        private static Either<FailedUnderstandingAttempt, ComplexTerm> BuildComplexTermCore(
            MeaningBuilder @this, 
            ComplexTermBuildingRecipe recipe, 
            ParsedSentence sentence) 
        =>
            recipe.ConcreteBuilder.Fold(
                regularRecipe => 
                    from functor in @this.BuildFunctor(@this, regularRecipe.FunctorBuildingRecipe, sentence)
                    from arguments in GatherBuiltElements(regularRecipe.ArgumentBuildingRecipies.ConvertAll(r => Build(@this, r, sentence)))
                    select ComplexTerm(functor, arguments),
                understanderRecipe => 
                    @this
                        .BuildName(@this, new (understanderRecipe.QuoteLocator.ToImmutable(), false), sentence)
                        .Map(quote => ComplexTerm(Functor(MetaUnderstand.Name, 1), Atom(quote))));

        private static Either<FailedUnderstandingAttempt, Term> Build(
            MeaningBuilder @this, 
            TermBuildingRecipe recipe, 
            ParsedSentence sentence) 
        =>
            recipe switch
            {
                AtomBuildingRecipe atomRecipe => @this.BuildAtom(@this, atomRecipe, sentence),
                NumberBuildingRecipe numberRecipe => @this.BuildNumber(@this, numberRecipe, sentence),
                VariableBuildingRecipe variableRecipe => @this.BuildVariable(@this, variableRecipe, sentence),
                ComplexTermBuildingRecipe complexTermRecipe => @this.BuildComplexTerm(@this, complexTermRecipe, sentence).Map(t => t as Term),
                _ => throw ProgramLogic.Error($"no code provided to build Term from {recipe.GetType().Name}")
            };

        private static Either<FailedUnderstandingAttempt, Term> BuildAtomCore(MeaningBuilder @this, AtomBuildingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.AtomContentBuilder, sentence).Map(it => Atom(it) as Term);

        private static Either<FailedUnderstandingAttempt, Term> BuildNumberCore(MeaningBuilder @this, NumberBuildingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.NumberTextRepresentationBuilder, sentence).Map(numberRepresentation => Number(int.Parse(numberRepresentation, CultureInfo.CurrentCulture)) as Term);

        private static Either<FailedUnderstandingAttempt, Term> BuildVariableCore(MeaningBuilder @this, VariableBuildingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.VariableNameBuilder, sentence).Map(it => Variable(it) as Term);

        private static Either<FailedUnderstandingAttempt, FunctorBase> BuildFunctorCore(MeaningBuilder @this, FunctorBuildingRecipe recipe, ParsedSentence sentence) =>
            recipe.Fold(
                builtInFinctor => Right(builtInFinctor),
                functorRecipe => @this
                                    .BuildName(@this, functorRecipe.FunctorNameBuildingRecipe, sentence)
                                    .Map(name => Functor(name, functorRecipe.Arity))
                                    .Map(f => f as FunctorBase));

        private static Either<FailedUnderstandingAttempt, string> BuildNameCore(MeaningBuilder @this, NameBuildingRecipe recipe, ParsedSentence sentence) =>
            recipe.NameComponentGetters.Count switch
            {
                0 => Left(new FailedUnderstandingAttempt(None, new EmptyNameBuilder())),
                1 => Right(recipe.NameComponentGetters.Single().ComponentGetter(sentence)),
                _ => Right(string.Join(
                        string.Empty, 
                        recipe.NameComponentGetters.Select((wl, i) => 
                            i == 0 && !recipe.CapitalizeFirstWord
                                ? wl.ComponentGetter(sentence)
                                : Russian.TextInfo.ToTitleCase(wl.ComponentGetter(sentence)))))
            };
    }
}