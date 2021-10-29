using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Ginger.Runner.Solarix;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    using FunctorBuildingRecipe = Either<FunctorBase /* BuiltIn Functor */, (NameBuilingRecipe FunctorNameBuildingRecipe, int Arity) /* Regular Functor building recipe */>;
    using MeaningBuildingRecipe = Either<IReadOnlyCollection<RuleBuilingRecipe>, IReadOnlyCollection<ComplexTermBuilingRecipe>>;

    using static DomainApi;
    using static Either;
    using static Impl;
    using static MayBe;
    using static MeaningMetaModifiers;

    internal sealed record RuleBuilingRecipe(
        ComplexTermBuilingRecipe ConclusionBuildingRecipe, 
        IReadOnlyCollection<ComplexTermBuilingRecipe> PremiseBuildingRecipies);

    internal sealed record ComplexTermBuilingRecipe(
        Either<RegularComplexTermBuilingRecipe, UnderstanderBuilingRecipe> ConcreteBuilder) 
        : TermBuilingRecipe;

    internal sealed record RegularComplexTermBuilingRecipe(
        FunctorBuildingRecipe FunctorBuildingRecipe, 
        IReadOnlyCollection<TermBuilingRecipe> ArgumentBuildingRecipies);

    internal sealed record UnderstanderBuilingRecipe(NameBuilingRecipe QuoteLocator);

    internal abstract record TermBuilingRecipe;

    internal sealed record AtomBuilingRecipe(NameBuilingRecipe AtomContentBuilder) : TermBuilingRecipe;

    internal sealed record NumberBuilingRecipe(NameBuilingRecipe NumberTextRepresentationBuilder) : TermBuilingRecipe;

    internal sealed record VariableBuilingRecipe(NameBuilingRecipe VariableNameBuilder) : TermBuilingRecipe;

    internal sealed record NameBuilingRecipe(
        IReadOnlyCollection<Func<ParsedSentence, string>> NameComponentGetters,
        bool CapitalizeFirstWord);

    internal sealed record MeaningBuilder(
            SentenceUnderstander SentenceUnderstander,
            IRussianGrammarParser GrammarParser,
            Func<MeaningBuilder, RuleBuilingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Rule>> BuildRule,
            Func<MeaningBuilder, ComplexTermBuilingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, ComplexTerm>> BuildComplexTerm,
            Func<MeaningBuilder, AtomBuilingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Atom>> BuildAtom,
            Func<MeaningBuilder, NumberBuilingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Prolog.Engine.Number>> BuildNumber,
            Func<MeaningBuilder, VariableBuilingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, Variable>> BuildVariable,
            Func<MeaningBuilder, FunctorBuildingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, FunctorBase>> BuildFunctor,
            Func<MeaningBuilder, NameBuilingRecipe, ParsedSentence, Either<FailedUnderstandingAttempt, string>> BuildName)
    {
        public Either<FailedUnderstandingAttempt, SentenceMeaning> BuildMeaning(MeaningBuildingRecipe meaningBuildingRecipe, ParsedSentence sentence) 
        =>
            LiftFailures(
                meaningBuildingRecipe
                    .Map2(
                        ruleRecipes => GatherBuiltElements(ruleRecipes.ConvertAll(recipe => BuildRule(this, recipe, sentence))),
                        statementRecipes => GatherBuiltElements(statementRecipes.ConvertAll(recipe => BuildComplexTerm(this, recipe, sentence)))));

        public static MeaningBuilder Create(
            SentenceUnderstander sentenceUnderstander, 
            IRussianGrammarParser grammarParser) 
        =>
            new (sentenceUnderstander, grammarParser, BuildRuleCore, BuildComplexTermCore, BuildAtomCore, BuildNumberCore, BuildVariableCore, BuildFunctorCore, BuildNameCore);

        private static Either<FailedUnderstandingAttempt, Rule> BuildRuleCore(MeaningBuilder @this, RuleBuilingRecipe recipe, ParsedSentence sentence) 
        =>
            from conclusion in @this.BuildComplexTerm(@this, recipe.ConclusionBuildingRecipe, sentence)
            from premises in GatherBuiltElements(recipe.PremiseBuildingRecipies.ConvertAll(r => @this.BuildComplexTerm(@this, r, sentence)))
            select Rule(conclusion, premises);

        private static Either<FailedUnderstandingAttempt, ComplexTerm> BuildComplexTermCore(MeaningBuilder @this, ComplexTermBuilingRecipe recipe, ParsedSentence sentence) 
        =>
            recipe.ConcreteBuilder.Fold(
                regularRecipe => 
                    from functor in @this.BuildFunctor(@this, regularRecipe.FunctorBuildingRecipe, sentence)
                    from arguments in GatherBuiltElements(regularRecipe.ArgumentBuildingRecipies.ConvertAll(r => Build(@this, r, sentence)))
                    select AccomodateInlinedArguments(functor, arguments),
                understanderRecipe => 
                    from quote in @this.BuildName(@this, understanderRecipe.QuoteLocator, sentence)
                    from understanding in MeaningMetaModifiers.BuildUnderstanding(
                                            quote, 
                                            @this.GrammarParser, 
                                            @this.SentenceUnderstander)
                    select understanding);

        private static Either<FailedUnderstandingAttempt, Term> Build(MeaningBuilder @this, TermBuilingRecipe recipe, ParsedSentence sentence) =>
            recipe switch
            {
                AtomBuilingRecipe atomRecipe => @this.BuildAtom(@this, atomRecipe, sentence).Map(t => t as Term),
                NumberBuilingRecipe numberRecipe => @this.BuildNumber(@this, numberRecipe, sentence).Map(t => t as Term),
                VariableBuilingRecipe variableRecipe => @this.BuildVariable(@this, variableRecipe, sentence).Map(t => t as Term),
                ComplexTermBuilingRecipe complexTermRecipe => @this.BuildComplexTerm(@this, complexTermRecipe, sentence).Map(t => t as Term),
                _ => throw ProgramLogic.Error($"no code provided to build Term from {recipe.GetType().Name}")
            };

        private static Either<FailedUnderstandingAttempt, Atom> BuildAtomCore(MeaningBuilder @this, AtomBuilingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.AtomContentBuilder, sentence).Map(Atom);

        private static Either<FailedUnderstandingAttempt, Prolog.Engine.Number> BuildNumberCore(MeaningBuilder @this, NumberBuilingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.NumberTextRepresentationBuilder, sentence).Map(numberRepresentation => Number(int.Parse(numberRepresentation, CultureInfo.CurrentCulture)));

        private static Either<FailedUnderstandingAttempt, Variable> BuildVariableCore(MeaningBuilder @this, VariableBuilingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.VariableNameBuilder, sentence).Map(Variable);

        private static Either<FailedUnderstandingAttempt, FunctorBase> BuildFunctorCore(MeaningBuilder @this, FunctorBuildingRecipe recipe, ParsedSentence sentence) =>
            recipe.Fold(
                builtInFinctor => Right(builtInFinctor),
                functorRecipe => @this
                                    .BuildName(@this, functorRecipe.FunctorNameBuildingRecipe, sentence)
                                    .Map(name => Functor(name, functorRecipe.Arity))
                                    .Map(f => f as FunctorBase));

        private static Either<FailedUnderstandingAttempt, string> BuildNameCore(MeaningBuilder @this, NameBuilingRecipe recipe, ParsedSentence sentence) =>
            recipe.NameComponentGetters.Count switch
            {
                0 => Left(new FailedUnderstandingAttempt(None, new EmptyNameBuilder())),
                1 => Right(recipe.NameComponentGetters.Single().Invoke(sentence)),
                _ => Right(string.Join(
                        string.Empty, 
                        recipe.NameComponentGetters.Select((wl, i) => 
                            i == 0 && !recipe.CapitalizeFirstWord
                                ? wl.Invoke(sentence)
                                : Russian.TextInfo.ToTitleCase(wl.Invoke(sentence)))))
            };

        private static Either<FailedUnderstandingAttempt, IReadOnlyCollection<T>> GatherBuiltElements<T>(
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

        private static Either<FailedUnderstandingAttempt, Either<IReadOnlyCollection<TLeft>, IReadOnlyCollection<TRight>>> LiftFailures<TLeft, TRight>(
            Either<Either<FailedUnderstandingAttempt, IReadOnlyCollection<TLeft>>, Either<FailedUnderstandingAttempt, IReadOnlyCollection<TRight>>> outcome)
        =>
            outcome.Fold(
                left => left.Map(leftItems => Left<IReadOnlyCollection<TLeft>, IReadOnlyCollection<TRight>>(leftItems)),
                right => right.Map(rightItems => Right<IReadOnlyCollection<TLeft>, IReadOnlyCollection<TRight>>(rightItems)));
    }
}