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
            Func<MeaningBuilder, RuleBuilingRecipe, ParsedSentence, MayBe<Rule>> BuildRule,
            Func<MeaningBuilder, ComplexTermBuilingRecipe, ParsedSentence, MayBe<ComplexTerm>> BuildComplexTerm,
            Func<MeaningBuilder, AtomBuilingRecipe, ParsedSentence, MayBe<Atom>> BuildAtom,
            Func<MeaningBuilder, NumberBuilingRecipe, ParsedSentence, MayBe<Prolog.Engine.Number>> BuildNumber,
            Func<MeaningBuilder, VariableBuilingRecipe, ParsedSentence, MayBe<Variable>> BuildVariable,
            Func<MeaningBuilder, FunctorBuildingRecipe, ParsedSentence, MayBe<FunctorBase>> BuildFunctor,
            Func<MeaningBuilder, NameBuilingRecipe, ParsedSentence, MayBe<string>> BuildName)
    {
        public MayBe<SentenceMeaning> BuildMeaning(MeaningBuildingRecipe meaningBuildingRecipe, ParsedSentence sentence) =>
            LiftOptionality(meaningBuildingRecipe.Map2(
                ruleRecipes => LiftOptionality(ruleRecipes.ConvertAll(recipe => BuildRule(this, recipe, sentence))),
                statementRecipes => LiftOptionality(statementRecipes.ConvertAll(recipe => BuildComplexTerm(this, recipe, sentence)))));

        public static MeaningBuilder Create(
            SentenceUnderstander sentenceUnderstander, 
            IRussianGrammarParser grammarParser) 
        =>
            new (sentenceUnderstander, grammarParser, Build, Build, Build, Build, Build, Build, Build);

        private static MayBe<Rule> Build(MeaningBuilder @this, RuleBuilingRecipe recipe, ParsedSentence sentence) =>
            from conclusion in @this.BuildComplexTerm(@this, recipe.ConclusionBuildingRecipe, sentence)
            from premises in LiftOptionality(recipe.PremiseBuildingRecipies.ConvertAll(r => @this.BuildComplexTerm(@this, r, sentence)))
            select Rule(conclusion, premises);

        private static MayBe<ComplexTerm> Build(MeaningBuilder @this, ComplexTermBuilingRecipe recipe, ParsedSentence sentence) =>
            recipe.ConcreteBuilder.Fold(
                regularRecipe => 
                    from functor in @this.BuildFunctor(@this, regularRecipe.FunctorBuildingRecipe, sentence)
                    from arguments in LiftOptionality(regularRecipe.ArgumentBuildingRecipies.ConvertAll(r => Build(@this, r, sentence)))
                    select AccomodateInlinedArguments(functor, arguments),
                understanderRecipe => 
                    from quote in @this.BuildName(@this, understanderRecipe.QuoteLocator, sentence)
                    from understanding in MeaningMetaModifiers.BuildUnderstanding(
                                            quote, 
                                            @this.GrammarParser, 
                                            @this.SentenceUnderstander)
                    select understanding);

        private static MayBe<Term> Build(MeaningBuilder @this, TermBuilingRecipe recipe, ParsedSentence sentence) =>
            recipe switch
            {
                AtomBuilingRecipe atomRecipe => @this.BuildAtom(@this, atomRecipe, sentence).Map(t => t as Term),
                NumberBuilingRecipe numberRecipe => @this.BuildNumber(@this, numberRecipe, sentence).Map(t => t as Term),
                VariableBuilingRecipe variableRecipe => @this.BuildVariable(@this, variableRecipe, sentence).Map(t => t as Term),
                ComplexTermBuilingRecipe complexTermRecipe => @this.BuildComplexTerm(@this, complexTermRecipe, sentence).Map(t => t as Term),
                _ => throw ProgramLogic.Error($"no code provided to build Term from {recipe.GetType().Name}")
            };

        private static MayBe<Atom> Build(MeaningBuilder @this, AtomBuilingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.AtomContentBuilder, sentence).Map(Atom);

        private static MayBe<Prolog.Engine.Number> Build(MeaningBuilder @this, NumberBuilingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.NumberTextRepresentationBuilder, sentence).Map(numberRepresentation => Number(int.Parse(numberRepresentation, CultureInfo.CurrentCulture)));

        private static MayBe<Variable> Build(MeaningBuilder @this, VariableBuilingRecipe recipe, ParsedSentence sentence) =>
            @this.BuildName(@this, recipe.VariableNameBuilder, sentence).Map(Variable);

        private static MayBe<FunctorBase> Build(MeaningBuilder @this, FunctorBuildingRecipe recipe, ParsedSentence sentence) =>
            recipe.Fold(
                Some,
                functorRecipe => @this
                                    .BuildName(@this, functorRecipe.FunctorNameBuildingRecipe, sentence)
                                    .Map(name => Functor(name, functorRecipe.Arity))
                                    .Map(f => f as FunctorBase));

        private static MayBe<string> Build(MeaningBuilder @this, NameBuilingRecipe recipe, ParsedSentence sentence) =>
            recipe.NameComponentGetters.Count switch
            {
                0 => None,
                1 => Some(recipe.NameComponentGetters.Single().Invoke(sentence)),
                _ => Some(string.Join(
                        string.Empty, 
                        recipe.NameComponentGetters.Select((wl, i) => 
                            i == 0 && !recipe.CapitalizeFirstWord
                                ? wl.Invoke(sentence)
                                : Russian.TextInfo.ToTitleCase(wl.Invoke(sentence)))))
            };
    }
}