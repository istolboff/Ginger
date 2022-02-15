global using SentenceMeaning =
    Prolog.Engine.Miscellaneous.Either<
        System.Collections.Generic.IReadOnlyCollection<Prolog.Engine.Rule>,
        System.Collections.Generic.IReadOnlyCollection<Prolog.Engine.ComplexTerm>>;

global using UnderstandingOutcome =
    Prolog.Engine.Miscellaneous.Either<
        Ginger.Runner.UnderstandingFailure,
        Ginger.Runner.UnderstoodSentence>;

global using MeaningBuildingRecipe = 
    Prolog.Engine.Miscellaneous.Either<
        System.Collections.Generic.IReadOnlyCollection<Ginger.Runner.RuleBuildingRecipe>, 
        System.Collections.Generic.IReadOnlyCollection<Ginger.Runner.ComplexTermBuildingRecipe>>;
