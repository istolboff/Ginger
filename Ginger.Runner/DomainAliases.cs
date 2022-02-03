global using SentenceMeaning =
    Prolog.Engine.Miscellaneous.Either<
        System.Collections.Generic.IReadOnlyCollection<Prolog.Engine.Rule>,
        System.Collections.Generic.IReadOnlyCollection<Prolog.Engine.ComplexTerm>>;

global using UnderstandingOutcome =
    Prolog.Engine.Miscellaneous.Either<
        System.Collections.Generic.IReadOnlyCollection<Ginger.Runner.FailedUnderstandingAttempt>,
        Ginger.Runner.UnderstoodSentence>;
