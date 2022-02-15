using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using static MayBe;

    public static class Unification
    {
        public static MayBe<UnificationResult> CarryOut(Term leftTerm, Term rightTerm) =>
            (leftTerm, rightTerm) switch 
            {
                (Atom leftAtom, Atom rightAtom) => Result(leftAtom.Equals(rightAtom)),
                (Number leftNumber, Number rightNumber) => Result(leftNumber.Equals(rightNumber)),
                (Variable leftVariable, Variable rightVariable) when leftVariable.Equals(rightVariable) => Success(),
                (Variable leftVariable, _) => Success(leftVariable, rightTerm),
                (_, Variable rightVariable) => Success(rightVariable, leftTerm),
                (ComplexTerm leftComplexTerm, ComplexTerm rightComplexTerm) =>
                    !leftComplexTerm.Functor.Equals(rightComplexTerm.Functor)
                        ? None
                        : leftComplexTerm.Arguments
                            .Zip(rightComplexTerm.Arguments)
                            .AggregateWhile(
                                Success(),
                                (result, correspondingArguments) => 
                                    result.And(CarryOut(correspondingArguments.Item1, correspondingArguments.Item2)),
                                result => result.HasValue),
                _ => None
            };

        public static bool IsPossible(Term leftTerm, Term rightTerm) =>
            (leftTerm, rightTerm) switch 
            {
                (Atom leftAtom, Atom rightAtom) => leftAtom.Equals(rightAtom),
                (Number leftNumber, Number rightNumber) => leftNumber.Equals(rightNumber),
                (Variable, _) => true,
                (_, Variable) => true,
                (ComplexTerm leftComplexTerm, ComplexTerm rightComplexTerm) =>
                    leftComplexTerm.Functor.Equals(rightComplexTerm.Functor) &&
                        leftComplexTerm.Arguments
                            .Zip(rightComplexTerm.Arguments)
                            .All(it => IsPossible(it.First, it.Second)),
                _ => false
            };

        public static MayBe<UnificationResult> Result(bool succeeded) => 
            succeeded ? Some(new UnificationResult()) : None;

        public static MayBe<UnificationResult> Success() => 
            Result(true);

        public static MayBe<UnificationResult> Success(Variable variable, Term value) =>
            Success(new UnificationResult(Enumerable.Repeat(KeyValuePair.Create(variable, value), 1)));

        public static MayBe<UnificationResult> Success(IEnumerable<KeyValuePair<Variable, Term>> variableInstantiations) => 
            Some(new UnificationResult(variableInstantiations));

        public static readonly MayBe<UnificationResult> Failure = Result(false);
    }
}