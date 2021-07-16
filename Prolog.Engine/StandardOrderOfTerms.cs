using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using static MakeCompilerHappy;

    internal sealed class StandardOrderOfTerms : IComparer<Term>
    {
        public int Compare(Term? x, Term? y) =>
            (x, y) switch
            {
                (null, null) => 0,
                (null, _) => -1,
                (_, null) => 1,
                (Variable variableX, Variable variableY) => string.CompareOrdinal(variableX.Name, variableY.Name),
                (Number numberX, Number numberY) => numberX.Value.CompareTo(numberY.Value),
                (Atom atomX, Atom atomY) => string.CompareOrdinal(atomX.Characters, atomY.Characters),
                (ComplexTerm complexTermX, ComplexTerm complexTermY) => 
                    complexTermX.Functor.Arity != complexTermY.Functor.Arity
                        ? complexTermX.Functor.Arity.CompareTo(complexTermY.Functor.Arity)
                        : (!string.Equals(complexTermX.Functor.Name, complexTermY.Functor.Name)
                            ? string.CompareOrdinal(complexTermX.Functor.Name, complexTermY.Functor.Name)
                            : complexTermX.Arguments
                                .Zip(complexTermY.Arguments)
                                .Select(it => Compare(it.First, it.Second))
                                .TryFirst(it => it != 0)
                                .OrElse(0)),
                _ => GetTypeCode(SuppressCa1062(x)).CompareTo(GetTypeCode(SuppressCa1062(y)))
            };

        public string Compare1(Term? x, Term? y) =>
            Math.Sign(Compare(x, y)) switch 
            {
                -1 => "<",
                0 => "=",
                _ => ">"
            };

        public static StandardOrderOfTerms Default => new ();

        private static int GetTypeCode(Term term) =>
            term switch
            {
                Variable => 1,
                Number => 2,
                Atom => 3,
                _ => 4
            };
    }
}
