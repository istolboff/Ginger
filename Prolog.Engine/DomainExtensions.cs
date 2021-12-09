using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using UnificationResult = StructuralEquatableDictionary<Variable, Term>;

    using static MayBe;
    using static DomainApi;

    public static class DomainExtensions
    {
        public static MayBe<UnificationResult> And(
            this MayBe<UnificationResult> @this, 
            MayBe<UnificationResult> another)
        =>
            from first in @this
            from second in another
            select first.Concat(second).GroupBy(i => i.Key).AsImmutable() switch
                    {
                        var accumulatedInstantiations 
                            when accumulatedInstantiations.All( // all instantiations of the same variable are equal
                                it => it.All(kvp => kvp.Value.Equals(it.First().Value)))
                            => Some(
                                new StructuralEquatableDictionary<Variable, Term>(
                                    accumulatedInstantiations.Select(it => it.First()).ToDictionary(it => it.Key, it => it.Value))),
                        _ => None
                    };

        public static bool IsList(this ComplexTerm @this) => 
            @this == Builtin.EmptyList || @this.Functor.Equals(Builtin.DotFunctor);

        public static IEnumerable<T> CastToList<T>(this Term term) =>
            term switch
            {
                ComplexTerm list when list.IsList() => IterableList(list).Cast<T>(),
                _ => throw new ArgumentException($"{term} is supposed to be a Prolog list of {typeof(T).Name}.", nameof(term))
            };
    }
}