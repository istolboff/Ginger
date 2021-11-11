using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using static DomainApi;

    public static class DomainExtensions
    {
        public static UnificationResult And(this UnificationResult @this, UnificationResult another)
        {
            if (!@this.Succeeded || !another.Succeeded)
            {
                return Unification.Failure;
            }

            var (result, success) = @this.Instantiations
                        .Concat(another.Instantiations)
                        .GroupBy(i => i.Key)
                        .AggregateIfAll(
                            new Dictionary<Variable, Term>() as IDictionary<Variable, Term>,
                            variableInstantiations => variableInstantiations.All(i => i.Value.Equals(variableInstantiations.First().Value)),
                            (accumulatedInstantiations, variableInstantiations) => 
                                accumulatedInstantiations.AddAndReturnSelf(variableInstantiations.First()));

            return success ? Unification.Success(result) : Unification.Failure;
        }

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