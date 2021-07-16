using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using static DomainApi;

    internal static class PrettyPrinting
    {
        public static string Print<T>(T @this, string enumSeparator = "; ") =>
            @this switch
            {
                Atom atom => atom.Characters,
                Number number => number.Value.ToString(CultureInfo.InvariantCulture),
                Variable variable => variable.Name,
                ComplexTerm cutOrFail when cutOrFail == Builtin.Cut || cutOrFail == Builtin.Fail => cutOrFail.Functor.Name,
                ComplexTerm list when list.IsList() => "[" + string.Join(", ", IterableList(list, strictMode: false).Select(t => Print(t))) + "]",
                ComplexTerm complexTerm => $"{complexTerm.Functor.Name}({string.Join(',', complexTerm.Arguments.Select(a => Print(a, enumSeparator)))})",
                Rule fact when !fact.Premises.Any() => $"{Print(fact.Conclusion)}.",
                Rule rule => $"{Print(rule.Conclusion, enumSeparator)}:-{string.Join(',', rule.Premises.Select(p => Print(p, enumSeparator)))}",
                UnificationResult unificationResult => unificationResult.Succeeded ? "success(" +  string.Join(" & ",unificationResult.Instantiations.Select(i => $"{Print(i.Key, enumSeparator)} = {Print(i.Value, enumSeparator)}")) + ")" : "no unification possible",
                IReadOnlyDictionary<Variable, Term> variableInstantiations => string.Join(", ", variableInstantiations.Select(kvp => $"[{Print(kvp.Key)}] = {Print(kvp.Value)}")),
                ValueTuple<Term, Term> unification => $"({Print(unification.Item1)}, {Print(unification.Item2)})",
                Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>> sentenceMeaning => sentenceMeaning.Fold(rules => Print(rules), statements => Print(statements)),
                string text => text,
                IEnumerable collection => string.Join(enumSeparator, collection.Cast<object>().Select(it => Print(it, enumSeparator))),
                _ => @this?.ToString() ?? "NULL"
            };
    }
}