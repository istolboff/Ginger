using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
#pragma warning disable SA1313 // ParameterNamesMustBeginWithLowerCaseLetter
    public abstract record Term
    {
        internal Term(Action? programLogicCheck = null) =>
            programLogicCheck?.Invoke();
    }

    public sealed record Atom(string Characters) : Term;

    public sealed record Number(int Value) : Term;

    public sealed record Variable(string Name, bool IsTemporary) : Term
    {
        public static Variable MakeNew(bool isTemporary = true) =>
            new (Name: $"_{++_nextNewVariableIndex}", IsTemporary: isTemporary);

        private static int _nextNewVariableIndex;
    }

    public abstract record FunctorBase(string Name, int Arity);

    public sealed record Functor(string Name, int Arity) : FunctorBase(Name, Arity);

#pragma warning disable CA1801 // Review unused parameters
    internal sealed record BinaryPredicate(string Name, int Arity, Func<IReadOnlyList<Term>, UnificationResult> Invoke) : FunctorBase(Name, Arity);

    internal sealed record MetaFunctor(string Name, int Arity, Func<IReadOnlyDictionary<(string FunctorName, int FunctorArity), IReadOnlyCollection<Rule>>, IReadOnlyList<Term>, UnificationResult> Invoke) : FunctorBase(Name, Arity);
#pragma warning restore CA1801

    public sealed record ComplexTerm(FunctorBase Functor, StructuralEquatableArray<Term> Arguments) 
        : Term(() => 
            {
                if (Functor.Arity != Arguments.Count) 
                {
                    throw new InvalidOperationException(
                        $"Inconsistent number of arguments ({Arguments.Count}) for '{Functor.Name}/{Functor.Arity}'");
                }
            });

    public sealed record Rule(ComplexTerm Conclusion, StructuralEquatableArray<ComplexTerm> Premises)
    {
        public bool IsFact => !Premises.Any();
    }

    public sealed record UnificationResult(bool Succeeded, StructuralEquatableDictionary<Variable, Term> Instantiations);
#pragma warning restore SA1313
}
