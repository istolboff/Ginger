﻿using System;
using System.Collections.Generic;

namespace Prolog.Engine
{
#pragma warning disable SA1313 // ParameterNamesMustBeginWithLowerCaseLetter
    public abstract record Term;

    public sealed record Atom(string Characters) : Term;

    public sealed record Number(int Value) : Term;

#pragma warning disable CA1801 // Review unused parameters
    public sealed record Variable(string Name, bool IsTemporary = false) : Term;
#pragma warning restore CA1801

    public abstract record FunctorBase(string Name, int Arity);

    public sealed record Functor(string Name, int Arity) : FunctorBase(Name, Arity);

#pragma warning disable CA1801 // Review unused parameters
    internal sealed record BuiltinFunctor(string Name, int Arity, Func<IReadOnlyList<Term>, bool> Invoke) : FunctorBase(Name, Arity);
#pragma warning restore CA1801

    public sealed record ComplexTerm(FunctorBase Functor, StructuralEquatableArray<Term> Arguments) : Term;

    public sealed record Rule(ComplexTerm Conclusion, StructuralEquatableArray<ComplexTerm> Premises);

    public sealed record UnificationResult(bool Succeeded, StructuralEquatableDictionary<Variable, Term> Instantiations);
#pragma warning restore SA1313
}