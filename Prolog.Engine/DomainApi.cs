using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using UnificationResult = StructuralEquatableDictionary<Variable, Term>;
    
    using static Builtin;

    public static class DomainApi
    {
        public delegate ComplexTerm ComplexTermFactory(params Term[] arguments);

        public static Atom Atom(string atomText) => new (atomText);

        public static Number Number(int value) => new (value);

        public static Variable Variable(string name) => new (name, IsTemporary: false);

        public static Functor Functor(string name, int arity = 0) => new (name, arity);

        public static ComplexTerm ComplexTerm(FunctorBase functor, params Term[] arguments) => 
            new (functor, new (arguments));

        public static ComplexTerm ComplexTerm(FunctorBase functor, IEnumerable<Term> arguments) => 
            new (functor, new (arguments));

        public static Rule Fact(ComplexTerm fact) => 
            new (Conclusion: fact, Premises: StructuralEquatableArray.Empty<ComplexTerm>());

        public static Rule Rule(ComplexTerm conclusion, params ComplexTerm[] premises) => 
            new (conclusion, new (premises));

        public static Rule Rule(ComplexTerm conclusion, IEnumerable<ComplexTerm> premises) =>
            new (conclusion, new (premises));

        public static ComplexTerm List(params Term[] elements) => 
            List(elements.Reverse());

        public static ComplexTerm List(IEnumerable<Term> elements) => 
            elements.Aggregate(EmptyList, (list, element) => Dot(element, list));

        public static IEnumerable<Term> IterableList(ComplexTerm list, bool strictMode = true)
        {
            for (var current = list;
                 current != null && current.IsList() && current != EmptyList;
                 current = current.Arguments[1] as ComplexTerm)
            {
                yield return current.Arguments[0];

                if (!strictMode && current.Arguments[1] is not ComplexTerm unused)
                {
                    yield return current.Arguments[1];
                }
            }
        }

        internal static ComplexTermFactory StandardPredicate(string operatorName, Func<Term, Term, MayBe<UnificationResult>> invoke) =>
            formalArguments => ComplexTerm(
                new BinaryPredicate(
                    operatorName, 
                    2, 
                    arguments => invoke(arguments[0], arguments[1])),
                formalArguments);

        internal static ComplexTermFactory StandardPredicate(string operatorName, Func<Term, Term, Term, MayBe<UnificationResult>> invoke) =>
            formalArguments => ComplexTerm(
                new BinaryPredicate(
                    operatorName, 
                    3, 
                    arguments => invoke(arguments[0], arguments[1], arguments[2])),
                formalArguments);

        internal static ComplexTermFactory MetaPredicate(
            string operatorName, 
            Func<IReadOnlyDictionary<(string FunctorName, int FunctorArity), IReadOnlyCollection<Rule>>, Term, Term, Term, MayBe<UnificationResult>> invoke) =>
            formalArguments => ComplexTerm(
                new MetaFunctor(
                    operatorName, 
                    3, 
                    (program, arguments) => invoke(program, arguments[0], arguments[1], arguments[2])),
                formalArguments);

        internal static ComplexTerm ReverseList(ComplexTerm list) =>
            List(IterableList(list));

        internal static IEnumerable<Term> FlattenList(ComplexTerm list) =>
            IterableList(list)
                .SelectMany(item => item switch
                {
                    ComplexTerm complexTerm when complexTerm.IsList() => FlattenList(complexTerm),
                    _ => new[] { item }
                });

        internal static Exception TypeError(string message) => 
            new InvalidOperationException(message);
   }
}