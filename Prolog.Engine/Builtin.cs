using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;


namespace Prolog.Engine
{
    using static DomainApi;
    
    public static class Builtin
    {
#pragma warning disable CA1707
        public static readonly Variable _ = Variable("_");
#pragma warning restore CA1707

        public static readonly ComplexTerm Cut = ComplexTerm(Functor("!"));

        public static readonly ComplexTerm Fail = ComplexTerm(Functor("fail"));

        public static readonly ComplexTerm EmptyList = ComplexTerm(Functor("[]"));

        public static readonly Functor CallFunctor = Functor("call", 1);

        public static ComplexTerm Dot(Term head, Term tail) =>
            ComplexTerm(DotFunctor, head, tail);

        public static ComplexTerm Call(Term callee) =>
            ComplexTerm(CallFunctor, callee);

        private static readonly ComplexTermFactory Equal = StandardPredicate(
            "=", 
            Unification.CarryOut);

        private static readonly ComplexTermFactory NotEqual = StandardPredicate(
            @"\=", 
            (left, right) => Unification.Result(!Unification.CarryOut(left, right).Succeeded));

        private static readonly ComplexTermFactory GreaterThan = StandardPredicate(
            ">",
            (left, right) => Unification.Result(StandardOrderOfTerms.Default.Compare(left, right) > 0));

        private static readonly ComplexTermFactory GreaterThanOrEqual = StandardPredicate(
            ">=", 
            (left, right) => Unification.Result(StandardOrderOfTerms.Default.Compare(left, right) >= 0));

        private static readonly ComplexTermFactory LessThan = StandardPredicate(
            "<", 
            (left, right) => Unification.Result(StandardOrderOfTerms.Default.Compare(left, right) < 0));

        private static readonly ComplexTermFactory LessThanOrEqual = StandardPredicate(
            "<", 
            (left, right) => Unification.Result(StandardOrderOfTerms.Default.Compare(left, right) <= 0));

        private static readonly ComplexTermFactory Compare = StandardPredicate(
            "compare",
            (order, term1, term2) => 
                order switch 
                {
                    Atom expectedOrder when expectedOrder.Characters.IsOneOf("<", "=", ">") => 
                        Unification.Result(StandardOrderOfTerms.Default.Compare1(term1, term2) == expectedOrder.Characters),
                    Atom wrongOrder => 
                        throw new ArgumentException(
                            message: $"Invalid value '{wrongOrder.Characters}' for parameter order of 'compare/3'. " + 
                                     "Only variables, or atoms '<', '=', '>' are suuported.",
                            paramName: nameof(order)),
                    Variable x =>
                        Unification.Success(
                            x, 
                            Atom(StandardOrderOfTerms.Default.Compare1(term1, term2))),
                    _ => throw TypeError("first parameter of 'compare/3' should be either variable or one of atoms '<', '=', '>'")
                }
        );

        private static readonly ComplexTermFactory Subset = StandardPredicate(
            "subset",
            (subset, set) =>
                (subset, set) switch 
                {
                    (ComplexTerm subList, ComplexTerm list) when subList.IsList() && list.IsList() => 
                        IterableList(subList)
                            .AggregateWhile(
                                (UnificationResult: Unification.Success(), 
                                 MatchedElements: new HashSet<Term>(ReferenceEqualityComparer.Instance)),
                                (state, listElement) => 
                                    IterableList(list)
                                        .TryFirst(t => 
                                            !state.MatchedElements.Contains(t) && 
                                            Unification.IsPossible(listElement, t))
                                        .Map(matchingTerm => 
                                            (state.UnificationResult.And(Unification.CarryOut(listElement, matchingTerm)),
                                             state.MatchedElements.AddAndReturnSelf(matchingTerm)))
                                        .OrElse(() => (Unification.Failure, state.MatchedElements)),
                                state => state.UnificationResult.Succeeded)
                            .UnificationResult,
                    _ => throw TypeError("both parameters of 'subset' predicate should be lists")
                });

        private static readonly ComplexTermFactory Subtract = StandardPredicate(
            "subtract",
            (set, delete, result) => 
                (set, delete, result) switch 
                {
                    (ComplexTerm setList, ComplexTerm deleteList, Variable resultList) when setList.IsList() && deleteList.IsList() =>
                        IterableList(setList)
                            .Aggregate(
                                (UnificationResult: Unification.Success(), 
                                 MatchedElements: new HashSet<Term>(ReferenceEqualityComparer.Instance),
                                 ResultingList: EmptyList),
                                (state, term) => 
                                    IterableList(deleteList)
                                        .Where(t => !state.MatchedElements.Contains(t))
                                        .Select(t => new { t, unification = Unification.CarryOut(term, t).And(state.UnificationResult) })
                                        .TryFirst(it => it.unification.Succeeded)
                                        .Map(it => (it.unification, state.MatchedElements.AddAndReturnSelf(it.t), state.ResultingList))
                                        .OrElse(() => (state.UnificationResult, state.MatchedElements, Dot(term, state.ResultingList))))
                            .Apply(state => Unification.Success(resultList, ReverseList(state.ResultingList))
                                                .And(state.UnificationResult)),
                    _ => throw TypeError("first and second parameters of 'subtract' predicate should be lists, and the third one should be a variable")
                });

        private static readonly ComplexTermFactory Append = StandardPredicate(
            "append",
            (set1, set2, set3) =>
                (set1, set2, set3) switch 
                {
                    (ComplexTerm set1List, ComplexTerm set2List, Variable set3List) when set1List.IsList() && set2List.IsList() =>
                        Unification.Success(set3List, List(IterableList(set1List).Concat(IterableList(set2List)).Reverse())),
                    _ => throw TypeError("first and second parameters of 'append' predicate should be lists, and the third one should be a variable")
                });

        private static readonly ComplexTermFactory Sort = StandardPredicate(
            "sort",
            (list, sorted) =>
                (list, sorted) switch
                {
                    (ComplexTerm listToSort, Variable sortedList) when listToSort.IsList() => 
                        Unification.Success(
                            sortedList, 
                            List(IterableList(listToSort).OrderByDescending(it => it, StandardOrderOfTerms.Default))),
                    _ => throw TypeError("first parameter of 'sort' predicate should be a list, and the second one should be a variable")
                });

        private static readonly ComplexTermFactory Flatten = StandardPredicate(
            "flatten",
            (nested, flat) =>
                (nested, flat) switch
                {
                    (ComplexTerm nestedList, Variable flatList) when nestedList.IsList() =>
                        Unification.Success(flatList, List(FlattenList(nestedList).Reverse())),
                    _ => throw TypeError("first parameter of 'flatten' predicate should be a list, and the second one should be a variable")
                });

        private static readonly ComplexTermFactory Reverse = StandardPredicate(
            "reverse",
            (original, reversed) => 
                (original, reversed) switch
                {
                    (ComplexTerm originalList, Variable reversedList) when originalList.IsList() =>
                        Unification.Success(reversedList, List(IterableList(originalList))),
                    _ => throw TypeError("first parameter of 'reverse' predicate should be a list, and the second one should be a variable")
                }
        );

        private static readonly ComplexTermFactory FindAll = MetaPredicate( 
            "findall", 
            (program, obj, goal, list) =>
            {
                var (o, g, l) = (obj, (ComplexTerm)goal, (Variable)list);  
                var solutions = Proof.FindCore(program, g).AsImmutable();
                return Unification.Success(
                    l, 
                    !solutions.Any()
                        ? EmptyList
                        : List(solutions.Select(s => Proof.ApplyVariableInstantiationsCore(o, s.Instantiations)).ToArray()));
            });

        private static ComplexTerm Member(Term element, Term list) =>
            ComplexTerm(Functor("member", 2), element, list);

        private static ComplexTerm Not(Term term) => 
            ComplexTerm(Functor("not", 1), term);


        public static readonly IReadOnlyDictionary<string, ComplexTermFactory> BinaryOperators = 
            new Dictionary<string, ComplexTermFactory>
            {
                ["="] = Equal,
                [@"\="] = NotEqual,
                ["<"] = LessThan,
                [">"] = GreaterThan,
                ["<="] = LessThanOrEqual,
                [">="] = GreaterThanOrEqual
            };

        public static readonly IReadOnlyCollection<Rule> Rules = new[]
        {
            // standard comparison predicates
            Introduce(Equal, 2),
            Introduce(NotEqual, 2),
            Introduce(GreaterThanOrEqual, 2),
            Introduce(LessThan, 2),
            Introduce(Compare, 3),

            // findall
            Introduce(FindAll, 3),

            // list predicates
            Introduce(Append, 3),
            Introduce(Flatten, 2),
            Introduce(Reverse, 2),
            Introduce(Sort, 2),
            Introduce(Subset, 2),
            Introduce(Subtract, 3),

            // not()
            Rule(Not(X), Call(X), Cut, Fail),
            Fact(Not(_)),

            // member()
            Rule(Member(X, Dot(Y, _)), Equal(X, Y)),
            Rule(Member(X, Dot(_, T)), Member(X, T)),
        };

        public static readonly FunctorBase[] Functors = new[]
        {
            Cut.Functor,
            Fail.Functor,
            EmptyList.Functor,
            DotFunctor,
            CallFunctor
        };

        public static ComplexTerm? TryResolveFunctor(ComplexTerm complexTerm) =>
            Rules
                .Select(r => r.Conclusion.Functor)
                .FirstOrDefault(f => f.Name == complexTerm.Functor.Name && f.Arity == complexTerm.Functor.Arity)
                ?.Apply(f => complexTerm with { Functor = f });

        internal static Functor DotFunctor => 
            Functor(".", 2);

        private static Rule Introduce(ComplexTermFactory factory, int arity) => 
            new (
                Conclusion: arity switch 
                    {
                        2 => factory.Invoke(X, Y),
                        3 => factory.Invoke(X, Y, T),
                        _ => throw ProgramLogic.Error("Do not know how to introduce standard functor with artity different from 2 or 3")
                    }, 
                Premises: new ()
            );

        // stock terms, usable for formulating built-in rules
        private static Variable X => Variable("X");
        private static Variable Y => Variable("Y");
        private static Variable T => Variable("T");
    }
}