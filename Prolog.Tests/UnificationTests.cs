using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;

namespace Prolog.Tests
{
    using V = System.Collections.Generic.Dictionary<Variable, Term>;

    using static DomainApi;

    
    [TestClass]
    public class UnificationTests
    {
        // If term1 and term2 are constants, then term1 and term2 unify if and only if 
        // they are the same atom, or the same number.
        [TestMethod]
        public void ConstantsUnification()
        {
            Assert.IsTrue(Unification.CarryOut(Atom("some-atom"), Atom("some-atom")).HasValue);
            Assert.IsFalse(Unification.CarryOut(Atom("some-atom"), Atom("some-other-atom")).HasValue);
            Assert.IsTrue(Unification.CarryOut(Number(42), Number(42)).HasValue);
            Assert.IsFalse(Unification.CarryOut(Number(42), Number(21)).HasValue);
            Assert.IsFalse(Unification.CarryOut(Atom("2"), Number(2)).HasValue);
        }

        // If term1 is a variable and term2 is any type of term, then term1 and term2 unify, 
        // and term1 is instantiated to term2 . Similarly, if term2 is a variable and term1 
        // is any type of term, then term1 and term2 unify, and term2 is instantiated to term1 
        [TestMethod]
        public void UnificationOfVariableWithAnyTypeOfTermButVariable()
        {
            var wrongUnifications = 
                (from term in new Term[] 
                    {
                        Atom("atom"),
                        Number(42),
                        // List(Atom("a"), Atom("b"), Number(42)),
                        ComplexTerm(Functor("hide", 3), Atom("X"), Number(42), ComplexTerm(Functor("father", 1), Atom("butch")))
                    }
                let variable = Variable("X")
                from arguments in new (Term, Term)[] { (variable, term), (term, variable) }
                let unification = Unification.CarryOut(arguments.Item1, arguments.Item2)
                where !unification.HasValue || !unification.Value![variable].Equals(term)
                select new { arguments.Item1, arguments.Item2, Unification = unification })
                .ToList();

            Assert.IsFalse(wrongUnifications.Any(), Environment.NewLine + string.Join(Environment.NewLine, wrongUnifications));
        }

        // (So if they are both variables, they’re both instantiated to each other, and we say that they share values.)
        [TestMethod]
        public void UnificationOfVariableWithAnotherVariable()
        {
            Assert.IsTrue(Unification.Success(X, Y).Equals(Unification.CarryOut(Variable("X"), Variable("Y"))));
        }

        // If term1 and term2 are complex terms, then they unify if and only if:
        //       They have the same functor and arity, and
        //       all their corresponding arguments unify, and
        //       the variable instantiations are compatible. 
        //       (For example, it is not possible to instantiate variable X to mia when 
        //        unifying one pair of arguments, and to instantiate X to vincent when
        //        unifying another pair of arguments .)
        [TestMethod]
        public void SuccessfulUnificationOfFunctor()
        {
            var wrongUnifications =
                (from test in new[]
                {
                    new 
                    {
                        ExpectedUnification = Unification.Success(X, Atom("atom")),
                        TermPairs = new[]
                            {
                                ("line(X)", "line(atom)"),
                                ("line(X, Something)", "line(atom, Something)"),
                                ("line(Something, X)", "line(Something, atom)"),
                                ("line(Something, X, SomethingElse)", "line(Something, atom, SomethingElse)")
                            }
                    },

                    new 
                    {
                        ExpectedUnification = Unification.Success(new V { [X] = Atom("atom"), [Y] = Number(1) }),
                        TermPairs = new[]
                            {
                                ("line(X, 1)", "line(atom, Y)"),
                                ("line(Something, X, 1)", "line(Something, atom, Y)"),
                                ("line(X, Something, 1)", "line(atom, Something, Y)"),
                                ("line(X, 1, Something)", "line(atom, Y, Something)"),
                                ("line(Something, X, 1, SomethingElse)", "line(Something, atom, Y, SomethingElse)")
                            }
                    },

                    new
                    {
                        ExpectedUnification = Unification.Success(X, Atom("atom")),
                        TermPairs = new[]
                            {
                                ("line(point(X))", "line(point(atom))"),
                                ("line(point(X), Something)", "line(point(atom), Something)"),
                                ("line(Something, point(X))", "line(Something, point(atom))"),
                                ("line(Something, point(X), SomethingElse)", "line(Something, point(atom), SomethingElse)"),

                                ("line(point(X, Something))", "line(point(atom, Something))"),
                                ("line(point(Something, X))", "line(point(Something, atom))"),
                                ("line(point(Something, X, SomethingElse))", "line(point(Something, atom, SomethingElse))")
                            }
                    },

                    new
                    {
                        ExpectedUnification = Unification.Success(X, PrologParser.ParseTerm("point(atom)")),
                        TermPairs = new[]
                            {
                                ("line(X)", "line(point(atom))"),
                                ("line(X, Something)", "line(point(atom), Something)"),
                                ("line(Something, X)", "line(Something, point(atom))"),
                                ("line(Something, X, SomethingElse)", "line(Something, point(atom), SomethingElse)")
                            }
                    }
                }
                from termPair in test.TermPairs
                from arguments in new[] { termPair, (termPair.Item2, termPair.Item1) }
                let unification = CarryOutUnification(arguments)
                where !unification.HasValue || !unification.Equals(test.ExpectedUnification)
                select new { arguments.Item1, arguments.Item2, test.ExpectedUnification, ActualUnification = unification })
                .ToList();

            Assert.IsFalse(wrongUnifications.Any(), Environment.NewLine + string.Join(Environment.NewLine, wrongUnifications));
        }

        [TestMethod]
        public void UnsuccessfulUnificationOfFunctor()
        {
            var wrongUnifications =
                (from termPair in new[]
                {
                    // incompatible functors
                    ("line(X)", "point(atom)"),
                    ("line(X, Something)", "point(atom, Something)"),
                    ("line(Something, X)", "point(Something, atom)"),
                    ("line(Something, X, SomethingElse)", "point(Something, atom, SomethingElse)"),

                    // incompatible arity
                    ("line(X)", "line(atom, Something)"),
                    ("line(X, Something)", "line(atom)"),
                    ("line(Something, X)", "line(atom)"),
                    ("line(Something, X, SomethingElse)", "line(Something, atom)"),

                    //  variable instantiations are incompatible
                    ("line(X, X)", "line(atom, one)"),
                    ("line(point(X), X)", "line(point(atom), one)"),
                    ("line(X, point(X))", "line(one, point(atom))")
                }
                from arguments in new[] { termPair, (termPair.Item2, termPair.Item1) }
                let unification = CarryOutUnification(arguments)
                where unification.HasValue
                select new { Arguments = arguments, Unification = unification })
                .ToList();

            Assert.IsFalse(wrongUnifications.Any(), string.Join(Environment.NewLine, wrongUnifications));
        }

        [TestMethod]
        public void UnificationWhenVariablesGetSubstitutedOnBothSides()
        {
            var wrongUnifications = 
                (from test in new[] 
                {
                    new 
                    { 
                        ExpectedUnification = Unification.Success(new V { [X] = X1, [Y] = Atom("atom"), [Z] = Number(1) }),
                        TermPair = ("date(X, Y, 1)", "date(X1, atom, Z)")
                    },
                    new
                    {
                        ExpectedUnification = Unification.Failure,
                        TermPair = ("point(1, X)", "point(X, 2)")
                    }
                }
                let unification = CarryOutUnification(test.TermPair)
                where !unification.Equals(test.ExpectedUnification)
                select new { test.TermPair.Item1, test.TermPair.Item2, test.ExpectedUnification, ActualUnification = unification })
                .ToList();

            Assert.IsFalse(wrongUnifications.Any(), string.Join(Environment.NewLine, wrongUnifications));
        }

        private static MayBe<UnificationResult> CarryOutUnification((string First, string Second) expressions) =>
            Unification.CarryOut(
                PrologParser.ParseTerm(expressions.First), 
                PrologParser.ParseTerm(expressions.Second));

        private static readonly Variable X = new ("X", false);
        private static readonly Variable X1 = new ("X1", false);
        private static readonly Variable Y = new ("Y", false);
        private static readonly Variable Z = new ("Z", false);
    }
}
