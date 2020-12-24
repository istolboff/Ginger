using Prolog.Engine;
using static Prolog.Engine.DomainApi;

namespace Prolog.Tests
{
    internal static class StockTerms
    {
        // ReSharper disable InconsistentNaming
        public static readonly ComplexTermFactory f = MakeComplexTerm("f");
        public static readonly ComplexTermFactory g = MakeComplexTerm("g");
        public static readonly ComplexTermFactory h = MakeComplexTerm("h");
        public static readonly ComplexTermFactory s = MakeComplexTerm("s");
        public static readonly ComplexTermFactory p = MakeComplexTerm("p");
        public static readonly ComplexTermFactory q = MakeComplexTerm("q");
        public static readonly ComplexTermFactory i = MakeComplexTerm("i");
        public static readonly ComplexTermFactory j = MakeComplexTerm("j");
        public static readonly ComplexTermFactory edge = MakeComplexTerm("edge");
        public static readonly ComplexTermFactory path = MakeComplexTerm("path");
        public static readonly ComplexTermFactory date = MakeComplexTerm("date");
        public static readonly ComplexTermFactory line = MakeComplexTerm("line");
        public static readonly ComplexTermFactory point = MakeComplexTerm("point");
        public static readonly ComplexTermFactory vertical = MakeComplexTerm("vertical");
        public static readonly ComplexTermFactory horizontal = MakeComplexTerm("horizontal");
        public static readonly ComplexTermFactory max = MakeComplexTerm("max");
        public static readonly ComplexTermFactory number = MakeComplexTerm("number");
        public static readonly ComplexTermFactory enjoys = MakeComplexTerm("enjoys");
        public static readonly ComplexTermFactory burger = MakeComplexTerm("burger");
        public static readonly ComplexTermFactory big_kahuna_burger = MakeComplexTerm("big_kahuna_burger");
        public static readonly ComplexTermFactory big_mac = MakeComplexTerm("big_mac");
        public static readonly ComplexTermFactory whopper = MakeComplexTerm("whopper");
        public static readonly ComplexTermFactory directTrain = MakeComplexTerm("directTrain");
        public static readonly ComplexTermFactory connected = MakeComplexTerm("connected");
        public static readonly ComplexTermFactory route = MakeComplexTerm("route");

        public static readonly Variable X = Variable("X");
        public static readonly Variable X1 = Variable("X1");
        public static readonly Variable Y = Variable("Y");
        public static readonly Variable Z = Variable("Z");
        public static readonly Variable P = Variable("P");
        public static readonly Variable A = Variable("A");
        public static readonly Variable B = Variable("B");
        public static readonly Variable C = Variable("C");
        public static readonly Variable R = Variable("R");
        public static readonly Variable Route = Variable("Route");
        public static readonly Variable Visited = Variable("Visited");

        public static readonly Atom atom = Atom("atom");
        public static readonly Atom a = Atom("a");
        public static readonly Atom b = Atom("b");
        public static readonly Atom c = Atom("c");
        public static readonly Atom d = Atom("d");

        public static readonly Atom vincent = Atom("vincent");

        public static readonly Atom dudweiler = Atom("dudweiler");
        public static readonly Atom fahlquemont = Atom("fahlquemont");
        public static readonly Atom forbach = Atom("forbach");
        public static readonly Atom freyming = Atom("freyming");
        public static readonly Atom metz = Atom("metz");
        public static readonly Atom nancy = Atom("nancy");
        public static readonly Atom saarbruecken = Atom("saarbruecken");
        public static readonly Atom stAvold = Atom("stAvold");

        public static readonly Number zero = Number(0);
        public static readonly Number one = Number(1);
        public static readonly Number two = Number(2);
        public static readonly Number three = Number(3);

        public static readonly Atom Something = Atom("AnUnimportantVariable");
        public static readonly Atom SomethingElse = Atom("AnotherUnimportantVariable");

        private static ComplexTermFactory MakeComplexTerm(string functorName) => 
            arguments => ComplexTerm(Functor(functorName, arguments.Length), arguments);
        // ReSharper enable InconsistentNaming
    }
}