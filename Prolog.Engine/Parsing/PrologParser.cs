using System;
using System.Globalization;
using System.Linq;
using System.Collections.Generic;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine.Parsing
{
    using static Builtin;
    using static DomainApi;
    using static Either;
    using static MayBe;
    using static TextParsingPrimitives;
    using static MonadicParsing;

    internal sealed record PrologConstructionsParsers(
        Parser<TextInput, IReadOnlyCollection<Rule>> ProgramParser, 
        Parser<TextInput, IReadOnlyCollection<IReadOnlyCollection<ComplexTerm>>> QueryParser,
        Parser<TextInput, IReadOnlyCollection<ComplexTerm>> PremisesGroupParser,
        Parser<TextInput, Term> TermParser);
    
    public static class PrologParser
    {
        public static IReadOnlyCollection<Rule> ParseProgram(string input) =>
            TryParse(PrologParsers.ProgramParser, input, "Failed to parse Prolog program");

        public static IReadOnlyCollection<IReadOnlyCollection<ComplexTerm>> ParseQuery(string input) =>
            TryParse(PrologParsers.QueryParser, input, "Failed to parse Prolog queries");

        public static Term ParseTerm(string input) =>
            TryParse(PrologParsers.TermParser, input, "Failed to parse Prolog term");

        internal static MayBe<Term> TryParseTerm(string input) =>
            TryParseCore(PrologParsers.TermParser, input, string.Empty)
            .Fold(_ => MakeNone<Term>(), Some);

        private static T TryParse<T>(Parser<TextInput, T> parser, string input, string errorMessage) =>
            TryParseCore(parser, input, errorMessage)
            .Fold(error => throw error, result => result);

        private static Either<Exception, T> TryParseCore<T>(Parser<TextInput, T> parser, string input, string errorMessage) =>
            WholeInput(parser)(new TextInput(input))
             .Fold(
                parsingError => Left<Exception, T>(ParsingError($"{errorMessage} [{input}] {parsingError.Text} at {parsingError.Location.Position}")),
                result => Right<Exception, T>(result.Value));

        private static PrologConstructionsParsers BuildParsers()
        {
            var comma = Tracer.Trace(
                        Lexem(","), 
                        "comma");

            var unquotedAtom = Tracer.Trace(
                        from atomText in Lexem(
                            ch => char.IsLetter(ch) && char.IsLower(ch), 
                            ch => char.IsLetterOrDigit(ch) || ch == '_')
                        select Atom(atomText),
                        "unquotedAtom");

            var quotedAtom = Tracer.Trace(
                        from unused1 in SkipWhitespaces.Then(Expect('\''))
                        from letters in Repeat(Expect(ch => ch != '\'', ignoreCommentCharacter: true))
                        from unused2 in Expect('\'')
                        select Atom(string.Join(string.Empty, letters)),
                        "quotedAtom");

            var specialAtom = Tracer.Trace(
                        from a in Lexem("∥")
                        select Atom(a),
                        "specialAtom");

            var atom = Tracer.Trace(
                        Or(quotedAtom, unquotedAtom, specialAtom), 
                        "atom");

            var number = Tracer.Trace(
                        from sign in Optional(Lexem("+", "-"))
                        let multiplier = sign.Map(s => s == "-" ? -1 : 1).OrElse(1)
                        from digits in SkipWhitespaces.Then(Repeat(Expect(char.IsDigit), atLeastOnce: true))
                        let v = string.Join(string.Empty, digits)
                        select Number(multiplier * int.Parse(v, CultureInfo.InvariantCulture)),
                        "number");

            var variable = Tracer.Trace(
                        from name in Lexem(
                            ch => ch == '_' || (char.IsLetter(ch) && char.IsUpper(ch)),
                            ch => char.IsLetterOrDigit(ch) || ch == '_')
                        select name == "_" ? Builtin._ : Variable(name),
                        "variable");

            var functorName = Tracer.Trace(
                        Lexem(
                            ch => ch == '@' || (char.IsLetter(ch) && char.IsLower(ch)),
                            ch => char.IsLetterOrDigit(ch) || ch == '_'),
                        "functorName");

            Parser<TextInput, Term>? term = null;

            var infixExpressionOrSingleTerm = Tracer.Trace(
                        from delayUsage in ForwardDeclaration(term)
// ReSharper disable once AccessToModifiedClosure
                        from term1 in term!
                        from operationAndSecondTerm in Optional(
                                    from @operator in Lexem(Builtin.BinaryOperators.Keys.ToArray())
                                    let complexTermFactory = Builtin.BinaryOperators[@operator]
// ReSharper disable once AccessToModifiedClosure
                                    from rightPart in term!
                                    select (complexTermFactory, rightPart))
                        select operationAndSecondTerm
                            .Map(it => it.complexTermFactory(term1, it.rightPart) as Term)
                            .OrElse(() => term1),
                        "infixExpressionOrSingleTerm");

            var complexTerm = Tracer.Trace(
                        from functor in SkipWhitespaces.Then(functorName)
                        from unused1 in Lexem("(")
// ReSharper disable once AccessToModifiedClosure
                        from arguments in Repeat(infixExpressionOrSingleTerm, separatorParser: comma, atLeastOnce: true)
                        from unused2 in Lexem(")")
                        select ComplexTerm(Functor(functor, arguments.Count), arguments),
                        "complexTerm");

            var explicitList = Tracer.Trace(
                        from delayUsage in ForwardDeclaration(term)
// ReSharper disable once AccessToModifiedClosure
                        from elements in Repeat(infixExpressionOrSingleTerm, separatorParser: comma)
                        select List(elements.Reverse()),
                        "explicitList");

            var barList = Tracer.Trace(
                        from delayUsage in ForwardDeclaration(term)
// ReSharper disable once AccessToModifiedClosure
                        from head in infixExpressionOrSingleTerm
                        from unused in Lexem("|")
// ReSharper disable once AccessToModifiedClosure
                        from tail in infixExpressionOrSingleTerm
                        select Dot(head, tail),
                        "barList");

            var list = Tracer.Trace(
                        from unused in Lexem("[")
                        from elements in Or(barList, explicitList)
                        from unused1 in Lexem("]")
                        select elements,
                        "list");

            term = Tracer.Trace(
                Or(AsTerm(list), AsTerm(complexTerm), AsTerm(variable), AsTerm(number), AsTerm(atom)),
                "term");

            var infixExpression = Tracer.Trace(
                        from leftPart in term
                        from @operator in Lexem(Builtin.BinaryOperators.Keys.ToArray())
                        let complexTermFactory = Builtin.BinaryOperators[@operator]
                        from rightPart in term!
                        select complexTermFactory(leftPart, rightPart),
                        "infixExpression");

            var cut = Tracer.Trace(
                        from unused in Lexem("!")
                        select Cut,
                        "cut");

            var fail = Tracer.Trace(
                        from unused in Lexem("fail")
                        select Fail,
                        "fail");

            var premise = Tracer.Trace(
                        Or(
                            cut, 
                            fail, 
                            infixExpression, 
                            complexTerm.Select(ct => Builtin.TryResolveFunctor(ct) ?? ct),
                            variable.Select(Call)),
                        "premise");

            var fact = Tracer.Trace(
                        from conclusion in complexTerm
                        from unused in Lexem(".")
                        select Fact(conclusion),
                        "fact");

            var premisesGroup = Tracer.Trace(
                        Repeat(premise, separatorParser: comma, atLeastOnce: true),
                        "premisesGroup");

            var premisesAlternatives = Tracer.Trace(
                        Repeat(premisesGroup, separatorParser: Lexem(";"), atLeastOnce: true),
                        "premisesAlternatives");

            var rule = Tracer.Trace(
                        from conclusion in complexTerm
                        from unused in Lexem(":-")
                        from premisesAlternative in premisesAlternatives
                        from unused1 in Lexem(".")
                        select premisesAlternative
                                .Select(premises => Rule(conclusion, premises))
                                .ToArray(),
                        "rule");

            var program = Tracer.Trace(
                from ruleGroup in Repeat(Or(rule, fact.Select(f => new[] { f })))
                select ruleGroup.SelectMany(r => r).AsImmutable(),
                "program");

            return new (program, premisesAlternatives, premisesGroup, term);

            Parser<TextInput, Term> AsTerm<T>(Parser<TextInput, T> parser) where T : Term =>
                from v in parser
                select v as Term;
        }

        internal static readonly PrologConstructionsParsers PrologParsers = BuildParsers();
    }
}