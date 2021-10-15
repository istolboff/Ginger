using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine.Parsing
{
    using static Either;
    using static MayBe;

    internal sealed record ParsingResult<TInput, TValue>(TValue Value, TInput Rest)
    {
        public ParsingResult<TInput, TMappedValue> Map<TMappedValue>(Func<TValue, TMappedValue> mapValue) =>
            new (mapValue(Value), Rest);
    }

    internal sealed record ParsingError<TInput>(string Text, TInput Location);

    internal delegate Either<ParsingError<TInput>, ParsingResult<TInput, TValue>> Parser<TInput, TValue>(TInput input);

#pragma warning disable CA1801 // Review unused parameters
    internal sealed record ParsingTracer(Action<string> TraceParsingEvent, int MaxNestingLevel = 64)
#pragma warning restore CA1801
    {
        public Parser<TInput, T> Trace<TInput, T>(Parser<TInput, T> parser, string parserName) =>
            input =>
            {
                if (_nestingLevel >= MaxNestingLevel)
                {
                    return parser(input);
                }

                var linePrefix = new string(' ', _nestingLevel * 3);
                TraceParsingEvent($"{linePrefix}+{parserName}: <== {input}");
                ++_nestingLevel;
                var result = parser(input);
                --_nestingLevel;
                TraceParsingEvent($"{linePrefix}-{parserName}: {result.Fold(e => $"failed; {e.Text} at {e.Location}", r => r.Value?.ToString())}");
                return result;
            };

        private int _nestingLevel;
    }

    internal static class MonadicParsing
    {
        public static Parser<TInput, TValue2> Select<TInput, TValue, TValue2>(
            this Parser<TInput, TValue> parser,
            Func<TValue, TValue2> selector) =>
            input => parser(input).Map(r => Result(selector(r.Value), r.Rest));

        public static Parser<TInput, TValue2> SelectMany<TInput, TValue, TIntermediate, TValue2>(
            this Parser<TInput, TValue> parser,
            Func<TValue, Parser<TInput, TIntermediate>> selector,
            Func<TValue, TIntermediate, TValue2> projector) =>
            input => parser(input)
                .Apply(it =>
                    it
                        .Combine(it
                            .Map(r => selector(r.Value)(r.Rest))
                            .Flatten())
                        .Map(r => Result(projector(r.First.Value, r.Second.Value), r.Second.Rest)));

        public static Parser<TInput, TValue> Where<TInput, TValue>(this Parser<TInput, TValue> parser, Func<TValue, bool> predicate) =>
            input => parser(input) switch 
                        { 
                            var r when r.IsLeft || predicate(r.Right!.Value) => r,
                            var r => Left(new ParsingError<TInput>($"Unexpected value {r.Right!.Value}", input)) 
                        };

        public static Parser<TInput, TValue2> Then<TInput, TValue1, TValue2>(this Parser<TInput, TValue1> parser1, Parser<TInput, TValue2> parser2) =>
            input => parser1(input).Map(r => parser2(r.Rest)).Flatten();

        public static Parser<TInput, MayBe<TValue>> Optional<TInput, TValue>(Parser<TInput, TValue> parser) =>
            input => Right(parser(input).Fold(_ => Result(MakeNone<TValue>(), input), r => Result(Some(r.Value), r.Rest)));

        public static Parser<TInput, TValue> Or<TInput, TValue>(Parser<TInput, TValue> parser1, Parser<TInput, TValue> parser2) =>
            input => parser1(input).Fold(_ => parser2(input), Right<ParsingError<TInput>, ParsingResult<TInput, TValue>>);

        public static Parser<TInput, TValue> Or<TInput, TValue>(
            Parser<TInput, TValue> parser1,
            Parser<TInput, TValue> parser2,
            Parser<TInput, TValue> parser3,
            params Parser<TInput, TValue>[] parsers) =>
            parsers.Aggregate(Or(Or(parser1, parser2), parser3), Or);

        public static Parser<TInput, Either<TValue1, TValue2>> Either<TInput, TValue1, TValue2>(
            Parser<TInput, TValue1> parser1,
            Parser<TInput, TValue2> parser2) 
        => 
            input => 
                parser1(input)
                    .Fold(
                        _ => parser2(input).Map(r => r.Map(Right<TValue1, TValue2>)),
                        r => Right(r.Map(Left<TValue1, TValue2>)));

        public static Parser<TInput, IReadOnlyCollection<TValue>> Repeat<TInput, TValue>(
            Parser<TInput, TValue> parser,
            bool atLeastOnce = false) =>
        input => 
        {
            var result = new List<TValue>();
            while (true)
            {
                var nextElement = parser(input);
                if (nextElement.IsLeft)
                {
                    return (!atLeastOnce || result.Any()) switch
                    {
                        true => Right(Result(result.AsImmutable(), input)),
                        false => Left(nextElement.Left!)
                    };
                }

                result.Add(nextElement.Right!.Value);
                input = nextElement.Right!.Rest;
            }
        };

        public static Parser<TInput, IReadOnlyCollection<TValue>> Repeat<TInput, TValue, TSeparator>(
            Parser<TInput, TValue> parser,
            Parser<TInput, TSeparator> separatorParser,
            bool atLeastOnce = false) =>
            atLeastOnce
                ? from firstElement in parser
                  from theOtherElements in Repeat(separatorParser.Then(parser))
                  select new[] { firstElement }.Concat(theOtherElements).AsImmutable()
                : from elements in Optional(Repeat(parser, separatorParser, atLeastOnce: true))
                  select elements.OrElse(Array.Empty<TValue>());

        public static Parser<TInput, bool> ForwardDeclaration<TInput, T>(Parser<TInput, T>? unsued) =>
            input => unsued == null 
                ? Right(Result(true, input)) 
                : throw ProgramLogic.Error("call to ForwardDeclaration() seems to be unnecessary in this spot.");

        public static ParsingResult<TInput, TValue> Result<TInput, TValue>(TValue value, TInput rest) =>
            new (value, rest);
        
        public static readonly ParsingTracer Tracer = new (text => ParsingEvent?.Invoke(text));

        public static event Action<string>? ParsingEvent;
    }
}