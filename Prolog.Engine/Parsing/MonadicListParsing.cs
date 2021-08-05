using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine.Parsing
{
    using static Either;
    using static MonadicParsing;

    internal sealed record ListInput<T>(IReadOnlyList<T> Items, int Position)
    {
        public override string ToString() => 
            string.Join(
                Environment.NewLine, 
                Items.Take(Position).Select(it => it?.ToString())
                    .Concat("▲▲▲▲▲▲▲".ToImmutable())
                    .Concat(Items.Skip(Position).Select(it => it?.ToString())));
    }

    internal static class ListParsingPrmitives
    {
        public static Parser<ListInput<T>, T> Expect<T>(Func<T, bool> check) =>
            new Parser<ListInput<T>, T>(Read).Where(check);

        public static Parser<ListInput<T>, T> OneOf<T>(T firstExpectedvalue, params T[] otherExpectedValues) => 
            Expect((T item) => 
                EqualityComparer<T>.Default.Equals(firstExpectedvalue, item) ||
                otherExpectedValues.Contains(item));

        public static Parser<ListInput<T>, Unit> Sequence<T>(T firstElement, T secondElement, params T[] otherElements) =>
            from unused1 in OneOf(firstElement)
            from unit in otherElements.Any() 
                                ? OneOf(secondElement).Then(SequenceCore(otherElements, 0))
                                : OneOf(secondElement).Select(_ => Unit.Instance)
            select unit;

        public static Parser<ListInput<TValue>, TResult> WholeInput<TValue, TResult>(Parser<ListInput<TValue>, TResult> parser) =>
            from result in parser
            from unsused in Eof<TValue>()
            select result;

        private static Either<ParsingError<ListInput<T>>, ParsingResult<ListInput<T>, T>> Read<T>(ListInput<T> input) =>
            (input.Position < input.Items.Count) switch
            {
                true => Right(Result(input.Items[input.Position], input with { Position = input.Position + 1 })),
                false => Left(new ParsingError<ListInput<T>>("Attempt to read past the end of the input list", input))
            };

        private static Parser<ListInput<T>, Unit> SequenceCore<T>(T[] elements, int currentElement) =>
            currentElement == elements.Length - 1
                ? OneOf(elements[currentElement]).Select(_ => Unit.Instance)
                : OneOf(elements[currentElement]).Then(SequenceCore(elements, currentElement + 1));

        private static Parser<ListInput<T>, Unit> Eof<T>() =>
            input => (input.Position == input.Items.Count) switch 
                { 
                    true => Right(Result(Unit.Instance, input)),
                    false => Left(new ParsingError<ListInput<T>>("expected to be at the end of input", input))
                };
    }
}