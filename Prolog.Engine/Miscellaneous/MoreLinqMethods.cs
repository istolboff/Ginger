using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Prolog.Engine.Miscellaneous
{
    using static MayBe;

    internal static class MoreLinqMethods
    {
        public static IReadOnlyCollection<T> ToImmutable<T>(this T @this) => 
            new[] { @this };

        public static IReadOnlyCollection<T> AsImmutable<T>(this IEnumerable<T> @this) =>
            @this as IReadOnlyCollection<T> ?? @this.ToArray();

        public static IReadOnlyDictionary<TKey, TValue> AsReadOnlyDictionary<TKey, TValue>(
            this IDictionary<TKey, TValue> @this)
            where TKey : notnull
        =>
            @this as IReadOnlyDictionary<TKey, TValue> ?? new Dictionary<TKey, TValue>(@this);

        public static IReadOnlyCollection<TResult> ConvertAll<T, TResult>(
            this IEnumerable<T> @this, 
            Converter<T, TResult> converter) 
        =>
            @this is T[] array 
                ? Array.ConvertAll(array, converter) 
                : @this.Select(it => converter(it)).AsImmutable();

        public static MayBe<TValue> TryFind<TKey, TValue>(this IReadOnlyDictionary<TKey, TValue> @this, TKey key) =>
            @this.TryGetValue(key, out var result) ? Some(result) : None;

        public static MayBe<T> TryFirst<T>(this IEnumerable<T> @this, Func<T, bool> predicate)
        {
            foreach (var element in @this)
            {
                if (predicate(element))
                {
                    return Some(element);
                }
            }

            return None;
        }
            
        public static MayBe<T> TryFirst<T>(this IEnumerable<T> @this) =>
            @this.TryFirst(_ => true);

        public static MayBe<T> TrySingle<T>(
            this IEnumerable<T> @this,
            Func<T, bool> predicate,
            Func<IReadOnlyCollection<T>, Exception> reportError) =>
            @this
                .Where(predicate)
                .Take(2)
                .AsImmutable() switch
                {
                    var matches when matches.Count == 0 => None,
                    var matches when matches.Count == 1 => Some(matches.Single()),
                    var matches => throw reportError(matches)
                };

        public static T Single<T>(
            this IEnumerable<T> @this,
            Func<T, bool> predicate,
            Func<IReadOnlyCollection<T>, Exception> reportError) =>
            @this
                .TrySingle(predicate, reportError)
                .OrElse(() => throw reportError(Array.Empty<T>()));

        public static TAccumulate AggregateWhile<TSource, TAccumulate>(
            this IEnumerable<TSource> source,
            TAccumulate seed,
            Func<TAccumulate, TSource, TAccumulate> func,
            Func<TAccumulate, bool> keepGoing)
        {
            using var sourceEnumerator = source.GetEnumerator();
            var result = seed;
            while (keepGoing(result) && sourceEnumerator.MoveNext())
            {
                result = func(result, sourceEnumerator.Current);
            }

            return result;
        }

        public static TValue AddAndReturnValue<TKey, TValue>(
            this IDictionary<TKey, TValue> @this, 
            TKey key, 
            TValue value)
        {
            @this.Add(key, value);
            return value;
        }

        public static TValue GetOrCreate<TKey, TValue>(
            this IDictionary<TKey, TValue> @this, 
            TKey key,
            Func<TValue> createValue)
        =>
            @this.TryGetValue(key, out var value) 
                ? value 
                : @this.AddAndReturnValue(key, createValue());

        public static TCollection AddAndReturnSelf<TCollection, TElement>(this TCollection @this, TElement item)
            where TCollection : ICollection<TElement>
        {
            @this.Add(item);
            return @this;
        }
        
        public static IEnumerable<TPartitioned> Partition<T, TPartitioned>(
            this IEnumerable<T> @this, 
            Func<T, T, TPartitioned> makePartition)
        {
            var first = default(T);
            var setFirstElement = true;

            foreach (var item in @this)
            {
                if (setFirstElement)
                {
                    first = item;
                    setFirstElement = false;
                }
                else
                {
                    yield return makePartition(first!, item);
                    setFirstElement = true;
                }
            }
        }

        public static IEnumerable<IEnumerable<T>> Split<T>(this IEnumerable<T> @this, Func<T, bool> isSplitter)
        {
            var nextValue = new List<T>();
            foreach (var item in @this)
            {
                if (isSplitter(item))
                {
                    if (nextValue.Any())
                    {
                        yield return nextValue;
                        nextValue = new List<T>();
                    }
                }
                else
                {
                    nextValue.Add(item);
                }
            }

            if (nextValue.Any())
            {
                yield return nextValue;
            }
        }

        public static IReadOnlyCollection<IReadOnlyCollection<TResult>> Split<T, TResult>(
            this IReadOnlyCollection<T> @this, 
            Func<T, bool> isSplitter, 
            Func<T, TResult> project) 
        =>
            Split(@this, isSplitter).Select(segment => segment.Select(project).AsImmutable()).AsImmutable();

        public static IEnumerable<T> Cast<T>(
            this IEnumerable @this, 
            Func<object, Exception> makeInvalidTypeException)
        {
            foreach (var it in @this)
            {
                yield return it is T result ? result : throw makeInvalidTypeException(it);
            }
        }

        public static bool HasMoreThanOneElement<T>(this IReadOnlyCollection<T> @this) =>
            @this.Count > 1;

        public static bool HasMoreThanOneElement<T>(this IEnumerable<T> @this) =>
            @this.Skip(1).Any();

        public static bool IsSubsetOf<T>(this IEnumerable<T> @this, IEnumerable<T> that) =>
            !@this.Except(that).Any();

        public static int FindIndex<T>(this IReadOnlyCollection<T> @this, Predicate<T> predicate) =>
            @this switch
            {
                T[] array => Array.FindIndex(array, predicate),
                List<T> list => list.FindIndex(predicate),
                _ => @this
                        .Select((item, index) => (item, index))
                        .TryFirst(it => predicate(it.item))
                        .Map(it => it.index)
                        .OrElse(-1)
            };

        public static (IReadOnlyCollection<TLeft> Lefts, IReadOnlyCollection<TRight> Rights) Segregate<T, TLeft, TRight>(
            this IReadOnlyCollection<T> @this,
            Func<T, Either<TLeft, TRight>> func) 
        =>
            @this.Aggregate(
                (Lefts: new List<TLeft>(), Rights: new List<TRight>()), 
                (accumulator, it) => func(it).Fold(
                    left => (accumulator.Lefts.AddAndReturnSelf(left), accumulator.Rights),
                    right => (accumulator.Lefts, accumulator.Rights.AddAndReturnSelf(right))));

        public static IEnumerable<(T1 First, T2 Second)> ZipStrictly<T1, T2>(
            this IReadOnlyCollection<T1> @this,
            IReadOnlyCollection<T2> other)
        {
            ProgramLogic.Check(@this.Count == other.Count);
            return @this.Zip(other);
        }
    }
    
    internal static class Immutable
    {
        public static IReadOnlyCollection<T> Empty<T>() => 
            Array.Empty<T>();
    }
}