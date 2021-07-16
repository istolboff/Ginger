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
                .AsImmutable()
                .Apply(matchingElements =>
                    matchingElements.Count switch 
                    {
                        0 => None,
                        1 => Some(matchingElements.Single()),
                        _ => throw reportError(matchingElements)
                    });

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

        public static (TAccumulate, bool) AggregateIfAll<TSource, TAccumulate>(
            this IEnumerable<TSource> source,
            TAccumulate seed,
            Func<TSource, bool> predicate,
            Func<TAccumulate, TSource, TAccumulate> func)
        {
            using var sourceEnumerator = source.GetEnumerator();
            var result = seed;
            while (sourceEnumerator.MoveNext())
            {
                if (!predicate(sourceEnumerator.Current))
                {
                    return (result, false);
                }

                result = func(result, sourceEnumerator.Current);
            }

            return (result, true);
        }

        public static TValue AddAndReturnValue<TKey, TValue>(
            this IDictionary<TKey, TValue> @this, 
            TKey key, 
            TValue value)
        {
            @this.Add(key, value);
            return value;
        }

        public static IDictionary<TKey, TValue> AddAndReturnSelf<TKey, TValue>(
            this IDictionary<TKey, TValue> @this, 
            KeyValuePair<TKey, TValue> item)
        {
            @this.Add(item);
            return @this;
        }

        public static HashSet<T> AddAndReturnSelf<T>(this HashSet<T> @this, T item)
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
    }
    
    internal static class Immutable
    {
        public static IReadOnlyCollection<T> Empty<T>() => 
            Array.Empty<T>();
    }
}