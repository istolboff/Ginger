using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Prolog.Engine.Miscellaneous
{
    internal static class GeneralExtensions
    {
        public static TResult Apply<T, TResult>(this T @this, Func<T, TResult> f) =>
            f(@this);

        public static bool IsOneOf<T>(this T value, T alternative1, T alternative2, params T[] otherAlternatives)
            => EqualityComparer<T>.Default.Equals(value, alternative1) || 
               EqualityComparer<T>.Default.Equals(value, alternative2) ||
               otherAlternatives.Contains(value);

        public static Type RemoveNullability(this Type @this) =>
            Nullable.GetUnderlyingType(@this) ?? @this;

        public static IEqualityComparer<T> ToEqualityComparer<T>(this Func<T, T, bool> compare)
            where T : notnull
        =>
            new EqualityComparerImpl<T>(compare);

        private record EqualityComparerImpl<T>(Func<T, T, bool> Compare) : IEqualityComparer<T>
            where T : notnull
        {
            public bool Equals(T? x, T? y) =>
                Compare(x!, y!);

            public int GetHashCode([DisallowNull] T obj) => 
                obj.GetHashCode();
        }
    }
}