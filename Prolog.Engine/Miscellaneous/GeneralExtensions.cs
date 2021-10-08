using System;
using System.Collections.Generic;
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
    }
}