using System;
using JetBrains.Annotations;

namespace Prolog.Engine.Miscellaneous
{
    internal static class MakeCompilerHappy
    {
        public static T SuppressCa1062<T>([UsedImplicitly] T? reference) where T : class => 
            reference ?? throw new ArgumentNullException(nameof(reference));

#pragma warning disable CA1801 // Review unused parameters
        public static void SuppressCa1806<T>([UsedImplicitly] T unused)
#pragma warning restore CA1801
        {
        }

        public static T SuppressCs1673<T>(T value) =>
            value;
    }
}