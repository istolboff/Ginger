using System;
using JetBrains.Annotations;

namespace Prolog.Engine.Miscellaneous
{
    internal static class ProgramLogic
    {
        public static InvalidOperationException Error(string message) =>
            new ("Program Logic Error: " + message);

        [AssertionMethod]
        public static void Check(bool condition, string message)
        {
            if (!condition)
            {
                throw Error(message);
            }
        }

        [AssertionMethod]
        public static void Check(MayBe<string> error)
        {
            Check(!error.HasValue, error.OrElse(string.Empty));
        }
    }
}