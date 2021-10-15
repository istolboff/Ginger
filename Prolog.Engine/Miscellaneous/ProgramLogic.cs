using System;
using JetBrains.Annotations;

namespace Prolog.Engine.Miscellaneous
{
    internal static class ProgramLogic
    {
        public static InvalidOperationException Error(string message) =>
            new ("Program Logic Error: " + message);

        public static Exception ThisSwitchPathSeemsToBeUnreachable => 
            Error("This switch path appeared to be unreachable when writing the code, yet it actually is reachable afetr all.");

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