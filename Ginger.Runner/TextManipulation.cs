using System;
using System.Collections.Generic;
using System.Text;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using static MayBe;

    internal sealed record SubstringLocation(int Start, int Length);

    internal static class TextManipulation
    {
        public static string Join<T>(Func<T, T, string> getSeparator, IEnumerable<T> sequence)
        {
            var result = new StringBuilder();
            MayBe<T> previousElement = None;
            foreach (var element in sequence)
            {
                result.Append(previousElement.Map(pe => getSeparator(pe, element)).OrElse(string.Empty));
                result.Append(element?.ToString() ?? string.Empty);
                previousElement = Some(element);
            }

            return result.ToString();
        }
    }
}