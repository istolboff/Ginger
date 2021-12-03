using System;

namespace Prolog.Engine.Miscellaneous
{
    internal static class RangeExtensions
    {
        public static bool Preceeds(this Range @this, Range secondRange) =>
            @this.End.Value <= secondRange.Start.Value;

        public static bool Includes(this Range @this, Range innerRange) =>
            @this.Start.Value <= innerRange.Start.Value &&
            @this.End.Value >= innerRange.End.Value;

        public static (Range, Range) Split(this Range @this, Range splitter)
        {
            ProgramLogic.Check(Includes(@this, splitter));
            return (new Range(@this.Start, splitter.Start), new Range(splitter.End, @this.End));
        }
    }
}