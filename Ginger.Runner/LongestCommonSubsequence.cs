using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using static MayBe;

    internal static class LongestCommonSubsequence
    {
        public static IReadOnlyCollection<Subsequence<T>> FindSurplusFragments<T>(
            IReadOnlyCollection<T> pattern,
            IReadOnlyCollection<T> sequence,
            IEqualityComparer<T>? comparer = default)
        =>
            (pattern.Any(), sequence.Any()) switch
            {
                (false, false) => Array.Empty<Subsequence<T>>(),
                (false, true) => Whole(sequence).ToImmutable(),
                _ => FindSurplusFragmentsCore(
                        new List<Subsequence<T>> { Whole(pattern) },
                        new List<Subsequence<T>> { Whole(sequence) },
                        comparer ?? EqualityComparer<T>.Default)
            };

        private static IReadOnlyCollection<Subsequence<T>> FindSurplusFragmentsCore<T>(
            IList<Subsequence<T>> patternPortions,
            IList<Subsequence<T>> sequencePortions,
            IEqualityComparer<T> comparer)
        {
            while (true)
            {
                var keepGoing = false;
                foreach (var (patternPortion, sequencePortion, index) in
                            patternPortions
                                .ZipStrictly(sequencePortions)
                                .Select((it, index) => (it.First, it.Second, index))
                                .AsImmutable()
                                .Reverse())
                {
                    var commonSubsequence = FindLongestCommonSubsequence(patternPortion, sequencePortion, comparer);
                    if (commonSubsequence.HasValue)
                    {
                        SplitPortionsBy(
                            patternPortions, 
                            index, 
                            commonSubsequence.Value!.Item1.Location);
                        SplitPortionsBy(
                            sequencePortions, 
                            index, 
                            commonSubsequence.Value!.Item2.Location);
                        keepGoing = true;
                    }
                }

                if (!keepGoing)
                {
                    var patternWasFullyMatched = patternPortions.All(s => s.Count == 0);
                    return patternWasFullyMatched
                            ? sequencePortions.Where(s => s.Count != 0).AsImmutable()
                            : Array.Empty<Subsequence<T>>();
                }
            }

            static void SplitPortionsBy(
                IList<Subsequence<T>> portions,
                int indexOfPortionToSplit,
                Range locationToSplitBy)
            {
                ProgramLogic.Check(0 <= indexOfPortionToSplit && indexOfPortionToSplit < portions.Count);
                var portionToSplit = portions[indexOfPortionToSplit];
                var (leftSubPortion, rightSubPortion) = portionToSplit.SplitBy(locationToSplitBy);
                portions.RemoveAt(indexOfPortionToSplit);
                portions.Insert(indexOfPortionToSplit, rightSubPortion);
                portions.Insert(indexOfPortionToSplit, leftSubPortion);
                ProgramLogic.Check(portions.CoupleAdjucent().All(it => it.Previous.Location.Preceeds(it.Next.Location)));
            }
        }

        private static MayBe<(Subsequence<T>, Subsequence<T>)> FindLongestCommonSubsequence<T>(
            Subsequence<T> one,
            Subsequence<T> another,
            IEqualityComparer<T> comparer)
        =>
            one.Count == 0 || another.Count == 0
                ? None
                : (one.Count <= another.Count 
                        ? FindLongestCommonSubsequenceCore(one, another, comparer)
                        : FindLongestCommonSubsequenceCore(another, one, comparer).Map(r => (r.Item2, r.Item1)));

        private static MayBe<(Subsequence<T>, Subsequence<T>)> FindLongestCommonSubsequenceCore<T>(
            Subsequence<T> shortOne,
            Subsequence<T> longOne,
            IEqualityComparer<T> comparer)
        =>
           (from subsequenceLength in Enumerable.Range(0, shortOne.Count).Select(i => shortOne.Count - i)
            from subsequence in shortOne.SubsequencesOfLength(subsequenceLength)
            select subsequence.TryFindIn(longOne, comparer).Map(found => (subsequence, found)))
            .TryFirst(found => found.HasValue);

        private static Subsequence<T> Whole<T>(IReadOnlyCollection<T> sequence) =>
            new (
                sequence as IReadOnlyList<T> ?? new ReadOnlyCollectionToListAdapter<T>(sequence),
                0..sequence.Count);
    }

    internal sealed record Subsequence<T>(IReadOnlyList<T> Sequence, Range Location)
    {
        public int Count =>
            Location.End.Value - Location.Start.Value;

        public T this[int index] => Sequence[index];

        public MayBe<Subsequence<T>> TryFindIn(
            Subsequence<T> anotherSequence, 
            IEqualityComparer<T> comparer)
        =>
            anotherSequence
                .SubsequencesOfLength(Count, Sequence[Location.Start], comparer)
                .TryFirst(anotherSubsequence => Match(anotherSubsequence, comparer));

        public (Subsequence<T>, Subsequence<T>) SplitBy(Range range)
        {
            var (left, right) = Location.Split(range);
            return (this with { Location = left }, this with { Location = right });
        }

        public IEnumerable<Subsequence<T>> SubsequencesOfLength(int length) =>
            from start in Enumerable.Range(Location.Start.Value, Count - length + 1)
            select new Subsequence<T>(Sequence, start..(start + length));

        public IEnumerable<Subsequence<T>> SubsequencesOfLength(
            int length,
            T startingWith,
            IEqualityComparer<T> comparer)
        =>
            Enumerable
                .Range(Location.Start.Value, Count - length + 1)
                .Where(i => comparer.Equals(startingWith, Sequence[i]))
                .Select(i => new Subsequence<T>(Sequence, i..(i + length)));

        public override string ToString() =>
            string.Join(" ", AsEnumerable());

        private bool Match(Subsequence<T> other, IEqualityComparer<T> comparer)
        {
            ProgramLogic.Check(
                Count == other.Count, 
                "The code should be written so that only subsequences of the same length are compared.");

            if (Sequence is ReadOnlyCollectionToListAdapter<T> || 
                other.Sequence is ReadOnlyCollectionToListAdapter<T>)
            {
                return AsEnumerable().SequenceEqual(other.AsEnumerable(), comparer);
            }
            else
            {
                for (var i = 0; i != Count; ++i)
                {
                    if (!comparer.Equals(
                                    Sequence[Location.Start.Value + i], 
                                    other.Sequence[other.Location.Start.Value + i]))
                    {
                        return false;
                    }
                }

                return true;
            }
        }

        private IEnumerable<T> AsEnumerable() =>
            Sequence.Skip(Location.Start.Value).Take(Count);
    }

    internal record ReadOnlyCollectionToListAdapter<T>(IReadOnlyCollection<T> Sequence) : IReadOnlyList<T>
    {
        public T this[int index] => Sequence.ElementAt(index);

        public int Count => Sequence.Count;

        public IEnumerator<T> GetEnumerator() =>
            Sequence.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() =>
            GetEnumerator();
    }
}