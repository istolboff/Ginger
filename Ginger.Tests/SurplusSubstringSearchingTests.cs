using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner;

namespace Ginger.Tests
{
    using static MayBe;
    using static LongestCommonSubsequence;

    [TestClass]
    public sealed class SurplusSubstringSearchingTests
    {
        [TestMethod]
        public void SurplusSubstringSearchingCornerCaseTests()
        {
            CheckSearch(string.Empty, string.Empty, Array.Empty<Range>()).VerboseCheckThrowOnFailure();

            // any non-empty string gives itself as the only surplus fragment for the empty pattern
            Prop
                .ForAll(
                    NonEmptyStrings,
                    s => CheckSearch(string.Empty, s, new[] { new Range(Index.Start, Index.FromStart(s.Length)) })) 
                .VerboseCheckThrowOnFailure();

            // any non-empty pattern should give no no surplus fragments for an empty string
            Prop
                .ForAll(NonEmptyStrings, s => CheckSearch(s, string.Empty, Array.Empty<Range>()))
                .VerboseCheckThrowOnFailure();

            // searching surpluses in two identical strings gives nothing
            Prop
                .ForAll(NonEmptyStrings, s => CheckSearch(s, s, Array.Empty<Range>())) 
                .VerboseCheckThrowOnFailure();
        }

        [TestMethod]
        public void MainSurplusSubstringSearchingTests()
        {
            const string insertCharacters = "абвгдежзиклмнопрстуфхцчшщъыьэюя";
            var guidCharacters = new Dictionary<char, char>
                    {
                        ['0'] = '刃', ['1'] = '令', ['2'] = '難', ['3'] = '骨', ['4'] = '詩',['5'] = '海', ['6'] = '雪', ['7'] = '起',
                        ['8'] = '及', ['9'] = '英', ['a'] = '次', ['b'] = '誤', ['c'] = '能', ['d'] = '函', ['e'] = '純', ['f'] = '船'
                    };
            Prop
                .ForAll(
                    Arb.From(
                        from s in Arb.Default.String().Generator
                        where (s?.Length ?? 0) >= 100 && !s.Intersect(insertCharacters).Any()
                        from l in Gen.Choose(1, 10)
                        from h in Gen.ArrayOf(l, Gen.Choose(1, s.Length))
                        let surplusFragments = h
                                    .Distinct()
                                    .Select(i => 
                                        (
                                            Position: i,
                                            Fragment: GenerateSurplusFragment(i, insertCharacters, guidCharacters)
                                        ))
                                    .ToArray()
                        select (
                                Pattern: s,
                                surplusFragments,
                                WidenedString: surplusFragments
                                                .OrderByDescending(surplus => surplus.Position)
                                                .Aggregate(
                                                    s, 
                                                    (t, surplus) => t.Insert(surplus.Position, surplus.Fragment))
                               )),
                        testData => CheckSearch(
                                        testData.Pattern, 
                                        testData.WidenedString, 
                                        testData.surplusFragments
                                                .OrderBy(surplus => surplus.Position)
                                                .Aggregate(
                                                    (Locations: new List<Range>(), CurrentDelta: 0),
                                                    (accumulator, surplus) =>
                                                    {
                                                        accumulator.Locations.Add(
                                                            new Range(
                                                                Index.FromStart(accumulator.CurrentDelta + surplus.Position),
                                                                Index.FromStart(accumulator.CurrentDelta + surplus.Position + surplus.Fragment.Length)));
                                                        return (
                                                                accumulator.Locations, 
                                                                accumulator.CurrentDelta + surplus.Fragment.Length
                                                               );
                                                    }
                                                )
                                                .Locations
                                                .ToArray()))
                .VerboseCheckThrowOnFailure();

                static string GenerateSurplusFragment(
                    int seed, 
                    string insertCharacters, 
                    IReadOnlyDictionary<char, char> guidCharacters) 
                =>
                    EncodeGuid(Guid.NewGuid(), guidCharacters) +
                    insertCharacters.Substring(
                        seed % insertCharacters.Length, 
                        Math.Min(seed, insertCharacters.Length - (seed % insertCharacters.Length))) +
                    EncodeGuid(Guid.NewGuid(), guidCharacters);

                static string EncodeGuid(Guid guid, IReadOnlyDictionary<char, char> guidCharacters) =>
                    string.Join(string.Empty, guid.ToString("N").Select(ch => guidCharacters[ch]));
        }

        private static Property CheckSearch(
            string pattern, 
            string widenedString, 
            Range[] surplusFragmentLocations)
        =>
            FindSurplusFragments(Chars(pattern), Chars(widenedString)) switch
            {
                var foundFragments when !foundFragments.Any() =>
                    (!surplusFragmentLocations.Any()).Label(
                        $"FindSurplusFragments(\"{pattern}\", \"{widenedString}\") did't find any surpluses, " +
                        "while it was expected to find the following ones: " + Environment.NewLine +
                        string.Join(
                            ", ", 
                            surplusFragmentLocations
                                .Select(it => $"\"{widenedString[it]}\" at {it.Start}"))),
                var foundFragments => 
                    surplusFragmentLocations.Any()
                        .Label(
                            $"FindSurplusFragments(\"{pattern}\", \"{widenedString}\") found some surplus fragments while it shouldn't have.")
                    .And(
                        foundFragments.Select(f => f.Location)
                            .SequenceEqual(surplusFragmentLocations)
                            .Label(
                                $"FindSurplusFragments(\"{pattern}\", \"{widenedString}\") produced wrong result." + Environment.NewLine +
                                $"Expected: " + string.Join(", ", surplusFragmentLocations) + Environment.NewLine +
                                $"Actual:   " + string.Join(", ", foundFragments.Select(f => f.Location))))
            };

        static IReadOnlyCollection<char> Chars(IEnumerable<char> chars) => 
            new StructuralEquatableArray<char>(chars);

        private static Arbitrary<string> NonEmptyStrings =>
            Arb.From(
                from s in Arb.Default.String().Generator
                where !string.IsNullOrEmpty(s)
                select s);
    }
}