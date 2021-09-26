using System;
using System.Collections.Generic;
using System.Linq;
using Ginger.Runner.Solarix;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;

namespace Ginger.Runner
{
    using static MayBe;
    using static MonadicParsing;
    using static ListParsingPrmitives;

    using Tokens = ListInput<string>;

    internal enum GenerationHint
    {
        Replicatable,
        PluralitySensitive
    }

    internal sealed record MarkedupWord(
        string Content,
        bool IsFixed,
        MayBe<GenerationHint> GenerationHint,
        MayBe<IReadOnlyCollection<string>> DisambiguatingCoordinates);

    internal static class NaturalLanguageSentenceMarkup
    {
        public static IReadOnlyCollection<MarkedupWord> ParseMarkup(
            this IRussianGrammarParser @this, 
            string markedupText)
        {
            var parsedText = ExtractTildasToSeparateTokens(@this.Tokenize(markedupText)).ToArray();
            return MarkupParser(new Tokens(parsedText, 0))
                        .Fold(
                            error => throw new InvalidOperationException(
                                            $"Error parsing markup text: {error.Text} at {error.Location}"), 
                            result => result.Value);
        }

        public const string ReplicatableHintText = "{и проч.}";

        public const string PluralSensitivityHintText = "{мн.}";

        private static IEnumerable<string> ExtractTildasToSeparateTokens(IEnumerable<string> tokens)
        {
            foreach (var token in tokens)
            {
                if (token == "~")
                {
                    yield return "~";
                }
                else
                {
                    if (token.StartsWith('~'))
                    {
                        yield return "~";
                    }
                    
                    yield return token.Trim('~');
                    
                    if (token.EndsWith('~'))
                    {
                        yield return "~";
                    }
                }
            }
        }

        private static Parser<Tokens, IReadOnlyCollection<MarkedupWord>> BuildParser()
        {
            var russianWord = Tracer.Trace(
                        Expect((string token) => !IsSpecialChar(token)), 
                        "russianWord");

            var generationHint = Tracer.Trace(
                        from unused1 in Expect("{")
                        from hintType in Either(Sequence("и", "проч"), Expect("мн"))
                        from unused2 in Sequence(".", "}")
                        select hintType.IsLeft ? GenerationHint.Replicatable : GenerationHint.PluralitySensitive,
                        "genereationHint");

            var singleDisambiguatingCoordinate = Tracer.Trace(
                        from coordinateName in russianWord
                        from unused in Expect(".")
                        select coordinateName,
                        "singleDisambiguatingCoordinate");

            var disambiguatingCoordinates = Tracer.Trace(
                        from unused1 in Expect("(")
                        from coordinates in Repeat(singleDisambiguatingCoordinate, Expect(","), atLeastOnce: true)
                        from unused2 in Expect(")")
                        select new StructuralEquatableArray<string>(coordinates) as IReadOnlyCollection<string>,
                        "disambiguatingCoordinates");

            var optionalAnnotations = Tracer.Trace(
                        Or<Tokens, (MayBe<GenerationHint> GenerationHint, MayBe<IReadOnlyCollection<string>> DisambiguatingCoordinates)>(
                            // generation hint, then disambiguator
                            from hint in generationHint
                            from coordinates in disambiguatingCoordinates
                            select (Some(hint), Some(coordinates)),
                            // disambiguator, then generation hint
                            from coordinates in disambiguatingCoordinates
                            from hint in generationHint
                            select (Some(hint), Some(coordinates)),
                            // just generation hint
                            from hint in generationHint
                            select (Some(hint), MakeNone<IReadOnlyCollection<string>>()),
                            // just disambiguator (if any)
                            from maybeCoordinates in Optional(disambiguatingCoordinates)
                            select maybeCoordinates
                                    .Map(coordinates => (MakeNone<GenerationHint>(), Some(coordinates)))
                                    .OrElse((MakeNone<GenerationHint>(), MakeNone<IReadOnlyCollection<string>>()))),
                        "optionalAnnotations");

            var markedupWord = Tracer.Trace(
                        from content in russianWord
                        from annotations in optionalAnnotations
                        select new MarkedupWord(
                            Content: content, 
                            IsFixed: false, 
                            GenerationHint: annotations.GenerationHint, 
                            DisambiguatingCoordinates: annotations.DisambiguatingCoordinates),
                        "markedupWord");

            var fixedWordsGroup = Tracer.Trace(
                        from unused1 in Expect("~")
                        from words in Repeat(markedupWord, atLeastOnce: true)
                        from unused2 in Expect("~")
                        select words.ConvertAll(w => w with { IsFixed = true }), 
                        "fixedWordsGroup");

            var markedupWords = Tracer.Trace(
                        from fixedWordsGroupOrWords in Repeat(Either(fixedWordsGroup, markedupWord), atLeastOnce: true)
                        select fixedWordsGroupOrWords
                            .SelectMany(it => it.Fold(
                                wordsGroup => wordsGroup,
                                singleWord => singleWord.ToImmutable()))
                            .AsImmutable(),
                        "markedupWords");

            return WholeInput(markedupWords);

            static bool IsSpecialChar(string text) => text.Length == 1 && text.Single().IsOneOf('~', '{', '}', '(', ')');
        }

        private static readonly Parser<Tokens, IReadOnlyCollection<MarkedupWord>> MarkupParser = BuildParser();
    }
}