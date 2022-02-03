using System;
using System.Collections.Generic;
using System.Linq;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;
using Prolog.Engine.Parsing;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using static Either;
    using static MonadicParsing;
    using static PrologParser;
    using static TextParsingPrimitives;

    // Пример: @имярек(поверхность, Сфера)
    internal sealed record CompoundWord(string ReplacementWord, IReadOnlyCollection<string> ActualWords)
    {
        public string GetActualWords() => 
            string.Join(string.Empty, ActualWords);

        public override string ToString() =>
            $"@{ReplacementWord}({string.Join(", ", ActualWords)})";
    }

    internal sealed record IntermediateMeaningSentence(IReadOnlyCollection<Either<CompoundWord, string>> Parts)
    {
        public IntermediateMeaningSentence Nested() =>
            new (
                Right<CompoundWord, string>("(").ToImmutable()
                .Concat(Parts)
                .Concat(Right<CompoundWord, string>(")").ToImmutable())
                .AsImmutable());

        public ParsedSentence ToParsedSentence(IRussianGrammarParser grammarParser) =>
            grammarParser.ParsePreservingQuotes(
                string.Join(
                    " ", 
                    Parts.Select(it => it.Fold(cw => cw.ReplacementWord, s => s))));

        public MeaningBuilder Enrich(MeaningBuilder originalMeaningBuilder) =>
            originalMeaningBuilder with
            {
                BuildName = (meaningBuilder, nameRecipe, parsedSentence) =>
                    originalMeaningBuilder.BuildName(
                        meaningBuilder,
                        nameRecipe with
                        {
                            NameComponentGetters = nameRecipe.NameComponentGetters.ConvertAll(
                                it => it with 
                                    { 
                                        ComponentGetter = parsedSentence1 =>
                                        {
                                            var result = it.ComponentGetter(parsedSentence1);
                                            return Parts
                                                    .TryFirst(p => p.IsLeft && p.Left!.ReplacementWord == result)
                                                    .Map(cw => cw.Left!.GetActualWords())
                                                    .OrElse(result);
                                        }
                                    })
                        },
                        parsedSentence)
            };

        public override string ToString() =>
            string
                .Join(" ", Parts.Select(it => it.Fold(cw => cw.ToString(), s => s)))
                .Replace("  ", " ");
    }

    internal readonly record struct ParsedGenerativePattern(
        string PatternId, 
        string PatternText, 
        Either<SentenceMeaning, IntermediateMeaningSentence> Meaning);

    internal static class GenerativePatternParser
    {
        public static IEnumerable<ParsedGenerativePattern> ParsePatterns(TextInput rulesText)
        {
            const string PatternIdPrefix = "pattern-";

            var patternId = Tracer.Trace(
                from unused in ReadTill(PatternIdPrefix)
                from id in ReadTill(":")
                select id,
                "patternId");

            var compoundWord = Tracer.Trace( 
                from unused in Expect('@')
                from replacementWord in ReadTill("(")
                from parts in Repeat(
                                Repeat(Expect(ch => !ch.IsOneOf(',', ')')), atLeastOnce: true), 
                                Lexem(","), 
                                atLeastOnce: true)
                from unused1 in Lexem(")")
                select new CompoundWord(
                            replacementWord.Trim(), 
                            parts.ConvertAll(p => string.Join(string.Empty, p).Trim())),
                "compoundWord");

            Parser<TextInput, IntermediateMeaningSentence>? textWithBalancedParenthesis = null;
            textWithBalancedParenthesis = Tracer.Trace(
                Repeat(
                    Or(
                        Either(
                            compoundWord,
                            from letters in Repeat(Expect(ch => !ch.IsOneOf('(', ')', '@')), atLeastOnce: true)
                            select string.Join(string.Empty, letters))
                        .Select(it => new IntermediateMeaningSentence(it.ToImmutable())),
                        from unused1 in Lexem("(")
                        from nestedText in textWithBalancedParenthesis!
                        from unused2 in Lexem(")")
                        select nestedText.Nested()))
                .Select(parts => new IntermediateMeaningSentence(parts.SelectMany(it => it.Parts).AsImmutable())),
                "textWithBalancedParenthesis");

            var indirectMeaning = Tracer.Trace(
                    from unused in Lexem(MetaUnderstand.Name).Then(Lexem("("))
                    from sentenceInNaturalLanguage in textWithBalancedParenthesis
                    from unused1 in Lexem(")")
                    select sentenceInNaturalLanguage,
                    "indirectMeaning");

            var meaning = Tracer.Trace(
                Either(
                    Either(
                        PrologParsers.ProgramParser.Where(rules => rules.Any()),
                        PrologParsers.PremisesGroupParser.Where(complexTerms => complexTerms.Any())),
                    indirectMeaning),
                "meaning");

            var patternText = Tracer.Trace(
                ReadTill("::="), 
                "patternText");

            var singlePattern = Tracer.Trace(
                from id in patternId
                from generativePattern in patternText
                from generativeMeaning in meaning
                select new ParsedGenerativePattern(id, generativePattern, generativeMeaning),
                "singlePattern");

            var patterns = Tracer.Trace(Repeat(singlePattern), "patterns");

            return WholeInput(patterns)
                    .Invoke(rulesText)
                    .Fold(
                        parsingError => throw ParsingError(
                            $"{parsingError.Text} at {parsingError.Location.Position} of the following input: " + 
                            Environment.NewLine + parsingError.Location),
                        result => result.Value);
        }
   }
}