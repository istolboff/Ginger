using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using WordOrQuotation = Either<Word, Quotation>;

    using static Either;
    
    internal sealed record Word(
        string Content, 
        IReadOnlyCollection<LemmaVersion> LemmaVersions, 
        IReadOnlyList<WordOrQuotation> Children, 
        LinkType? LeafLinkType,
        int PositionInSentence);

    internal sealed record Quotation(string Content, IReadOnlyList<WordOrQuotation> Children);

    internal sealed record ParsedSentence(string Sentence, WordOrQuotation SentenceSyntax)
    {
        public ParsedSentence ApplyDisambiguator(DisambiguatedPattern disambiguatedPattern) =>
            disambiguatedPattern.IsTrivial
                ? this
                : this with { SentenceSyntax = disambiguatedPattern.ApplyTo(SentenceSyntax) };
    }

    internal static class WordOrQuotationExtensions
    {
        public static Word LocateWord(
            this Word @this,
            string elementContent,
            Func<string, Exception> reportError) 
        =>
            @this
                .IterateWordsDepthFirst()
                .Single(
                    word => elementContent.Equals(word.Content, StringComparison.OrdinalIgnoreCase),
                    first2MatchingWordsOrQuotes =>
                        first2MatchingWordsOrQuotes.Count switch
                        {
                            0 => reportError($"Could not find word '{elementContent}'"),
                            _ => reportError($"There are several words '{elementContent}'")
                        });

        public static Word LocateWord(this WordOrQuotation @this, int wordPositionInSentence) =>
            @this
                .IterateWordsDepthFirst()
                .Single(w => w.PositionInSentence == wordPositionInSentence);

        public static IEnumerable<Word> IterateWordsDepthFirst(this WordOrQuotation @this) =>
            @this
                .IterateDepthFirst()
                .Where(woq => woq.IsLeft)
                .Select(woq => woq.Left!);

        private static IEnumerable<Word> IterateWordsDepthFirst(this Word @this) =>
            Left<Word, Quotation>(@this).IterateWordsDepthFirst();

        private static IEnumerable<WordOrQuotation> IterateDepthFirst(this WordOrQuotation @this) 
        =>
            new[] { @this }.Concat(
                @this.Fold(
                    word => word.Children.SelectMany(IterateDepthFirst),
                    _ => Enumerable.Empty<WordOrQuotation>()));
    }
    
    internal static class RussianGrammarTreatingQuotedSequencesAsSingleSentenceElement
    {
        public static ParsedSentence ParsePreservingQuotes(
            this IRussianGrammarParser @this, 
            string text)
        {
            var quotedSequences = LocateQuotedSequences(text);
            var quotationSubstitutions = MakeSubstitutionsOfQuotedSequencesWithSingleWords(quotedSequences);
            var textWithSubstitutedQuotes = ApplySubstitutions(quotationSubstitutions, text);
            var parsedText = @this.Parse(textWithSubstitutedQuotes);
            var textSyntax = RestoreQuotations(quotationSubstitutions, parsedText).Single(
                                _ => true, 
                                _ => new InvalidOperationException(
                                        $"Text '{textWithSubstitutedQuotes}' could not be tokenized unambiguously. Please reformulate it."));
            return new (text, textSyntax);
        }

        public static ParsedSentence ParsePreservingQuotes(
            this IRussianGrammarParser @this, 
            DisambiguatedPattern disambiguatedPattern)
        =>
            @this
                .ParsePreservingQuotes(disambiguatedPattern.PlainText)
                .ApplyDisambiguator(disambiguatedPattern);

        private static IEnumerable<string> LocateQuotedSequences(string text) =>
            QuotationRecognizer.Matches(text).Select(m => m.Groups[1].Value).AsImmutable();

        private static IReadOnlyDictionary<string, string> MakeSubstitutionsOfQuotedSequencesWithSingleWords(
            IEnumerable<string> quotedSequences) =>
            quotedSequences
                .Select((quotation, index) => (quotation, index))
                .ToDictionary(
                    it => it.index < StockWordsForQuotationSubstitution.Length 
                        ? StockWordsForQuotationSubstitution[it.index]
                        : throw new NotSupportedException($"Sentences with more than {StockWordsForQuotationSubstitution.Length} quotations are not supported."),
                    it => it.quotation);

        private static string ApplySubstitutions(
            IReadOnlyDictionary<string, string> quotationSubstitutions,
            string text)
        {
            var result = text;
            foreach (var (toWhat, fromWhat) in quotationSubstitutions)
            {
                result = result.Replace($"'{fromWhat}'", toWhat);
            }

            return result;
        }

        private static IReadOnlyList<WordOrQuotation> RestoreQuotations(
            IReadOnlyDictionary<string, string> quotationSubstitutions, 
            IEnumerable<SentenceElement> parsedText) 
        =>
            (from sentenceElement in parsedText
            let children = RestoreQuotations(quotationSubstitutions, sentenceElement.Children)
            select quotationSubstitutions
                    .TryFind(sentenceElement.Content)
                    .Fold(
                        substitution => Right<Word, Quotation>(new Quotation(substitution, children)),
                        () => Left<Word, Quotation>(
                            new Word(
                                sentenceElement.Content, 
                                sentenceElement.LemmaVersions, 
                                children, 
                                sentenceElement.LeafLinkType,
                                sentenceElement.PositionInSentence)))
            ).ToList();

        private static readonly Regex QuotationRecognizer = new (@"'([^']+)'", RegexOptions.Compiled);

        private static readonly string[] StockWordsForQuotationSubstitution = new[] 
        {
            "Амбивалентность", "Апперцепция", "Благо", "Герменевтика", "Детерминизм",
            "Индивидуализм", "Императив", "Либерализм", "Мистицизм", "Нигилизм", "Обскурантизм",
            "Патристика", "Плюрализм", "Схоластика", "Телеология", "Фатализм"
        };
    }
}