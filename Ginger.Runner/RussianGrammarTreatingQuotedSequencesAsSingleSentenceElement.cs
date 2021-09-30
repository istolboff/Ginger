using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    using static MayBe;

    internal static class RussianGrammarTreatingQuotedSequencesAsSingleSentenceElement
    {
        public static ParsedSentence ParsePreservingQuotes(
            this IRussianGrammarParser @this, 
            string sentence)
        {
            var quotedSequences = LocateQuotedSequences(sentence);
            var quotationSubstitutions = MakeSubstitutionsOfQuotedSequencesWithSingleWords(quotedSequences);
            var textWithSubstitutedQuotes = ApplySubstitutions(quotationSubstitutions, sentence);
            var parsedText = WorkaroundForSituationsWhenParserRetunsFlatChainOfElements(@this, textWithSubstitutedQuotes);
            var sentenceStructure = RestoreQuotations(quotationSubstitutions, parsedText).Single(
                                _ => true, 
                                _ => new InvalidOperationException(
                                        $"Text '{textWithSubstitutedQuotes}' could not be tokenized unambiguously. Please reformulate it."));
            return new (sentence, sentenceStructure);
        }

        public static AnnotatedSentence ParseAnnotatedPreservingQuotes(
            this IRussianGrammarParser @this, 
            string annotatedText,
            IRussianLexicon russianLexicon)
        {
            var quotedSequences = LocateQuotedSequences(annotatedText);
            var quotationSubstitutions = MakeSubstitutionsOfQuotedSequencesWithSingleWords(quotedSequences);
            var textWithSubstitutedQuotes = ApplySubstitutions(quotationSubstitutions, annotatedText);
            var markedupWords = @this.ParseMarkup(textWithSubstitutedQuotes).ToArray();
            var sentenceWithoutMarkup = string.Join(" ", markedupWords.Select(w => w.Content));
            var parsedText = WorkaroundForSituationsWhenParserRetunsFlatChainOfElements(@this, sentenceWithoutMarkup);
            var sentenceStructure = RestoreQuotations(quotationSubstitutions, parsedText).Single(
                                _ => true, 
                                _ => new InvalidOperationException(
                                        $"Text '{textWithSubstitutedQuotes}' could not be tokenized unambiguously. Please reformulate it."));

            ProgramLogic.Check(
                (
                    markedupWords.Select(w => w.Content).AsImmutable(),
                    IterateDepthFirst(parsedText)
                        .OrderBy(it => it.PositionInSentence)
                        .Select(it => it.Content)
                        .AsImmutable()
                ) switch
                {
                    var (markups, grammars) when !markups.SequenceEqual(grammars, Impl.RussianIgnoreCase) => 
                        Some(
                            "Parsing marked-up sentence gave the result that is incompatible with " + 
                            "the structure returned by russian grammmar parser." + Environment.NewLine +
                            "Markup parsing result: " + string.Join(";" , markups) + Environment.NewLine +
                            "Russian grammar parsing result: " + string.Join(";", grammars)),
                    _ => None
                });

            return new (annotatedText, sentenceStructure.Map((wordOrQuotation, word) => new AnnotatedWord(
                                    wordOrQuotation.Content, 
                                    word.LemmaVersions,
                                    word.LeafLinkType,
                                    MakeAnnotations(markedupWords[wordOrQuotation.PositionInSentence]))));

            WordAnnotations MakeAnnotations(MarkedupWord word) =>
                new (
                    word.IsFixed, 
                    word.GenerationHint, 
                    word.DisambiguatingCoordinates.Map(
                        coordinateValueNames => LemmaVersionPicker.Create(
                            russianLexicon.ResolveCoordinates(coordinateValueNames))));

            static IEnumerable<SentenceElement> IterateDepthFirst(IEnumerable<SentenceElement> sentenceElements)
            {
                foreach (var it in sentenceElements)
                {
                    yield return it;
                    foreach (var child in IterateDepthFirst(it.Children))
                    {
                        yield return child;
                    }
                }
            }
        }

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

        private static IReadOnlyList<WordOrQuotation<Word>> RestoreQuotations(
            IReadOnlyDictionary<string, string> quotationSubstitutions, 
            IEnumerable<SentenceElement> parsedText) 
        =>
            (from sentenceElement in parsedText
            let children = RestoreQuotations(quotationSubstitutions, sentenceElement.Children)
            select quotationSubstitutions
                    .TryFind(sentenceElement.Content)
                    .Map(
                        substitution => new WordOrQuotation<Word>(
                                substitution, 
                                MakeNone<Word>(),
                                children, 
                                sentenceElement.PositionInSentence))
                    .OrElse(
                        () => new WordOrQuotation<Word>(
                                sentenceElement.Content, 
                                Some(new Word(
                                    sentenceElement.LemmaVersions, 
                                    sentenceElement.LeafLinkType)),
                                children, 
                                sentenceElement.PositionInSentence))
            ).ToList();

        private static IReadOnlyCollection<SentenceElement> WorkaroundForSituationsWhenParserRetunsFlatChainOfElements(
            IRussianGrammarParser parser, 
            string text)
        => parser.Parse(text) switch 
            {
                var elements when elements.HasMoreThanOneElement() && elements.All(e => !e.Children.Any()) => 
                    (elements.First() with { Children = elements.Skip(1).AsImmutable() }).ToImmutable(),
                var elements => elements
            };

        private static readonly Regex QuotationRecognizer = new (@"'([^']+)'", RegexOptions.Compiled);

        private static readonly string[] StockWordsForQuotationSubstitution = new[] 
        {
            "Амбивалентность", "Апперцепция", "Благо", "Герменевтика", "Детерминизм",
            "Индивидуализм", "Императив", "Либерализм", "Мистицизм", "Нигилизм", "Обскурантизм",
            "Патристика", "Плюрализм", "Схоластика", "Телеология", "Фатализм"
        };
    }
}