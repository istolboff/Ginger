using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using Prolog.Engine.Miscellaneous;
using Ginger.Runner.Solarix;

namespace Ginger.Runner
{
    internal sealed record WordOrQuotation<TWord>(
        string Content,
        MayBe<TWord> Word, // None in case of Quotation -- see RussianGrammarTreatingQuotedSequencesAsSingleSentenceElement.cs
        IReadOnlyCollection<WordOrQuotation<TWord>> Children,
        int PositionInSentence)
    {
        public bool IsQuote => !Word.HasValue;

        public IEnumerable<WordOrQuotation<TWord>> IterateDepthFirst() =>
            this.ToImmutable().Concat(Children.SelectMany(it => it.IterateDepthFirst()));

        public IEnumerable<WordOrQuotation<TWord>> IterateByPosition() =>
            IterateDepthFirst().OrderBy(it => it.PositionInSentence);

        public IEnumerable<TWord> IterateWordsDepthFirst() =>
            IterateDepthFirst().Where(it => it.Word.HasValue).Select(it => it.Word.Value!);

        public WordOrQuotation<TMappedWord> Map<TMappedWord>(Func<WordOrQuotation<TWord>, TWord, TMappedWord> f) =>
            new (
                Content,
                Word.Map(w => f(this, w)),
                Children.ConvertAll(child => child.Map(f)),
                PositionInSentence);

        public WordOrQuotation<TMappedWord> Map<TMappedWord>(Func<TWord, TMappedWord> f) =>
            Map((_, w) => f(w));
    }

    internal sealed record Word(IReadOnlyCollection<LemmaVersion> LemmaVersions, LinkType? LeafLinkType);

    internal sealed record ParsedSentence(string Sentence, WordOrQuotation<Word> SentenceStructure)
    {
        public LemmaVersion GetRelevantLemmaAt(
            int positionInSentence,
            LemmaVersion lemmaVersion,
            Func<string, Exception> reportException) 
        =>
            SentenceStructure
                .IterateDepthFirst()
                .Single(
                    it => it.Word.HasValue && it.PositionInSentence == positionInSentence,
                    _ => reportException(
                            $"{Sentence} does not have a word at position={positionInSentence}. " + 
                            "We expected to see word at this position, but it's either quotation, or there's no element at this index at all."))
                .Word.Value!.LemmaVersions
                .FindRelevantLemma(lemmaVersion)
            .OrElse(() => throw reportException(
                            $"Could not find {lemmaVersion.PartOfSpeech} lemma version " +
                            $"of type {lemmaVersion.Characteristics.GetType().Name} " +
                            $"position={positionInSentence}  in sentence {Sentence}."));

        public string GetQuotationAt(
            int positionInSentence,
            Func<string, Exception> reportException)
        =>
            SentenceStructure
                .IterateDepthFirst()
                .Single(
                    it => !it.Word.HasValue && it.PositionInSentence == positionInSentence,
                    _ => reportException(
                            $"{Sentence} does not have a quotation at position={positionInSentence}. " + 
                            "We expected to see quotation at this position, but it's either word, or there's no element at this index at all."))
                .Content;
    }

    internal sealed record AnnotatedWord(
        string Content,
        IReadOnlyCollection<LemmaVersion> LemmaVersions, 
        LinkType? LeafLinkType,
        WordAnnotations Annotations)
    {
        public LemmaVersion GetDisambiguatedLemmaVersion(
            IRussianLexicon russianLexicon, 
            bool enforceLemmaVersions,
            Func<string, Exception> reportError) => 
            Annotations.LemmaVersionPicker
                .Map(picker => LemmaVersions
                        .TrySingle(
                            lv => picker.CheckLemmaVersion(lv.Characteristics),
                            matchingLemmaVersions => throw reportError(
                                $"Disambiguator ({picker.Definition}) could not pick a single lemma version for '{Content}'. " + 
                                "The following lemma versions all match: " + string.Join(";", matchingLemmaVersions) +
                                ". In order to fully disambiguate, please use one of the following: " + Environment.NewLine +
                                string.Join(Environment.NewLine, AnnotatedSentence.BuildDisambiguatingAnnotations(LemmaVersions, russianLexicon))))
                        .OrElse(() => 
                            enforceLemmaVersions
                                ? picker
                                    .TryProduceLemmaVersion(LemmaVersions)
                                    .OrElse(() => throw reportError(
                                        $"Could not produce LemmaVersion for ({picker.Definition}) from these lemma versions: " +
                                        string.Join(";", LemmaVersions)))
                                : throw reportError(
                                    $"Disambiguator ({picker.Definition}) didn't match any of possible lemma version alternatives: " + 
                                    string.Join(";", LemmaVersions))))
                .OrElse(() => LemmaVersions.Single(
                            _ => true, 
                            _ => throw reportError(
                                    $"Missing disambiguation for {Content}: there are these lemma versions: {string.Join(";", LemmaVersions)} " + 
                                    " and they can be fully disambiguated with one of the following:" + Environment.NewLine +
                                    string.Join(Environment.NewLine, AnnotatedSentence.BuildDisambiguatingAnnotations(LemmaVersions, russianLexicon)) + 
                                    ", but no disambiguation has been specified.")));

        public LemmaVersion GetSingleLemmaVersion(
            Func<IReadOnlyCollection<LemmaVersion>, Exception> reportMultipleLemmaVersionsError,
            bool ignoreGender = false) =>
            LemmaVersions
                .Single(
                    lv => !ignoreGender || 
                          LemmaVersions.Count == 1 || 
                          (lv.Characteristics.TryGetGender() ?? Gender.Мужской) == Gender.Мужской,
                    reportMultipleLemmaVersionsError);

        public override string ToString() =>
            Annotations.Quote(Content);
    }

    internal sealed record AnnotatedSentence(string Sentence, WordOrQuotation<AnnotatedWord> SentenceStructure)
    {
        public AnnotatedSentence Transform(
            Func<AnnotatedWord, string> mapWord, 
            IRussianGrammarParser grammarParser, 
            IRussianLexicon russianLexicon) 
        =>
            grammarParser.ParseAnnotatedPreservingQuotes(
                string.Join(
                    " ", 
                    SentenceStructure
                        .IterateByPosition()
                        .Select(it => it.Word.Fold(mapWord, () => $"'{it.Content}'"))),
                russianLexicon);

        public DisambiguatedSentence Disambiguate(IRussianLexicon russianLexicon, bool enforceLemmaVersions = false) =>
             new (
                Normalize(SentenceStructure), 
                Sentence, 
                SentenceStructure.Map(word => 
                {
                    if (word.LemmaVersions.HasMoreThanOneElement() && !word.Annotations.LemmaVersionPicker.HasValue)
                    {
                        var annotationVariants = string.Join(";", BuildDisambiguatingAnnotations(word.LemmaVersions, russianLexicon));
                        throw new InvalidOperationException(
                                $"The word '{word.Content}' in pattern '{Sentence}' has the following lemma versions: " +
                                string.Join("; ", word.LemmaVersions) +
                                $". You can annotate the word with one of the following variants {annotationVariants} in order to disambiguate it, " + 
                                "or reformulate the pattern wording.");
                    }

                    return new DisambiguatedWord(
                                word.GetDisambiguatedLemmaVersion(
                                    russianLexicon,
                                    enforceLemmaVersions,
                                    message => new InvalidOperationException($"Sentence '{Sentence}': {message}")),
                                word.Annotations.IsFixed,
                                word.LeafLinkType);
                }));

        public static string BuildDisambiguatingAnnotations(
            IReadOnlyCollection<LemmaVersion> lemmaVersions,
            IRussianLexicon russianLexicon) =>
            string.Join(
                ", ", 
                LemmaVersionDisambiguator.Create(lemmaVersions)
                    .ProposeDisambiguations(russianLexicon));

        private static string Normalize(WordOrQuotation<AnnotatedWord> sentenceStructure) =>
            TextManipulation.Join(
                (_, it) => it.All(char.IsPunctuation) ? string.Empty : " ", 
                sentenceStructure
                    .IterateByPosition()
                    .Select(it => it.Word.Fold(w => w.Annotations.IsFixed ? $"~{w.Content}~": w.Content, () => $"'{it.Content}'")));
    }

    internal sealed record WordAnnotations(
        bool IsFixed, 
        MayBe<GenerationHint> GenerationHint, 
        MayBe<LemmaVersionPicker> LemmaVersionPicker)
    {
        public string Quote(string wordContent)
        {
            var result = new StringBuilder();
            if (IsFixed)
            {
                result.Append('~');
            }

            result.Append(wordContent);

            if (GenerationHint.HasValue)
            {
                result.Append(GenerationHint.Value.Quote());
            }

            if (LemmaVersionPicker.HasValue)
            {
                result.Append('(');
                result.Append(LemmaVersionPicker.Value!.Definition);
                result.Append(')');
            }

            if (IsFixed)
            {
                result.Append('~');
            }

            return result.ToString();
        }
    }

    internal sealed record LemmaVersionPicker(
        IReadOnlyCollection<CoordinateValue> ExpectedCoordinates,
        IReadOnlyDictionary<Type, IReadOnlyCollection<PropertyInfo>> RelevantGrammarCharacteristicsPropertyGetters)
    {
        public string Definition => string.Join(".,", ExpectedCoordinates.Select(c => c.ValueName)) + ".";
            
        public bool CheckLemmaVersion(GrammarCharacteristics characteristics)
        {
            return RelevantGrammarCharacteristicsPropertyGetters.TryGetValue(characteristics.GetType(), out var properties) &&
                    ExpectedCoordinates.Zip(properties)
                        .All(it => StateIdsAreEqual(it.First.EnumValue, it.Second.GetValue(characteristics)));

            bool StateIdsAreEqual(int expected, object? actual) =>
                actual != null && expected == (int)actual;
        }

        public LemmaVersionPicker ForPluralForm() =>
            new (
                ExpectedCoordinates.Where(c => !c.CoordinateType.IsOneOf(typeof(Number), typeof(Gender))).AsImmutable(),
                RelevantGrammarCharacteristicsPropertyGetters);

        public MayBe<LemmaVersion> TryProduceLemmaVersion(IEnumerable<LemmaVersion> lemmaVersions)
        {
            var coordinateTypes = ExpectedCoordinates.Select(c => c.CoordinateType).AsImmutable();
            return lemmaVersions
                        .TryFirst(lv => coordinateTypes.IsSubsetOf(lv.Characteristics.GetCoordinateTypes()))
                        .Map(lv => lv with { Characteristics = lv.Characteristics.With(ExpectedCoordinates) });
        }   

        public override string ToString() => $"({Definition})";

        public static LemmaVersionPicker Create(IReadOnlyCollection<CoordinateValue> expectedCoordinates)
        {
            var propertyGettersMap = (
                        from it in Impl.GrammarCharacteristicsTypes
                        let properties = it.GetProperties()
                                        .Where(p => expectedCoordinates.Any(c => c.CoordinateType == p.PropertyType.RemoveNullability()))
                                        .OrderBy(p => expectedCoordinates.FindIndex(c => c.CoordinateType == p.PropertyType.RemoveNullability()))
                                        .AsImmutable()
                        where properties.Count == expectedCoordinates.Count
                        select (GrammarCharacteristicsType: it, Properties: properties)
                        )
                        .ToDictionary(it => it.GrammarCharacteristicsType, it => it.Properties);

            return new (expectedCoordinates, propertyGettersMap);
        }
    }
    
    internal sealed record DisambiguatedWord(
        LemmaVersion LemmaVersion, 
        bool IsFixed,
        LinkType? LeafLinkType);

    internal sealed record DisambiguatedSentence(string Sentence, string AnnotatedSentence, WordOrQuotation<DisambiguatedWord> SentenceStructure)
    {
        public ParsedSentence AsParsedSentence() =>
            new (Sentence, SentenceStructure.Map(word => new Word(word.LemmaVersion.ToImmutable(), word.LeafLinkType)));
    }

    internal static class NaturalLanguageSentenceDomain
    {
        public static string Quote(this GenerationHint @this) =>
            @this switch
            {
                GenerationHint.Replicatable => "{и проч.}",
                GenerationHint.PluralitySensitive => "{мн.}",
                _ => throw ProgramLogic.Error($"Do not know how to handle {@this}")
            };

        public static MayBe<LemmaVersion> FindRelevantLemma(
            this IEnumerable<LemmaVersion> lemmaVersions,
            LemmaVersion targetLemmaVersion) =>
                lemmaVersions.TryFirst(lm => 
                                targetLemmaVersion.PartOfSpeech == lm.PartOfSpeech &&
                                targetLemmaVersion.Characteristics.CompatibleTo(lm.Characteristics));
    }
}