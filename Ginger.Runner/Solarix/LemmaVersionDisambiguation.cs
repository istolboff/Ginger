using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner.Solarix
{
    using static Either;
    using static Impl;
    
    using WordOrQuotation = Either<Word, Quotation>;

    internal sealed record LemmaVersionDisambiguator(
        IReadOnlyCollection<LemmaVersion> LemmaVersions,
        bool IncludePartOfSpeech, 
        IReadOnlyDictionary<Type, PropertyInfo[]> Coordinates)
    {
        public IEnumerable<string> ProposeDisambiguations(IRussianLexicon russianLexicon) 
        =>
            from lemmaVersion in LemmaVersions
            let partOfSpeech = IncludePartOfSpeech && lemmaVersion.PartOfSpeech != null 
                                    ? russianLexicon.GetPartOfSpeechName(lemmaVersion.PartOfSpeech.Value) 
                                    : default(string)
            let relevantCoordinates = GetRelevantCoordinates(lemmaVersion.Characteristics, russianLexicon)
            select "(" + string.Join(',', new[] { partOfSpeech }.Concat(relevantCoordinates).Where(s => !string.IsNullOrEmpty(s))) + ")";

        public static LemmaVersionDisambiguator Create(
            IReadOnlyCollection<LemmaVersion> lemmaVersions)
        {
            ProgramLogic.Check(
                lemmaVersions.Count > 1,
                "Disambiguator is meaningful for at least 2 lemma versions.");

            var groupedByCharacteristicTypes = lemmaVersions
                                                .GroupBy(lv => lv.Characteristics.GetType())
                                                .ToDictionary(it => it.Key, it => it.AsImmutable());
            var coordinates = (from g in groupedByCharacteristicTypes
                              let lemmaVersionsOfGivenType = g.Value
                              where lemmaVersionsOfGivenType.Count > 1
                              let coordinateProperties = g.Key.GetProperties()
                              let coordinateDiversities = coordinateProperties
                                                            .Select(p => new 
                                                                { 
                                                                    Property = p,
                                                                    NumberOfDistinctValuesInLemmaVersions = lemmaVersionsOfGivenType
                                                                        .Select(lv => p.GetValue(lv.Characteristics))
                                                                        .Distinct()
                                                                        .Count()
                                                                })
                              let relevantCoordinateProperties = coordinateDiversities
                                                            .Where(it => it.NumberOfDistinctValuesInLemmaVersions > 1)
                                                            .OrderBy(it => it.NumberOfDistinctValuesInLemmaVersions)
                                                            .Select(it => it.Property)
                                                            .ToArray()
                              let singleDisambiguatingProperty = coordinateDiversities
                                                            .FirstOrDefault(it => it.NumberOfDistinctValuesInLemmaVersions == 
                                                                                  lemmaVersionsOfGivenType.Count)
                                                            ?.Property
                              select new 
                                    { 
                                        CharacteristicsType = g.Key, 
                                        DisambiguatingProperties = singleDisambiguatingProperty != null 
                                            ? new[] { singleDisambiguatingProperty }  
                                            : relevantCoordinateProperties
                                    })
                              .AsImmutable();

            return new(
                LemmaVersions: lemmaVersions,
                IncludePartOfSpeech: groupedByCharacteristicTypes.Count > 1, 
                Coordinates: coordinates.ToDictionary(it => it.CharacteristicsType, it => it.DisambiguatingProperties));
        }

        private IEnumerable<string> GetRelevantCoordinates(
            GrammarCharacteristics characteristics, 
            IRussianLexicon russianLexicon)
        =>
            from coordinateProperty in Coordinates[characteristics.GetType()]
            let boxedStateId = coordinateProperty.GetValue(characteristics)
            where boxedStateId != null
            select russianLexicon.GetStateName(
                Impl.CoordinateStateTypeToCoordinateIdMap[boxedStateId.GetType()], 
                (int)boxedStateId);
    }
    
    internal sealed record DisambiguatedPattern(
        string AnnotatedText, 
        string PlainText,
        IReadOnlyDictionary<string, LemmaVersionPicker> WordsInPatternToLemmaPickerMap)
    {
        public bool IsTrivial => !WordsInPatternToLemmaPickerMap.Any();

        public WordOrQuotation ApplyTo(WordOrQuotation wordOrQuotation) =>
            wordOrQuotation.Fold<WordOrQuotation>(
                word => Left(word with
                        {
                            LemmaVersions = 
                                word.LemmaVersions.Count > 1 && 
                                WordsInPatternToLemmaPickerMap.TryGetValue(word.Content, out var lemmaPicker)
                                    ? new[] 
                                        { 
                                            lemmaPicker
                                                .PickLemma(
                                                    word.LemmaVersions,
                                                    errorText => PatternBuilder.PatternBuildingException(
                                                        $"Could not find lemma version for the word '{word.Content}({word.PositionInSentence})' that would match " +
                                                        "to the disambiguation. " + 
                                                        errorText + 
                                                        " Only the following lemma versions are available: " + 
                                                        string.Join(";", word.LemmaVersions),
                                                        invalidOperation: true)) 
                                        }
                                    : word.LemmaVersions,
                            Children = word.Children.Select(ApplyTo).ToList()
                        }),
                quotation => Right(quotation with { Children = quotation.Children.Select(ApplyTo).ToList() }));

        public static DisambiguatedPattern Create(string annotatedText, IRussianLexicon russianLexicon)
        {
            var disambiguators = DisambiguatorRegex
                .Matches(annotatedText)
                .Select(m => new 
                            { 
                                Word = m.Groups[1].Value, 
                                Definition = LemmaVersionDisambiguatorDefinition.Create(m.Groups[2].Value, russianLexicon),
                                Location = (Start: m.Groups[2].Index, Length: m.Groups[2].Length)
                            })
                .AsImmutable();

            var plainText = disambiguators
                                .Reverse()
                                .Aggregate(
                                    annotatedText, 
                                    (text, it) => text.Remove(it.Location.Start, it.Location.Length));

            return new DisambiguatedPattern(
                annotatedText, 
                plainText, 
                disambiguators.ToDictionary(
                    d => d.Word,
                    d => d.Definition.BuildLemmaVersionPicker(),
                    RussianIgnoreCase));
        }

        private static readonly Regex DisambiguatorRegex = new (@"([а-яА-Я]+)\s*(\([а-я0-9]+\.(?:\s*,\s*[а-я0-9]+\.)*\))", RegexOptions.Compiled);
    }

    internal sealed record LemmaVersionDisambiguatorDefinition(
        string Definition, 
        IReadOnlyCollection<Type> CoordinateTypes, 
        IReadOnlyCollection<int> CoordinateEnumValues)
    {
        public LemmaVersionPicker BuildLemmaVersionPicker()
        {
            var propertyGettersMap = (
                        from it in Impl.GrammarCharacteristicsTypes
                        let properties = it.GetProperties()
                                        .Where(p => CoordinateTypes.Contains(p.PropertyType.RemoveNullability()))
                                        .OrderBy(p => CoordinateTypes.IndexOf(p.PropertyType.RemoveNullability()))
                                        .AsImmutable()
                        where properties.Count == CoordinateTypes.Count
                        select (GrammarCharacteristicsType: it, Properties: properties)
                        )
                        .ToDictionary(it => it.GrammarCharacteristicsType, it => it.Properties);
            
            return new (
                Definition,
                characteristics => 
                    propertyGettersMap.TryGetValue(characteristics.GetType(), out var properties) &&
                    CoordinateEnumValues.Zip(properties)
                        .All(it => StateIdsAreEqual(it.First, it.Second.GetValue(characteristics))));

            bool StateIdsAreEqual(int expected, object? actual) =>
                actual != null && expected == (int)actual;
        }

        public static LemmaVersionDisambiguatorDefinition Create(string definition, IRussianLexicon russianLexicon)
        {
            var coordinateValueNames = SplitDefinition(definition);
            var (coordinateTypes, coordinateValues) = russianLexicon.ResolveCoordinates(coordinateValueNames);
            return new (definition, coordinateTypes, coordinateValues);
        }

        public LemmaVersionDisambiguatorDefinition Remove(params Type[] coordinateTypesToRemove)
        {
            var indexesToRemove = coordinateTypesToRemove
                .Select(t => CoordinateTypes.IndexOf(t))
                .Where(i => i >= 0)
                .AsImmutable();

            return !indexesToRemove.Any()
                    ? this
                    : new LemmaVersionDisambiguatorDefinition(
                        $"({string.Join(".,", RemoveAtIndexes(SplitDefinition(Definition), indexesToRemove))}.)",
                        RemoveAtIndexes(CoordinateTypes.ToList(), indexesToRemove).AsImmutable(),
                        RemoveAtIndexes(CoordinateEnumValues.ToList(), indexesToRemove).AsImmutable());
        }

        private static IList<string> SplitDefinition(string definition) =>
            definition.Trim('(', ')').Split(',').Select(s => s.Trim(' ', '.')).ToList();

        private static IList<T> RemoveAtIndexes<T>(IList<T> list, IEnumerable<int> indexes)
        {
            foreach (var i in indexes.OrderByDescending(ind => ind))
            {
                list.RemoveAt(i);
            }

            return list;
        }
    }

    internal sealed record LemmaVersionPicker(string Definition, Func<GrammarCharacteristics, bool> CheckLemmaVersion)
    {
        public LemmaVersion PickLemma(
            IEnumerable<LemmaVersion> lemmaVersions,
            Func<string, Exception> reportError) =>
            lemmaVersions
                .Single(
                    lv => CheckLemmaVersion(lv.Characteristics),
                    first2MatchingLemmaVersions =>
                        first2MatchingLemmaVersions.Count switch
                        {
                            0 => reportError($"No lemma version matching {Definition} found."),
                            _ => reportError($"At least 2 lemma versions matching {Definition} found: " + 
                                             string.Join(",", first2MatchingLemmaVersions)),
                        });
    }
}