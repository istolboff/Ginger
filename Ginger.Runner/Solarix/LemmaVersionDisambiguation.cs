using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner.Solarix
{
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
                "A disambiguator is meaningful for at least 2 lemma versions.");

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
}