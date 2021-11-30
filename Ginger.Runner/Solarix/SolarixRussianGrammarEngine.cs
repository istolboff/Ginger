using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using Prolog.Engine.Miscellaneous;
using SolarixGrammarEngineNET;

namespace Ginger.Runner.Solarix
{
    using static MayBe;
    using static MakeCompilerHappy;
    using static Impl;

    internal sealed class SolarixRussianGrammarEngine : IRussianGrammarParser, IRussianLexicon
    {
        public SolarixRussianGrammarEngine()
        {
            _engineHandle = new DisposableIntPtr(
                GrammarEngine.sol_CreateGrammarEngineW(null), 
                handle => SuppressCa1806(GrammarEngine.sol_DeleteGrammarEngine(handle)));

            var loadStatus = GrammarEngine.sol_LoadDictionaryExW(
                    _engineHandle,
                    DefaultSolarixDictionaryXmlPath,
                    GrammarEngine.EngineInstanceFlags.SOL_GREN_LAZY_LEXICON);

            if (loadStatus != 1)
            {
                throw new InvalidOperationException($"Could not load Dictionary from {DefaultSolarixDictionaryXmlPath}. {DescribeError()}");
            }

            _grammarCharacteristicsBuilders =
                new Dictionary<PartOfSpeech, Func<IntPtr, int, GrammarCharacteristics>>
                {
                    {
                        PartOfSpeech.Существительное,
                        (hNode, versionIndex) =>
                            TryGetInfinitive(hNode)
                            .Fold(
                                infinitive => new VerbalNounCharacteristics(
                                    GetNodeVersionCoordinateState<Case>(hNode, versionIndex),
                                    GetNodeVersionCoordinateState<Number>(hNode, versionIndex),
                                    GetNodeVersionCoordinateState<Gender>(hNode, versionIndex),
                                    infinitive),
                                () => new NounCharacteristics(
                                    GetNodeVersionCoordinateState<Case>(hNode, versionIndex),
                                    GetNodeVersionCoordinateState<Number>(hNode, versionIndex),
                                    GetNodeVersionCoordinateState<Gender>(hNode, versionIndex)))
                    },
                    {
                        PartOfSpeech.Глагол,
                        (hNode, versionIndex) =>
                            new VerbCharacteristics(
                                TryGetNodeVersionCoordinateState<Case>(hNode, versionIndex),
                                GetNodeVersionCoordinateState<Number>(hNode, versionIndex),
                                GetNodeVersionCoordinateState<VerbForm>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Person>(hNode, versionIndex),
                                GetNodeVersionCoordinateState<VerbAspect>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Tense>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Transitiveness>(hNode, versionIndex))
                    },
                    {
                        PartOfSpeech.Прилагательное,
                        (hNode, versionIndex) =>
                            new AdjectiveCharacteristics(
                                TryGetNodeVersionCoordinateState<Case>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Number>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Gender>(hNode, versionIndex),
                                GetBoolCoordinateState(hNode, versionIndex, GrammarEngineAPI.SHORTNESS_ru, AdjectiveForm.Краткое, AdjectiveForm.Полное),
                                GetNodeVersionCoordinateState<ComparisonForm>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Animacy>(hNode, versionIndex))
                    },
                    {
                        PartOfSpeech.Наречие,
                        (hNode, versionIndex) =>
                            new AdverbCharacteristics(GetNodeVersionCoordinateState<ComparisonForm>(hNode, versionIndex))
                    },
                    {
                        PartOfSpeech.Деепричастие,
                        (hNode, versionIndex) =>
                            new GerundCharacteristics(
                                GetNodeVersionCoordinateState<Case>(hNode, versionIndex),
                                GetNodeVersionCoordinateState<VerbAspect>(hNode, versionIndex))
                    },
                    {
                        PartOfSpeech.Местоимение,
                        (hNode, versionIndex) =>
                            new PronounCharacteristics(
                                GetNodeVersionCoordinateState<Case>(hNode, versionIndex),
                                TryGetNodeVersionCoordinateState<Gender>(hNode, versionIndex),
                                GetNodeVersionCoordinateState<Number>(hNode, versionIndex),
                                GetNodeVersionCoordinateState<Person>(hNode, versionIndex))
                    },
                    {
                        PartOfSpeech.Инфинитив,
                        (hNode, versionIndex) =>
                            GetNodeVersionCoordinateState<VerbAspect>(hNode, versionIndex).Apply(
                                verbAspect => 
                                    new InfinitiveCharacteristics(
                                        GetNodeVersionCoordinateState<VerbAspect>(hNode, versionIndex),
                                        GetNodeVersionCoordinateState<Transitiveness>(hNode, versionIndex),
                                        verbAspect == VerbAspect.Совершенный 
                                            ? GrammarEngine.sol_GetNodeContentsFX(hNode)
                                            : GetPerfectFormOfInfinitive(hNode).OrElse(string.Empty)))
                    }
                };

                _knownCoordStateNames = (
                    from it in CoordinateStateTypeToCoordinateIdMap
                    from attributeId in Enum.GetValues(it.Key).Cast<int>()
                    let coordStateName = GetCoordStateName(_engineHandle, it.Value, attributeId)
                    where !string.IsNullOrEmpty(coordStateName)
                    select (coordStateName, CoordType: it.Key, StateId: attributeId)
                ).ToDictionary(it => it.coordStateName, it => (it.CoordType, it.StateId), RussianIgnoreCase);

                static string GetCoordStateName(IntPtr engineHandle, int categoryId, int attrId)
                {
                    var buffer = CreateBuffer(engineHandle);
                    SuppressCa1806(GrammarEngine.sol_GetCoordStateName(engineHandle, categoryId, attrId, buffer));
                    return buffer.ToString();
                }
        }

        public IReadOnlyCollection<SentenceElement> Parse(string text)
        {
            var hPack = IntPtr.Zero;
            try
            {
                hPack = GrammarEngine.sol_SyntaxAnalysis(
                    _engineHandle,
                    text,
                    GrammarEngine.MorphologyFlags.SOL_GREN_MODEL,
                    GrammarEngine.SyntaxFlags.DEFAULT,
                    (20 << 22) | 30000,
                    GrammarEngineAPI.RUSSIAN_LANGUAGE);

                if (hPack == IntPtr.Zero)
                {
                    throw new InvalidOperationException($"Could parse text: {text}. {DescribeError()}");
                }

                var ngrafs = GrammarEngine.sol_CountGrafs(hPack);
                if (ngrafs <= 0)
                {
                    throw new InvalidOperationException($"No graphs were parsed from the text: {text}.");
                }

                return Enumerable.Range(1, GrammarEngine.sol_CountRoots(hPack, 0) - 2)
                    .Select(i => CreateSentenceElement(GrammarEngine.sol_GetRoot(hPack, 0, i)))
                    .AsImmutable();
            }
            finally
            {
                GrammarEngine.sol_DeleteResPack(hPack);
            }
        }

        public string[] Tokenize(string text) =>
            WorkaroundForBugWhenSometimesTokenizationDoesNotSplitWords(
                GrammarEngine.sol_TokenizeFX(_engineHandle, text, GrammarEngineAPI.RUSSIAN_LANGUAGE));

        public string GetNeutralForm(string word)
        {
            var hProjs = GrammarEngine.sol_ProjectWord(_engineHandle, word, 0);
            var result = Enumerable
                            .Range(0, GrammarEngine.sol_CountProjections(hProjs))
                            .Select(i => GrammarEngine.sol_GetEntryNameFX(_engineHandle, GrammarEngine.sol_GetIEntry(hProjs,i)))
                            .Distinct()
                            .AsImmutable();
            GrammarEngine.sol_DeleteProjections(hProjs);
            return result.Count switch
                {
                    0 => throw new InvalidOperationException(
                        $"Could not find neutral form of the word '{word}'"),
                    1 => result.Single(),
                    _ => throw new InvalidOperationException(
                        $"There are several neutral forms ({string.Join(", ", result)}) of the word '{word}'")
                };
        }

        public string GenerateWordForm(string word, PartOfSpeech? partOfSpeech, GrammarCharacteristics characteristics)
        {
            var entryId = GrammarEngine.sol_FindEntry(
                _engineHandle, 
                word, 
                partOfSpeech.HasValue ? (int)partOfSpeech.Value : -1,
                GrammarEngineAPI.RUSSIAN_LANGUAGE);

            if (entryId == -1)
            {
                throw new InvalidOperationException($"Could not find {partOfSpeech} '{word}' in lexicon.");
            }

            var result = GenerateWordForms(
                    entryId, 
                    characteristics.ToCoordIdStateIdPairArray(
                        (coordId, stateId) => coordId.IsOneOf(GrammarEngineAPI.CASE_ru, GrammarEngineAPI.NUMBER_ru, GrammarEngineAPI.FORM_ru)
                            ? (coordId, stateId)
                            : default((int, int)?)))
                    .FirstOrDefault();

            return !string.IsNullOrEmpty(result)
                ? result
                : throw new InvalidOperationException($"Could not put the word '{word}' to the form {characteristics}");
        }

        public IReadOnlyCollection<string> GenerateWordForms(int entryId, (int CoordinateId, int StateId)[] coordinateStates)
        {
            var grammarForms = coordinateStates.SelectMany(cs => new[] { cs.CoordinateId, cs.StateId }).ToArray();
            var hstr = GrammarEngine.sol_GenerateWordforms(_engineHandle, entryId, coordinateStates.Length, grammarForms);
            var result = Enumerable
                            .Range(0, GrammarEngine.sol_CountStrings(hstr))
                            .Select(i => GrammarEngine.sol_GetStringFX(hstr, i).ToLower(Russian))
                            .AsImmutable();
            SuppressCa1806(GrammarEngine.sol_DeleteStrings(hstr));
            return result;
        }

        public string GetPluralForm(LemmaVersion lemmaVersion)
        {
            ProgramLogic.Check(
                (lemmaVersion.Characteristics.TryGetNumber() ?? Number.Единственное) == Number.Единственное,
                $"An attempt was made to get plural form of a lemma ({lemmaVersion}) that is already in Plural form. " +
                "This is the indication that a generative pattern contains a Plurality Sensitive hint {мн.} on word in plural form.");

            var lemmaState = lemmaVersion.Characteristics.ToCoordIdStateIdPairArray(
                (coordinateId, stateId) => coordinateId switch 
                    {
                        GrammarEngineAPI.NUMBER_ru => (coordinateId, GrammarEngineAPI.PLURAL_NUMBER_ru),
                        GrammarEngineAPI.GENDER_ru => (coordinateId, GrammarEngineAPI.MASCULINE_GENDER_ru),
                        _ => (coordinateId, stateId)
                    });

            var entryId = TryFindMasculineForm(lemmaVersion).OrElse(lemmaVersion.EntryId);

            return (
                from wordForm in GenerateWordForms(entryId, new[] { (GrammarEngineAPI.NUMBER_ru, GrammarEngineAPI.PLURAL_NUMBER_ru) })
                from wordFormLemmaState in ProjectWord(wordForm, GetProjectionCharacteristics, lemmaVersion.PartOfSpeech)
                let numberOfMatchingCoordinates = lemmaState.Intersect(wordFormLemmaState).Count()
                orderby numberOfMatchingCoordinates descending
                select wordForm.ToLower(Russian))
                .TryFirst()
                .OrElse(() => throw new InvalidOperationException($"Could not find plural form for {lemmaVersion}"));

            (int CoordinateId, int StateId)[] GetProjectionCharacteristics(IntPtr hProjs, int i) =>
                lemmaState!
                    .Select(it => (it.CoordinateId, GrammarEngine.sol_GetProjCoordState(_engineHandle, hProjs, i, it.CoordinateId)))
                    .ToArray();
        }

        public string GetPartOfSpeechName(PartOfSpeech partOfSpeech)
        {
            var buffer = CreateBuffer(_engineHandle);
            SuppressCa1806(GrammarEngine.sol_GetClassName(_engineHandle, (int)partOfSpeech, buffer));
            buffer.Append('.');
            return buffer.ToString().ToLower(Russian);
        }

        public string GetStateName(int categoryId, int stateId)
        {
            var buffer = CreateBuffer(_engineHandle);
            SuppressCa1806(GrammarEngine.sol_GetCoordStateName(_engineHandle, categoryId, stateId, buffer));
            buffer.Append('.');
            return buffer.ToString().ToLower(Russian);
        }

        public IReadOnlyCollection<CoordinateValue> ResolveCoordinates(
            IEnumerable<string> valueNames) =>
                (from name in valueNames
                 let it = _knownCoordStateNames[name]
                 select new CoordinateValue(it.CoordinateType, name, it.StateId)
                ).AsImmutable();

        public void Dispose()
        {
            _engineHandle.Dispose();
        }

        private SentenceElement CreateSentenceElement(IntPtr hNode, int? leafType = null)
        {
            var content = GrammarEngine.sol_GetNodeContentsFX(hNode);
            
            var position = GrammarEngine.sol_GetNodePosition(hNode);

            var lemmaVersions = Enumerable.Range(0, GrammarEngine.sol_GetNodeVersionCount(_engineHandle, hNode))
                .Select(versionIndex => 
                new
                {
                    VersionIndex = versionIndex,
                    EntryVersionId = GrammarEngine.sol_GetNodeVerIEntry(_engineHandle, hNode, versionIndex)
                })
                .Select(item =>
                {
                    var entryVersionId = item.EntryVersionId;
                    var versionIndex = item.VersionIndex;
                    var lemma = CreateBuffer(_engineHandle);
                    SuppressCa1806(GrammarEngine.sol_GetEntryName(_engineHandle, entryVersionId, lemma));

                    var partOfSpeechIndex = GrammarEngine.sol_GetEntryClass(_engineHandle, entryVersionId);
                    var partOfSpeech = partOfSpeechIndex < 0 ? (PartOfSpeech?)null : (PartOfSpeech)partOfSpeechIndex;
                    var grammarCharacteristcs = BuldGrammarCharacteristics(hNode, versionIndex, partOfSpeech);
                    return new LemmaVersion(lemma.ToString(), entryVersionId, partOfSpeech, grammarCharacteristcs);
                });

            var children = Enumerable.Range(0, GrammarEngine.sol_CountLeafs(hNode))
                .Select(leaveIndex => CreateSentenceElement(
                    GrammarEngine.sol_GetLeaf(hNode, leaveIndex), 
                    GrammarEngine.sol_GetLeafLinkType(hNode, leaveIndex)));

            return new SentenceElement(
                Content: content,
                LeafLinkType: leafType == null || leafType.Value < 0 ? null : (LinkType)leafType.Value,
                LemmaVersions: new StructuralEquatableArray<LemmaVersion>(lemmaVersions.Distinct()), 
                Children: children.AsImmutable(),
                PositionInSentence: position);
        }

        private IReadOnlyCollection<T> ProjectWord<T>(
            string word, 
            Func<IntPtr, int, T> projectionSelector, 
            PartOfSpeech? partOfSpeech = default)
        {
            var hProjs = GrammarEngine.sol_ProjectWord(_engineHandle, word, 0);
            var result = Enumerable
                            .Range(0, GrammarEngine.sol_CountProjections(hProjs))
                            .Where(i => partOfSpeech == null || 
                                   (int)partOfSpeech.Value == GetProjClass(hProjs, i))
                            .Select(i => projectionSelector(hProjs, i))
                            .AsImmutable();
            GrammarEngine.sol_DeleteProjections(hProjs);
            return result;

            int GetProjClass(IntPtr projections, int i) =>
                GrammarEngine.sol_GetEntryClass(
                    _engineHandle, 
                    GrammarEngine.sol_GetIEntry(projections, i));
        }

        private MayBe<int> TryFindMasculineForm(LemmaVersion lemmaVersion) =>
            lemmaVersion.Characteristics switch
            {
                NounCharacteristics noun => noun.Gender == Gender.Мужской 
                    ? Some(lemmaVersion.EntryId) 
                    : TryFindLinks(lemmaVersion.EntryId, GrammarEngineAPI.SEX_SYNONYM_link)
                        .TryFirst(entryId => GrammarEngineAPI.MASCULINE_GENDER_ru ==
                                  GrammarEngine.sol_GetEntryCoordState(_engineHandle, entryId, GrammarEngineAPI.GENDER_ru)),
                _ => Some(lemmaVersion.EntryId)
            };

        private IReadOnlyCollection<int> TryFindLinks(int entryId, int linkType)
        {
            var linksList = GrammarEngine.sol_ListLinksTxt(_engineHandle, entryId, linkType, 0);
            if (linksList == IntPtr.Zero)
            {
                return Array.Empty<int>();
            }

            var result = Enumerable
                .Range(0, GrammarEngine.sol_LinksInfoCount(_engineHandle, linksList))
                .Select(i => GrammarEngine.sol_LinksInfoEKey2(_engineHandle, linksList, i))
                .AsImmutable();

            SuppressCa1806(GrammarEngine.sol_DeleteLinksInfo(_engineHandle, linksList));

            return result;
        }

        private string DescribeError() => 
            GrammarEngine.sol_GetErrorFX(_engineHandle);

        private static string DefaultSolarixDictionaryXmlPath => 
            Path.GetFullPath(Path.Combine(Directory.GetParent(Assembly.GetExecutingAssembly().Location)!.FullName, @"dictionary.xml"));

        private GrammarCharacteristics BuldGrammarCharacteristics(IntPtr hNode, int versionIndex, PartOfSpeech? partOfSpeech) => 
            partOfSpeech == null || !_grammarCharacteristicsBuilders.TryGetValue(partOfSpeech.Value, out var builder)
                    ? new NullGrammarCharacteristics()
                    : builder(hNode, versionIndex);

        private static TState GetNodeVersionCoordinateState<TState>(IntPtr hNode, int versionIndex) where TState: struct 
        {
            var result = TryGetNodeVersionCoordinateState<TState>(hNode, versionIndex);
            if (result == null)
            {
                throw new InvalidOperationException($"Could not get {typeof(TState).Name} coordinate");
            }

            return result.Value;
        }

        private MayBe<string> TryGetInfinitive(IntPtr hNode)
        {
            var wordList = GrammarEngine.sol_SeekThesaurus(_engineHandle, GrammarEngine.sol_GetNodeIEntry(_engineHandle, hNode), 0, 0, 0, 0, 0);
            var potentialInfinitives = SolarixIntArrayToSystemArray(wordList);
            GrammarEngine.sol_DeleteInts(wordList);

            return potentialInfinitives
                .Select(id => new
                {
                    id,
                    classId = GrammarEngine.sol_GetEntryClass(_engineHandle, id),
                    aspect = GrammarEngine.sol_GetEntryCoordState(_engineHandle, id, GrammarEngineAPI.ASPECT_ru)
                })
                .Where(item => item.classId == GrammarEngineAPI.INFINITIVE_ru &&
                               item.aspect.IsOneOf(GrammarEngineAPI.PERFECT_ru, GrammarEngineAPI.IMPERFECT_ru))
                .OrderBy(item => item.aspect == GrammarEngineAPI.PERFECT_ru ? 0 : 1)
                .TryFirst()
                .Map(item => GetEntryName(item.id));
        }

        private MayBe<string> GetPerfectFormOfInfinitive(IntPtr hNode)
        {
            var linksList = GrammarEngine.sol_ListLinksTxt(_engineHandle, GrammarEngine.sol_GetNodeIEntry(_engineHandle, hNode), GrammarEngineAPI.TO_PERFECT, 0);
            if (linksList == IntPtr.Zero)
            {
                return None;
            }

            var linkListCount = GrammarEngine.sol_LinksInfoCount(_engineHandle, linksList);
            if (linkListCount == 0)
            {
                SuppressCa1806(GrammarEngine.sol_DeleteLinksInfo(_engineHandle, linksList));
                return None;
            }

            string result = string.Empty;
            for (var i = 0; i != linkListCount; ++i)
            {
                var linkedWordId = GrammarEngine.sol_LinksInfoEKey2(_engineHandle, linksList, i);
                if (GrammarEngine.sol_GetEntryCoordState(_engineHandle, linkedWordId, GrammarEngineAPI.ASPECT_ru) == GrammarEngineAPI.PERFECT_ru)
                {
                    result = GetEntryName(linkedWordId);
                    break;
                }
            }

            SuppressCa1806(GrammarEngine.sol_DeleteLinksInfo(_engineHandle, linksList));

            return Some(result);
        }

        private static TState? TryGetNodeVersionCoordinateState<TState>(IntPtr hNode, int versionIndex) where TState : struct
        {
            var coordinateId = CoordinateStateTypeToCoordinateIdMap[typeof(TState)];
            var coordinateState = GrammarEngine.sol_GetNodeVerCoordState(hNode, versionIndex, coordinateId);
            return (coordinateState < 0) ? null : (TState)(object)coordinateState;
        }

        private static TState GetBoolCoordinateState<TState>(IntPtr hNode, int versionIndex, int coordinateId, TState trueValue, TState falseValue)
        {
            var valueCode = GrammarEngine.sol_GetNodeVerCoordState(hNode, versionIndex, coordinateId);
            switch (valueCode)
            {
                case 0:
                    return falseValue;
                case 1:
                    return trueValue;
                default:
                    throw new InvalidOperationException($"GetBoolCoordinateState<{typeof(TState).Name}>({coordinateId}) returned {valueCode} instead of 0 or 1.");
            }
        }

        private static int[] SolarixIntArrayToSystemArray(IntPtr solarixIntArray)
        {
            var arrayLength = GrammarEngine.sol_CountInts(solarixIntArray);
            if (arrayLength <= 0)
            {
                return Array.Empty<int>();
            }

            var result = new int[arrayLength];
            for (var i = 0; i != arrayLength; ++i)
            {
                result[i] = GrammarEngine.sol_GetInt(solarixIntArray, i);
            }

            return result;
        }

        private string GetEntryName(int entryId)
        {
            var buffer = CreateBuffer(_engineHandle);
            SuppressCa1806(GrammarEngine.sol_GetEntryName(_engineHandle, entryId, buffer));
            return buffer.ToString();
        }

        private static StringBuilder CreateBuffer(IntPtr engineHandle) =>
            new (GrammarEngine.sol_MaxLexemLen(engineHandle));

        
        private static string[] WorkaroundForBugWhenSometimesTokenizationDoesNotSplitWords(IEnumerable<string> tokens) =>
            tokens
                .SelectMany(token => token.Contains(' ') 
                                ? token.Split(' ', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
                                : token.ToImmutable())
                .ToArray();

        private readonly DisposableIntPtr _engineHandle;
        private readonly IDictionary<PartOfSpeech, Func<IntPtr, int, GrammarCharacteristics>> _grammarCharacteristicsBuilders;
        private readonly IReadOnlyDictionary<string, (Type CoordinateType, int StateId)> _knownCoordStateNames;
    }
}