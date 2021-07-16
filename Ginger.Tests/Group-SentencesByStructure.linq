<Query Kind="Program">
  <Reference Relative="..\bin\Debug\net5.0\gren_fx.dll">&lt;MyDocuments&gt;\devl\Ginger\bin\Debug\net5.0\gren_fx.dll</Reference>
  <Namespace>SolarixGrammarEngineNET</Namespace>
  <Namespace>System.Text.Encodings.Web</Namespace>
  <Namespace>System.Text.Json</Namespace>
  <Namespace>System.Text.Json.Serialization</Namespace>
  <Namespace>System.Text.Unicode</Namespace>
</Query>

private const string DefaultSolarixDictionaryXmlPath = @"C:\Users\istolbov\Documents\devl\Ginger\bin\Debug\net5.0\dictionary.xml";

void Main()
{
    _engineHandle = new DisposableIntPtr(
        GrammarEngine.sol_CreateGrammarEngineW(null), 
        handle => SuppressCa1806(GrammarEngine.sol_DeleteGrammarEngine(handle)),
        "GrammarEngine");
		
    var loadStatus = GrammarEngine.sol_LoadDictionaryExW(
            _engineHandle,
            DefaultSolarixDictionaryXmlPath,
            GrammarEngine.EngineInstanceFlags.SOL_GREN_LAZY_LEXICON);
			
    if (loadStatus != 1)
    {
        throw new InvalidOperationException($"Could not load Dictionary from {DefaultSolarixDictionaryXmlPath}. {DescribeError()}");
    }
	
	new[]
	{
		"Если волк и коза располагаются на одном конце острова, а заяц располагается на другом конце острова, то история завершается успехом с диагнозом апперцепция",
		"Если волк и коза находятся на одном береге острова, а фермер находится на другом береге острова, то миссия заканчивается неудачей с формулировкой апперцепция"
	}
	.Select(text => new { Text = text, Structure = Parse(text) })
	.GroupBy(it => JsonSerializer.Serialize(it.Structure, _jsonSerializerOptions))
	.Select(g => new { g.Key, Variants = g.Select(it => it.Text).ToList() })
	.Dump();
	
	_engineHandle.Dispose();
}

SentenceElement Parse(string text)
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
            throw new InvalidOperationException($"Could parse the text: {text}. {DescribeError()}");
        }

        var ngrafs = GrammarEngine.sol_CountGrafs(hPack);
        if (ngrafs <= 0)
        {
            throw new InvalidOperationException($"No graphs were parsed from the text: {text}.");
        }

		return Enumerable
				.Range(1, GrammarEngine.sol_CountRoots(hPack, 0) - 2)
				.Select(i => DumpSentenceElement(GrammarEngine.sol_GetRoot(hPack, 0, i)))
				.Single();
    }
    finally
    {
        GrammarEngine.sol_DeleteResPack(hPack);
    }
}


private SentenceElement DumpSentenceElement(IntPtr hNode, int? leafType = null)
{
    var content = GrammarEngine.sol_GetNodeContentsFX(hNode);

    var allLemmaVersions = Enumerable.Range(0, GrammarEngine.sol_GetNodeVersionCount(_engineHandle, hNode))
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
            var lemma = CreateBuffer();
            SuppressCa1806(GrammarEngine.sol_GetEntryName(_engineHandle, entryVersionId, lemma));

            var partOfSpeechIndex = GrammarEngine.sol_GetEntryClass(_engineHandle, entryVersionId);
            var partOfSpeech = partOfSpeechIndex < 0 ? $"undefined ({partOfSpeechIndex})" : GrammarEngine.sol_GetClassNameFX(_engineHandle, partOfSpeechIndex);
			var coordinates = string.Join(
							"; ",
							new[] { GrammarEngineAPI.CASE_ru, GrammarEngineAPI.NUMBER_ru,
                                    GrammarEngineAPI.VERB_FORM_ru, GrammarEngineAPI.PERSON_ru, GrammarEngineAPI.ASPECT_ru,
                                    GrammarEngineAPI.TENSE_ru, GrammarEngineAPI.COMPAR_FORM_ru }
							.Select(coordinateId => 
					            {
					                var stateName = CreateBuffer();
					                SuppressCa1806(GrammarEngine.sol_GetCoordName(_engineHandle, coordinateId, stateName));
					                var stateValue = CreateBuffer();
					                if (GrammarEngine.sol_CountCoordStates(_engineHandle, coordinateId) != 0)
					                {
					                    var coordState = GrammarEngine.sol_GetNodeVerCoordState(hNode, versionIndex, coordinateId);

					                    if (coordState >= 0)
					                    {
					                        SuppressCa1806(GrammarEngine.sol_GetCoordStateName(_engineHandle, coordinateId, coordState, stateValue));
											return $"{stateName}:{stateValue}";
					                    }
					                }
									
									return string.Empty;
					            })
								.Where(s => !string.IsNullOrEmpty(s)));
            return new { partOfSpeech, coordinates, entryVersionId };
        })
		.ToArray();
	
	if (allLemmaVersions.Length > 1)
	{
		throw new InvalidOperationException(
			"More than one possible treatment of the following word: " + 
			string.Join(
				"; ", 
				allLemmaVersions.Select(lv => 
				{
					var lemma = CreateBuffer();
					SuppressCa1806(GrammarEngine.sol_GetEntryName(_engineHandle, lv.entryVersionId, lemma));
					return lemma;
				})));
	}
	
	var lemmaVersion = allLemmaVersions.Single();

	return new SentenceElement(
					lemmaVersion.partOfSpeech, 
					lemmaVersion.coordinates, 
					Enumerable
						.Range(0, GrammarEngine.sol_CountLeafs(hNode))
						.Select(
							leaveIndex => DumpSentenceElement(
								            GrammarEngine.sol_GetLeaf(hNode, leaveIndex), 
								            GrammarEngine.sol_GetLeafLinkType(hNode, leaveIndex)))
						.ToList(), 
					leafType.HasValue ? ((LinkType)leafType).ToString() : string.Empty);
}

record SentenceElement(string PartOfSpeech, string Coordinates, IReadOnlyList<SentenceElement> Children, string LeafLinkType);


internal sealed class DisposableIntPtr : IDisposable 
{
    public DisposableIntPtr(IntPtr handle, Action<IntPtr> dispose, string objectName, IntPtr? nullValue = null)
    {
        if (handle == (nullValue ?? IntPtr.Zero))
        {
            throw new ArgumentNullException(nameof(handle), "Could not create " + objectName);
        }

        _handle = handle;
        _dispose = dispose;
        _objectName = objectName;
    }

    public void Dispose()
    {
        if (_isDisposed)
        {
            return;
        }

        _dispose(_handle);
        _isDisposed = true;
    }

    public static implicit operator IntPtr(DisposableIntPtr @this) =>
        !@this._isDisposed 
            ? @this._handle
            : throw new ObjectDisposedException(@this._objectName);

    private readonly IntPtr _handle;
    private readonly Action<IntPtr> _dispose;
    private readonly string _objectName;
    private bool _isDisposed;
}

private StringBuilder CreateBuffer() =>
    new StringBuilder(GrammarEngine.sol_MaxLexemLen(_engineHandle));

private static string DescribeError()
{
    var errorLength = GrammarEngine.sol_GetErrorLen(_engineHandle);
    var errorBuffer = new StringBuilder(errorLength);
    var errorCode = GrammarEngine.sol_GetError(_engineHandle, errorBuffer, errorLength);
    return errorCode == 1 ? errorBuffer.ToString() : "Unknown error";
}

static void SuppressCa1806<T>(T unused)
{
}

static class GrammarEngineAPI
{
 public const int RUSSIAN_LANGUAGE = 2;            // language Russian
 public const int CASE_ru = 39;                    // enum ПАДЕЖ
 public const int NUMBER_ru = 28;                  // enum ЧИСЛО
 public const int GENDER_ru = 29;                  // enum РОД
 public const int VERB_FORM_ru = 35;               // enum НАКЛОНЕНИЕ
 public const int PERSON_ru = 27;                  // enum ЛИЦО
 public const int ASPECT_ru = 33;                  // enum ВИД
 public const int TENSE_ru = 36;                   // enum ВРЕМЯ 
 public const int SHORTNESS_ru = 37;               // enum КРАТКИЙ
 public const int FORM_ru = 40;                    // enum ОДУШ
 public const int COMPAR_FORM_ru = 42;             // enum СТЕПЕНЬ
}

internal enum LinkType
{
    OBJECT_link = 0,
    ATTRIBUTE_link = 1,
    CONDITION_link = 2,
    CONSEQUENCE_link = 3,
    SUBJECT_link = 4,
    RHEMA_link = 5,
    COVERB_link = 6,
    NUMBER2OBJ_link = 12,
    TO_VERB_link = 16,
    TO_INF_link = 17,
    TO_PERFECT = 18,
    TO_UNPERFECT = 19,
    TO_NOUN_link = 20,
    TO_ADJ_link = 21,
    TO_ADV_link = 22,
    TO_RETVERB = 23,
    TO_GERUND_2_link = 24,
    WOUT_RETVERB = 25,
    TO_ENGLISH_link = 26,
    TO_RUSSIAN_link = 27,
    TO_FRENCH_link = 28,
    SYNONYM_link = 29,
    SEX_SYNONYM_link = 30,
    CLASS_link = 31,
    MEMBER_link = 32,
    TO_SPANISH_link = 33,
    TO_GERMAN_link = 34,
    TO_CHINESE_link = 35,
    TO_POLAND_link = 36,
    TO_ITALIAN_link = 37,
    TO_PORTUGUAL_link = 38,
    ACTION_link = 39,
    ACTOR_link = 40,
    TOOL_link = 41,
    RESULT_link = 42,
    TO_JAPANESE_link = 43,
    TO_KANA_link = 44,
    TO_KANJI_link = 45,
    ANTONYM_link = 46,
    EXPLANATION_link = 47,
    WWW_link = 48,
    ACCENT_link = 49,
    YO_link = 50,
    TO_DIMINUITIVE_link = 51,
    TO_RUDE_link = 52,
    TO_BIGGER_link = 53,
    TO_NEUTRAL_link = 54,
    TO_SCOLARLY = 55,
    TO_SAMPLE_link = 56,
    USAGE_TAG_link = 57,
    PROPERTY_link = 58,
    TO_CYRIJI_link = 59,
    HABITANT_link = 60,
    CHILD_link = 61,
    PARENT_link = 62,
    UNIT_link = 63,
    SET_link = 64,
    TO_WEAKENED_link = 65,
    VERBMODE_BASIC_link = 66,
    NEGATION_PARTICLE_link = 67,
    NEXT_COLLOCATION_ITEM_link = 68,
    SUBORDINATE_CLAUSE_link = 69,
    RIGHT_GENITIVE_OBJECT_link = 70,
    ADV_PARTICIPLE_link = 71,
    POSTFIX_PARTICLE_link = 72,
    INFINITIVE_link = 73,
    NEXT_ADJECTIVE_link = 74,
    NEXT_NOUN_link = 75,
    THEMA_link = 76,
    RIGHT_AUX2INFINITIVE_link = 77,
    RIGHT_AUX2PARTICIPLE = 78,
    RIGHT_AUX2ADJ = 79,
    RIGHT_LOGIC_ITEM_link = 80,
    RIGHT_COMPARISON_Y_link = 81,
    RIGHT_NOUN_link = 82,
    RIGHT_NAME_link = 83,
    ADJ_PARTICIPLE_link = 84,
    PUNCTUATION_link = 85,
    IMPERATIVE_SUBJECT_link = 86,
    IMPERATIVE_VERB2AUX_link = 87,
    AUX2IMPERATIVE_VERB = 88,
    PREFIX_PARTICLE_link = 89,
    PREFIX_CONJUNCTION_link = 90,
    LOGICAL_CONJUNCTION_link = 91,
    NEXT_CLAUSE_link = 92,
    LEFT_AUX_VERB_link = 93,
    BEG_INTRO_link = 94,
    RIGHT_PREPOSITION_link = 95,
    WH_SUBJECT_link = 96,
    IMPERATIVE_PARTICLE_link = 97,
    GERUND_link = 98,
    PREPOS_ADJUNCT_link = 99,
    DIRECT_OBJ_INTENTION_link = 100,
    COPULA_link = 101,
    DETAILS_link = 102,
    SENTENCE_CLOSER_link = 103,
    OPINION_link = 104,
    APPEAL_link = 105,
    TERM_link = 106,
    SPEECH_link = 107,
    QUESTION_link = 108,
    POLITENESS_link = 109,
    SEPARATE_ATTR_link = 110,
}

private static JsonSerializerOptions _jsonSerializerOptions = new JsonSerializerOptions 
	{ 
	  Encoder = JavaScriptEncoder.Create(UnicodeRanges.BasicLatin, UnicodeRanges.Cyrillic), 
	  WriteIndented = true 
	};
private static DisposableIntPtr _engineHandle;

