using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Prolog.Engine.Miscellaneous;
using SolarixGrammarEngineNET;

namespace Ginger.Runner.Solarix
{
// ReSharper disable InconsistentNaming
// ReSharper disable UnusedMember.Global
    internal enum PartOfSpeech
    {
        Существительное = GrammarEngineAPI.NOUN_ru,
        Прилагательное = GrammarEngineAPI.ADJ_ru,
        Наречие = GrammarEngineAPI.ADVERB_ru,
        Глагол = GrammarEngineAPI.VERB_ru,
        Местоимение = GrammarEngineAPI.PRONOUN_ru,
        Инфинитив = GrammarEngineAPI.INFINITIVE_ru,
        Предлог = GrammarEngineAPI.PREPOS_ru,
        Союз = GrammarEngineAPI.CONJ_ru,
        Деепричастие = GrammarEngineAPI.GERUND_2_ru,
        Пунктуатор = GrammarEngineAPI.PUNCTUATION_class,
        Частица = GrammarEngineAPI.PARTICLE_ru,
        Местоим_Сущ = GrammarEngineAPI.PRONOUN2_ru,
        Притяж_Частица = GrammarEngineAPI.POSESS_PARTICLE,
        Num_Word = GrammarEngineAPI.NUM_WORD_CLASS,
        Impersonal_Verb = GrammarEngineAPI.IMPERSONAL_VERB_ru
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

    internal enum Case
    {
        Именительный = GrammarEngineAPI.NOMINATIVE_CASE_ru,
        Звательный = GrammarEngineAPI.VOCATIVE_CASE_ru,
        Родительный  = GrammarEngineAPI.GENITIVE_CASE_ru,
        Партитив = GrammarEngineAPI.PARTITIVE_CASE_ru,
        Творительный = GrammarEngineAPI.INSTRUMENTAL_CASE_ru,
        Винительный = GrammarEngineAPI.ACCUSATIVE_CASE_ru,
        Дательный = GrammarEngineAPI.DATIVE_CASE_ru,
        Предложный = GrammarEngineAPI.PREPOSITIVE_CASE_ru,
        Местный = GrammarEngineAPI.LOCATIVE_CASE_ru,
    }

    internal enum Number
    {
        Единственное = GrammarEngineAPI.SINGULAR_NUMBER_ru,
        Множественное = GrammarEngineAPI.PLURAL_NUMBER_ru
    }

    internal enum VerbForm
    {
        Изъявительное = GrammarEngineAPI.VB_INF_ru,
        Повелительное = GrammarEngineAPI.VB_ORDER_ru
    }

    internal enum Person
    {
        Первое = GrammarEngineAPI.PERSON_1_ru,
        Второе = GrammarEngineAPI.PERSON_2_ru,
        Третье = GrammarEngineAPI.PERSON_3_ru
    }

    internal enum VerbAspect
    {
        Совершенный = GrammarEngineAPI.PERFECT_ru,
        Несовершенный = GrammarEngineAPI.IMPERFECT_ru
    }

    internal enum Tense
    {
        Прошедшее = GrammarEngineAPI.PAST_ru,
        Настоящее = GrammarEngineAPI.PRESENT_ru,
        Будущее = GrammarEngineAPI.FUTURE_ru
    }

    internal enum Transitiveness
    {
        Непереходный = GrammarEngineAPI.NONTRANSITIVE_VERB_ru,
        Переходный = GrammarEngineAPI.TRANSITIVE_VERB_ru
    }

    internal enum Gender
    {
        Мужской = GrammarEngineAPI.MASCULINE_GENDER_ru,
        Женский = GrammarEngineAPI.FEMININE_GENDER_ru,
        Средний = GrammarEngineAPI.NEUTRAL_GENDER_ru
    }

    internal enum AdjectiveForm
    {
        Полное,
        Краткое
    }

    internal enum ComparisonForm
    {
        Атрибут = GrammarEngineAPI.ATTRIBUTIVE_FORM_ru,
        Сравнительная = GrammarEngineAPI.COMPARATIVE_FORM_ru,
        Превосходная = GrammarEngineAPI.SUPERLATIVE_FORM_ru,
        Компаратив2 = GrammarEngineAPI.LIGHT_COMPAR_FORM_RU
    }

    internal static class Impl
    {
        public static readonly CultureInfo Russian = CultureInfo.GetCultureInfo("ru");

        public static readonly StringComparer RussianIgnoreCase = StringComparer.Create(Russian, ignoreCase: true);

        public static IReadOnlyCollection<GrammarCharacteristics> GrammarCharacteristicsInstances =>
            new GrammarCharacteristics[]
            {
                new AdjectiveCharacteristics(default, default, default, default, default),
                new VerbCharacteristics(default, default, default, default, default, default, default),
                new NounCharacteristics(default, default, default),
                new VerbalNounCharacteristics(default, default, default, string.Empty),
                new PronounCharacteristics(default, default, default, default),
                new AdverbCharacteristics(default),
                new GerundCharacteristics(default, default),
                new InfinitiveCharacteristics(default, default, string.Empty),
                new NullGrammarCharacteristics()
            };

        public static IReadOnlyCollection<Type> GrammarCharacteristicsTypes =>
            GrammarCharacteristicsInstances.Select(i => i.GetType()).AsImmutable();

        public static readonly IReadOnlyDictionary<Type, int> CoordinateStateTypeToCoordinateIdMap = 
            new Dictionary<Type, int>
            {
                { typeof(Case), GrammarEngineAPI.CASE_ru },
                { typeof(Number), GrammarEngineAPI.NUMBER_ru },
                { typeof(Gender), GrammarEngineAPI.GENDER_ru },
                { typeof(Person), GrammarEngineAPI.PERSON_ru },
                { typeof(VerbForm), GrammarEngineAPI.VERB_FORM_ru },
                { typeof(VerbAspect), GrammarEngineAPI.ASPECT_ru },
                { typeof(Tense), GrammarEngineAPI.TENSE_ru },
                { typeof(ComparisonForm), GrammarEngineAPI.COMPAR_FORM_ru },
                { typeof(Transitiveness), GrammarEngineAPI.TRANSITIVENESS_ru },
                { typeof(AdjectiveForm), GrammarEngineAPI.SHORTNESS_ru }
            };
    }

    internal abstract record GrammarCharacteristics
    {
        public Number? TryGetNumber() =>
            this switch
            {
                AdjectiveCharacteristics adjectiveCharacteristics => adjectiveCharacteristics.Number,
                VerbCharacteristics verbCharacteristics => verbCharacteristics.Number,
                NounCharacteristics nounCharacteristics => nounCharacteristics.Number,
                PronounCharacteristics pronounCharacteristics => pronounCharacteristics.Number,
                AdverbCharacteristics => default,
                GerundCharacteristics => default,
                InfinitiveCharacteristics => default,
                NullGrammarCharacteristics => default,
                _ => throw ProgramLogic.Error($"Please add switch branch for {GetType().Name} in GrammarCharacteristics.TryGetNumber()")
            };

        public Gender? TryGetGender() =>
            this switch
            {
                AdjectiveCharacteristics adjectiveCharacteristics => adjectiveCharacteristics.Gender,
                VerbCharacteristics => default(Gender?),
                NounCharacteristics nounCharacteristics => nounCharacteristics.Gender,
                PronounCharacteristics pronounCharacteristics => pronounCharacteristics.Gender,
                AdverbCharacteristics => default(Gender?),
                GerundCharacteristics => default(Gender?),
                InfinitiveCharacteristics => default(Gender?),
                NullGrammarCharacteristics => default(Gender?),
                _ => throw ProgramLogic.Error($"Please add switch branch for {GetType().Name} in GrammarCharacteristics.TryGetGender()")
            };

        public (int CoordinateId, int StateId)[] ToCoordIdStateIdPairArray(
            Func<int, int, (int CoordinateId, int StateId)?> adjustCoordIdStateIdPair) 
        =>
            (from property in GetType().GetProperties()
            let propertyType = property.PropertyType.RemoveNullability()
            where propertyType.IsEnum
            let coordinateId = Impl.CoordinateStateTypeToCoordinateIdMap[propertyType]
            let stateId = property.GetValue(this)
            where stateId != null
            let adjustedValues = adjustCoordIdStateIdPair(coordinateId, (int)stateId!)
            where adjustedValues != null
            select adjustedValues.Value
            ).ToArray();
   }

#pragma warning disable CA1801 // Review unused parameters
    internal sealed record AdjectiveCharacteristics(
        Case? Case, 
        Number? Number, 
        Gender? Gender, 
        AdjectiveForm AdjectiveForm, 
        ComparisonForm ComparisonForm) 
        : GrammarCharacteristics;

    internal sealed record VerbCharacteristics(
        Case? Case, 
        Number Number, 
        VerbForm VerbForm, 
        Person? Person, 
        VerbAspect VerbAspect, 
        Tense? Tense,
        Transitiveness? Transitiveness) 
        : GrammarCharacteristics;

    internal sealed record AdverbCharacteristics(ComparisonForm ComparisonForm) : GrammarCharacteristics;

    internal record NounCharacteristics(Case Case, Number Number, Gender Gender) : GrammarCharacteristics;

    internal sealed record VerbalNounCharacteristics(
        Case Case, 
        Number Number, 
        Gender Gender, 
        string RelatedInfinitive) 
        : NounCharacteristics(Case, Number, Gender);

    internal sealed record PronounCharacteristics(Case Case, Gender? Gender, Number Number, Person Person) : GrammarCharacteristics;

    internal sealed record GerundCharacteristics(Case Case, VerbAspect VerbAspect) : GrammarCharacteristics;

    internal sealed record InfinitiveCharacteristics(
        VerbAspect VerbAspect, 
        Transitiveness Transitiveness, 
        string PerfectForm)
        : GrammarCharacteristics;
#pragma warning restore CA1801

    internal sealed record NullGrammarCharacteristics : GrammarCharacteristics;

    internal sealed record LemmaVersion(string Lemma, int EntryId, PartOfSpeech? PartOfSpeech, GrammarCharacteristics Characteristics);

    internal sealed record SentenceElement(string Content, IReadOnlyCollection<LemmaVersion> LemmaVersions, IReadOnlyList<SentenceElement> Children, LinkType? LeafLinkType, int PositionInSentence);
// ReSharper restore UnusedMember.Global
// ReSharper restore InconsistentNaming
}