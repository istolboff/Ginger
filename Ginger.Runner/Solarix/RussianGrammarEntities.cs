using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Prolog.Engine.Miscellaneous;
using SolarixGrammarEngineNET;

namespace Ginger.Runner.Solarix
{
    using static MakeCompilerHappy;

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
        OBJECT_link                    = GrammarEngineAPI.OBJECT_link,
        ATTRIBUTE_link                 = GrammarEngineAPI.ATTRIBUTE_link,
        CONDITION_link                 = GrammarEngineAPI.CONDITION_link,
        CONSEQUENCE_link               = GrammarEngineAPI.CONSEQUENCE_link,
        SUBJECT_link                   = GrammarEngineAPI.SUBJECT_link,
        RHEMA_link                     = GrammarEngineAPI.RHEMA_link,
        COVERB_link                    = GrammarEngineAPI.COVERB_link,
        NUMBER2OBJ_link                = GrammarEngineAPI.NUMBER2OBJ_link,
        TO_VERB_link                   = GrammarEngineAPI.TO_VERB_link,
        TO_INF_link                    = GrammarEngineAPI.TO_INF_link,
        TO_PERFECT                     = GrammarEngineAPI.TO_PERFECT,
        TO_UNPERFECT                   = GrammarEngineAPI.TO_UNPERFECT,
        TO_NOUN_link                   = GrammarEngineAPI.TO_NOUN_link,
        TO_ADJ_link                    = GrammarEngineAPI.TO_ADJ_link,
        TO_ADV_link                    = GrammarEngineAPI.TO_ADV_link,
        TO_RETVERB                     = GrammarEngineAPI.TO_RETVERB,
        TO_GERUND_2_link               = GrammarEngineAPI.TO_GERUND_2_link,
        WOUT_RETVERB                   = GrammarEngineAPI.WOUT_RETVERB,
        TO_ENGLISH_link                = GrammarEngineAPI.TO_ENGLISH_link,
        TO_RUSSIAN_link                = GrammarEngineAPI.TO_RUSSIAN_link,
        TO_FRENCH_link                 = GrammarEngineAPI.TO_FRENCH_link,
        SYNONYM_link                   = GrammarEngineAPI.SYNONYM_link,
        SEX_SYNONYM_link               = GrammarEngineAPI.SEX_SYNONYM_link,
        CLASS_link                     = GrammarEngineAPI.CLASS_link,
        MEMBER_link                    = GrammarEngineAPI.MEMBER_link,
        TO_SPANISH_link                = GrammarEngineAPI.TO_SPANISH_link,
        TO_GERMAN_link                 = GrammarEngineAPI.TO_GERMAN_link,
        TO_CHINESE_link                = GrammarEngineAPI.TO_CHINESE_link,
        TO_POLAND_link                 = GrammarEngineAPI.TO_POLAND_link,
        TO_ITALIAN_link                = GrammarEngineAPI.TO_ITALIAN_link,
        TO_PORTUGUAL_link              = GrammarEngineAPI.TO_PORTUGUAL_link,
        ACTION_link                    = GrammarEngineAPI.ACTION_link,
        ACTOR_link                     = GrammarEngineAPI.ACTOR_link,
        TOOL_link                      = GrammarEngineAPI.TOOL_link,
        RESULT_link                    = GrammarEngineAPI.RESULT_link,
        TO_JAPANESE_link               = GrammarEngineAPI.TO_JAPANESE_link,
        TO_KANA_link                   = GrammarEngineAPI.TO_KANA_link,
        TO_KANJI_link                  = GrammarEngineAPI.TO_KANJI_link,
        ANTONYM_link                   = GrammarEngineAPI.ANTONYM_link,
        EXPLANATION_link               = GrammarEngineAPI.EXPLANATION_link,
        WWW_link                       = GrammarEngineAPI.WWW_link,
        ACCENT_link                    = GrammarEngineAPI.ACCENT_link,
        YO_link                        = GrammarEngineAPI.YO_link,
        TO_DIMINUITIVE_link            = GrammarEngineAPI.TO_DIMINUITIVE_link,
        TO_RUDE_link                   = GrammarEngineAPI.TO_RUDE_link,
        TO_BIGGER_link                 = GrammarEngineAPI.TO_BIGGER_link,
        TO_NEUTRAL_link                = GrammarEngineAPI.TO_NEUTRAL_link,
        TO_SCOLARLY                    = GrammarEngineAPI.TO_SCOLARLY,
        TO_SAMPLE_link                 = GrammarEngineAPI.TO_SAMPLE_link,
        USAGE_TAG_link                 = GrammarEngineAPI.USAGE_TAG_link,
        PROPERTY_link                  = GrammarEngineAPI.PROPERTY_link,
        TO_CYRIJI_link                 = GrammarEngineAPI.TO_CYRIJI_link,
        HABITANT_link                  = GrammarEngineAPI.HABITANT_link,
        CHILD_link                     = GrammarEngineAPI.CHILD_link,
        PARENT_link                    = GrammarEngineAPI.PARENT_link,
        UNIT_link                      = GrammarEngineAPI.UNIT_link,
        SET_link                       = GrammarEngineAPI.SET_link,
        TO_WEAKENED_link               = GrammarEngineAPI.TO_WEAKENED_link,
        VERBMODE_BASIC_link            = GrammarEngineAPI.VERBMODE_BASIC_link,
        NEGATION_PARTICLE_link         = GrammarEngineAPI.NEGATION_PARTICLE_link,
        NEXT_COLLOCATION_ITEM_link     = GrammarEngineAPI.NEXT_COLLOCATION_ITEM_link,
        SUBORDINATE_CLAUSE_link        = GrammarEngineAPI.SUBORDINATE_CLAUSE_link,
        RIGHT_GENITIVE_OBJECT_link     = GrammarEngineAPI.RIGHT_GENITIVE_OBJECT_link,
        ADV_PARTICIPLE_link            = GrammarEngineAPI.ADV_PARTICIPLE_link,
        POSTFIX_PARTICLE_link          = GrammarEngineAPI.POSTFIX_PARTICLE_link,
        INFINITIVE_link                = GrammarEngineAPI.INFINITIVE_link,
        NEXT_ADJECTIVE_link            = GrammarEngineAPI.NEXT_ADJECTIVE_link,
        NEXT_NOUN_link                 = GrammarEngineAPI.NEXT_NOUN_link,
        THEMA_link                     = GrammarEngineAPI.THEMA_link,
        RIGHT_AUX2INFINITIVE_link      = GrammarEngineAPI.RIGHT_AUX2INFINITIVE_link,
        RIGHT_AUX2PARTICIPLE           = GrammarEngineAPI.RIGHT_AUX2PARTICIPLE,
        RIGHT_AUX2ADJ                  = GrammarEngineAPI.RIGHT_AUX2ADJ,
        RIGHT_LOGIC_ITEM_link          = GrammarEngineAPI.RIGHT_LOGIC_ITEM_link,
        RIGHT_COMPARISON_Y_link        = GrammarEngineAPI.RIGHT_COMPARISON_Y_link,
        RIGHT_NOUN_link                = GrammarEngineAPI.RIGHT_NOUN_link,
        RIGHT_NAME_link                = GrammarEngineAPI.RIGHT_NAME_link,
        ADJ_PARTICIPLE_link            = GrammarEngineAPI.ADJ_PARTICIPLE_link,
        PUNCTUATION_link               = GrammarEngineAPI.PUNCTUATION_link,
        IMPERATIVE_SUBJECT_link        = GrammarEngineAPI.IMPERATIVE_SUBJECT_link,
        IMPERATIVE_VERB2AUX_link       = GrammarEngineAPI.IMPERATIVE_VERB2AUX_link,
        AUX2IMPERATIVE_VERB            = GrammarEngineAPI.AUX2IMPERATIVE_VERB,
        PREFIX_PARTICLE_link           = GrammarEngineAPI.PREFIX_PARTICLE_link,
        PREFIX_CONJUNCTION_link        = GrammarEngineAPI.PREFIX_CONJUNCTION_link,
        LOGICAL_CONJUNCTION_link       = GrammarEngineAPI.LOGICAL_CONJUNCTION_link,
        NEXT_CLAUSE_link               = GrammarEngineAPI.NEXT_CLAUSE_link,
        LEFT_AUX_VERB_link             = GrammarEngineAPI.LEFT_AUX_VERB_link,
        BEG_INTRO_link                 = GrammarEngineAPI.BEG_INTRO_link,
        RIGHT_PREPOSITION_link         = GrammarEngineAPI.RIGHT_PREPOSITION_link,
        WH_SUBJECT_link                = GrammarEngineAPI.WH_SUBJECT_link,
        IMPERATIVE_PARTICLE_link       = GrammarEngineAPI.IMPERATIVE_PARTICLE_link,
        GERUND_link                    = GrammarEngineAPI.GERUND_link,
        PREPOS_ADJUNCT_link            = GrammarEngineAPI.PREPOS_ADJUNCT_link,
        DIRECT_OBJ_INTENTION_link      = GrammarEngineAPI.DIRECT_OBJ_INTENTION_link,
        COPULA_link                    = GrammarEngineAPI.COPULA_link,
        DETAILS_link                   = GrammarEngineAPI.DETAILS_link,
        SENTENCE_CLOSER_link           = GrammarEngineAPI.SENTENCE_CLOSER_link,
        OPINION_link                   = GrammarEngineAPI.OPINION_link,
        APPEAL_link                    = GrammarEngineAPI.APPEAL_link,
        TERM_link                      = GrammarEngineAPI.TERM_link,
        SPEECH_link                    = GrammarEngineAPI.SPEECH_link,
        QUESTION_link                  = GrammarEngineAPI.QUESTION_link,
        POLITENESS_link                = GrammarEngineAPI.POLITENESS_link,
        SEPARATE_ATTR_link             = GrammarEngineAPI.SEPARATE_ATTR_link
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

    internal sealed record CoordinateValue(Type CoordinateType, string ValueName, int EnumValue)
    {
        public object ToObject() => Enum.ToObject(CoordinateType, EnumValue);
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

        public IEnumerable<Type> GetCoordinateTypes() =>
            GetType().GetProperties()
                .Select(property => property.PropertyType.RemoveNullability())
                .Where(propertyType => propertyType.IsEnum);

        public GrammarCharacteristics With(IEnumerable<CoordinateValue> coordinates)
        {
            var type = GetType();
            var constructorArguments = 
                from propertyType in type.GetMethod("Deconstruct")!.GetParameters().Select(p => p.ParameterType.GetElementType())
                let property = type.GetProperties().Single(p => p.PropertyType == propertyType)
                let coordinate = coordinates.TrySingle(
                                                c => c.CoordinateType == propertyType.RemoveNullability(), 
                                                matchingProperties => ProgramLogic.Error(
                                                    $"Several coordinate values of the same type {propertyType.RemoveNullability().Name} were provided: " +
                                                    string.Join(",", matchingProperties.Select(mp => mp.ValueName))))
                select coordinate.Map(c => c.ToObject()).OrElse(() => SuppressCa1062(property.GetValue(this)));
            return (GrammarCharacteristics)SuppressCa1062(Activator.CreateInstance(type, constructorArguments.ToArray()));
        }
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