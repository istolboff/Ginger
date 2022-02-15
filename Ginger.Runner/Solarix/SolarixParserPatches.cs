using System.Collections.Generic;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner.Solarix
{
    internal static class SolarixParserPatches
    {
        public static IReadOnlyCollection<SentenceElement> PostProcess(
            IReadOnlyCollection<SentenceElement> sentenceElements)
        =>
            sentenceElements.ConvertAll(element => ApplyPostProcessors(element).Element);

        public static (SentenceElement Element, bool Modified) ApplyPostProcessors(SentenceElement sentenceElement)
        {
            var children = sentenceElement.Children.ConvertAll(ApplyPostProcessors);
            var modified = children.Any(child => child.Modified);
            if (modified)
            {
                sentenceElement = sentenceElement with { Children = children.ConvertAll(it => it.Element) };
            }

            // 'то', распознанное как ПРИЛАГАТЕЛЬНОЕ 'тот' с ПАДЕЖ:ИМ; ЧИСЛО:ЕД; РОД:СР; СТЕПЕНЬ:АТРИБ,
            //  может быть СОЮЗОМ 'то'
            if (sentenceElement.LemmaVersions.Count == 1 &&
                sentenceElement.LemmaVersions.Single() is var lemmaVersion &&
                Impl.RussianIgnoreCase.Equals(lemmaVersion.Lemma, "тот") &&
                lemmaVersion.Characteristics is AdjectiveCharacteristics charcteristics &&
                charcteristics.Case == Case.Именительный && 
                charcteristics.Number == Number.Единственное &&
                charcteristics.Gender == Gender.Средний &&
                charcteristics.ComparisonForm == ComparisonForm.Атрибут)
            {
                sentenceElement = sentenceElement with 
                                    { 
                                        LemmaVersions = sentenceElement.LemmaVersions
                                                            .Concat(
                                                                new LemmaVersion(
                                                                    "то", 
                                                                    1074167235, 
                                                                    PartOfSpeech.Союз, 
                                                                    new NullGrammarCharacteristics()).ToImmutable())
                                                            .AsImmutable()
                                    };
                modified = true;
            }

            return (sentenceElement, modified);
        }
    }
}