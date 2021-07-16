using System.Collections.Generic;

namespace Ginger.Runner.Solarix
{
    internal interface IRussianLexicon
    {
        string GetNeutralForm(string word);
        
        string GenerateWordForm(string word, PartOfSpeech? partOfSpeech, GrammarCharacteristics characteristics);

        IReadOnlyCollection<string> GenerateWordForms(int entryId, (int CoordinateId, int StateId)[] coordinateStates);
        
        string GetPluralForm(LemmaVersion lemmaVersion);

        string GetPartOfSpeechName(PartOfSpeech partOfSpeech);

        string GetStateName(int categoryId, int stateId);

        IReadOnlyCollection<CoordinateValue> ResolveCoordinates(IEnumerable<string> valueNames);
    }
}