using System;
using System.Collections.Generic;

namespace Ginger.Runner.Solarix
{
    internal interface IRussianGrammarParser : IDisposable 
    {
        IReadOnlyCollection<SentenceElement> Parse(string text);

        string[] Tokenize(string text);
    }
}