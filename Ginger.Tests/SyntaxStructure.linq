<Query Kind="Program">
  <Reference Relative="..\bin\Debug\net5.0\Newtonsoft.Json.dll">&lt;MyDocuments&gt;\devl\Ginger\bin\Debug\net5.0\Newtonsoft.Json.dll</Reference>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

// https://vanya.jp.net/vtree/
void Main()
{
	var lines = File.ReadAllLines(MemoizedParsingResults);
	Enumerable
		.Range(0, lines.Length / 2)
		.Select(i => new 
					{ 
						Text = lines[i * 2], 
						ParsingResult = JsonConvert.SerializeObject(JsonConvert.DeserializeObject<IReadOnlyCollection<SentenceElement>>(lines[i * 2 + 1]))
					})
		.OrderBy(it => it.Text)
		.Dump();
}

record SentenceElement(string Content, IReadOnlyList<SentenceElement> Children, string LeafLinkType);

const string MemoizedParsingResults = @"C:\Users\istolbov\Documents\devl\Ginger\bin\Debug\net6.0\SolarixRussianGrammarParser.memoized";
