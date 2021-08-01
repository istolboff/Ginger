using BoDi;
using Ginger.Runner;
using Ginger.Runner.Solarix;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Prolog.Tests;
using TechTalk.SpecFlow;

namespace Ginger.Tests.StepDefinitions
{
    [Binding]
#pragma warning disable CA1812 // Your class is an internal class that is apparently never instantiated on Derived class    
    internal sealed class TestRun
#pragma warning restore CA1812
    {
        public TestRun(IObjectContainer diContainer)
        {
            _diContainer = diContainer;
        }

        [BeforeTestRun]
        public static void SetupTestRun(TestContext testContext)
        {
            var russianGrammarEngine = new SolarixRussianGrammarEngine();
            _russianGrammarParser = new SolarixParserMemoizer(russianGrammarEngine);
            _russianLexicon = russianGrammarEngine;
            _sentenceUnderstander = SentenceUnderstander.LoadFromEmbeddedResources(_russianGrammarParser, _russianLexicon);
            PrologLogging.Setup(testContext);
            PatternRecognitionLogging.Setup(testContext);
        }

        [AfterTestRun]
        public static void TeardownTestRun()
        {
            _russianGrammarParser?.Dispose();
        }

        [BeforeScenario]
        public void SetupDiContainer()
        {
            _diContainer.RegisterInstanceAs(_russianGrammarParser);
            _diContainer.RegisterInstanceAs(_russianLexicon);
            _diContainer.RegisterInstanceAs(_sentenceUnderstander);
        }

        private readonly IObjectContainer _diContainer;

        private static IRussianGrammarParser? _russianGrammarParser;
        private static IRussianLexicon? _russianLexicon;
        private static SentenceUnderstander? _sentenceUnderstander; 
   }
}