using System;
using System.IO;
using System.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ginger.Runner;

namespace Ginger.Tests
{
    using static TextManipulation;

    internal static class PatternRecognitionLogging
    {
        [Conditional("UseLogging")]
        public static void Setup(TestContext testContext)
        {
            var logFilePath = Path.Combine(testContext.TestLogsDir, "Patterns.log");

            PatternBuilder.PatternEstablished += (patternId, annotatedPattern, meaning, meaningBuildingRecipe) => 
                File.AppendAllText(
                    logFilePath, 
                    $"| {patternId} | {annotatedPattern.Sentence} | {Print(meaning, " ")} | {Print(meaningBuildingRecipe)} | {Environment.NewLine}");

            PatternBuilder.PatternRecognitionEvent += (log, checkSucceeded) =>
            {
                var prefix = checkSucceeded.HasValue 
                                ? ((checkSucceeded.Value ? "succeeded" : "failed") + "\tCheck for ") 
                                : string.Empty;
                File.AppendAllText(logFilePath, $"{prefix}{log}{Environment.NewLine}");
            };
        }
    }
}