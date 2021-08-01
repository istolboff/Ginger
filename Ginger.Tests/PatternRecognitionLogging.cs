using System;
using System.IO;
using System.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Ginger.Runner;

namespace Ginger.Tests
{
    using static Prolog.Tests.VerboseReporting;

    internal static class PatternRecognitionLogging
    {
        [Conditional("UseLogging")]
        public static void Setup(TestContext testContext)
        {
            var logFilePath = Path.Combine(testContext.TestLogsDir, "Patterns.log");

            PatternBuilder.PatternEstablished += (patternId, annotatedPattern, meaning) => 
                File.AppendAllText(
                    logFilePath, 
                    $"| {patternId} | {annotatedPattern.Text} | {Dump(meaning, " ")} |" + Environment.NewLine);

            PatternBuilder.PatternRecognitionEvent += (log, checkSucceeded) => 
                File.AppendAllText(logFilePath, $"{(checkSucceeded ? "succeeded" : "failed")}\tCheck for {log}{Environment.NewLine}");

            GenerativePattern.PatternBuildingEvent += message => 
                File.AppendAllText(logFilePath, $"Generative Pattern Building Event: {message}" + Environment.NewLine);
        }
    }
}