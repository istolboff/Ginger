using System.Diagnostics;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Prolog.Engine;
using Prolog.Engine.Parsing;

namespace Prolog.Tests
{
    using static PrettyPrinting;
    
    internal static class PrologLogging
    {
        [Conditional("UseLogging")]
        public static void Setup(TestContext? testContext)
        {
            if (_loggingInitialized || testContext == null)
            {
                return;
            }

            _traceFilePath = GetTraceFilePath(testContext);

            MonadicParsing.ParsingEvent += text => File.AppendAllLines(_traceFilePath, new[] { text });
            Proof.ProofEvent += (description, nestingLevel, @this) =>
            {
                File.AppendAllText(_traceFilePath, new string(' ', nestingLevel * 3));

                if (description != null)
                {
                    File.AppendAllText(_traceFilePath, $"{description}: ");
                }

                File.AppendAllText(_traceFilePath, Print(@this));
                File.AppendAllLines(_traceFilePath, new[] { string.Empty });
            };

            _loggingInitialized = true;
        }

        [Conditional("UseLogging")]
        public static void OnTestStartup()
        {
            if (_traceFilePath != null)
            {
                File.Delete(_traceFilePath);
            }
        }

        private static string GetTraceFilePath(this TestContext testContext) =>
            Path.Combine(testContext.TestLogsDir, "MonadicParsing.trace");

        private static bool _loggingInitialized;
        private static string? _traceFilePath;
    }
}