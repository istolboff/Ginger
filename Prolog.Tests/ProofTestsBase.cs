using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Prolog.Engine;

using static Prolog.Tests.VerboseReporting;

using V = System.Collections.Generic.Dictionary<Prolog.Engine.Variable, Prolog.Engine.Term>;

namespace Prolog.Tests
{
    public abstract class ProofTestsBase
    {
#if UseLogging
        [TestInitialize]
        public void Setup()
        {
            System.IO.File.Delete(_traceFilePath!);
        }
#endif

        [Conditional("UseLogging")]
        protected static void SetupLogging(TestContext? testContext)
        {
            _traceFilePath = System.IO.Path.Combine(testContext?.TestLogsDir ?? System.IO.Path.GetTempPath(), "Prolog.trace");

            Proof.ProofEvent += (description, nestingLevel, @this) =>
            {
                System.IO.File.AppendAllText(_traceFilePath, new string(' ', nestingLevel * 3));

                if (description != null)
                {
                    System.IO.File.AppendAllText(_traceFilePath, $"{description}: ");
                }

                System.IO.File.AppendAllText(_traceFilePath, Dump(@this));
                System.IO.File.AppendAllLines(_traceFilePath, new[] { string.Empty });
            };
        }

        protected static void CheckSituations(
            IEnumerable<(string Description, Rule[] Program, ComplexTerm[] Query, V[] ExpectedProofs)> situations,
            bool onlyFirstSolution = false,
            bool ignoreUnlistedActualInstantiations = false)
        {
            var erroneousProofs = 
                (from situation in situations
                let expectedProofs = situation.ExpectedProofs.Select(Unification.Success).ToArray()
                let actualProofs = Proof.Find(situation.Program, situation.Query).Take(onlyFirstSolution ? 1 : int.MaxValue).ToArray()
                where !ignoreUnlistedActualInstantiations
                        ? !expectedProofs.SequenceEqual(actualProofs)
                        : expectedProofs.Length != actualProofs.Length ||
                          expectedProofs.Zip(actualProofs).Any(it => it.First != RemoveUnlistedInstantiations(it.Second, it.First.Instantiations.Keys))
                select new 
                { 
                    situation.Description,
                    Prorgam = Dump(situation.Program, Environment.NewLine), 
                    Query = Dump(situation.Query),
                    ExpectedProofs = Dump(expectedProofs), 
                    ActualProofs = Dump(actualProofs) 
                })
                .ToList();

            Assert.IsFalse(erroneousProofs.Any(), Environment.NewLine + string.Join(Environment.NewLine, erroneousProofs));
        }

        private static UnificationResult RemoveUnlistedInstantiations(UnificationResult result, IEnumerable<Variable> keys) =>
            result with { Instantiations = new (result.Instantiations.Where(kvp => keys.Contains(kvp.Key))) };

        private static string? _traceFilePath; 
    }
}