using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Prolog.Engine.Miscellaneous;

namespace Prolog.Engine
{
    using UnificationResult = StructuralEquatableDictionary<Variable, Term>;

    using static Builtin;
    using static DomainApi;

    public static class Proof
    {
        public static IEnumerable<UnificationResult> Find(
            IReadOnlyCollection<Rule> programRules, 
            IEnumerable<IReadOnlyCollection<ComplexTerm>> queriesAlternatives) =>
            queriesAlternatives.SelectMany(queries => Find(programRules, queries.ToArray()));

        public static IEnumerable<UnificationResult> Find(
            IReadOnlyCollection<Rule> programRules, 
            params ComplexTerm[] queries) =>
            FindCore(
                Builtin.Rules
                    .Concat(CheckProgramRules(programRules))
                    .GroupBy(r => (r.Conclusion.Functor.Name, r.Conclusion.Functor.Arity))
                    .ToDictionary(g => g.Key, g => g.AsImmutable()), 
                queries);

        internal static IEnumerable<UnificationResult> FindCore(
            IReadOnlyDictionary<(string FunctorName, int FunctorArity), IReadOnlyCollection<Rule>> programRules, 
            params ComplexTerm[] queries)
        {
            var queryVariableNames = ListAllMentionedVariableNames(queries);
            return FindCore(
                        programRules: programRules, 
                        queries: ImmutableList.Create(queries),
                        mentionedVariableNames: ImmutableHashSet.CreateRange(queryVariableNames),
                        variableInstantiations: ImmutableDictionary.Create<Variable, Term>(),
                        useCutMode: queries.Contains(Cut),
                        nestingLevel: 0)
                    .Where(result => result.HasValue)
                    .Select(result => ResolveInternalInstantiations(result.Value!, queryVariableNames)
                                        .Trace(0, "yield Proof"));
        }

        public static event Action<string?, int, object>? ProofEvent;

        private static IEnumerable<MayBe<UnificationResult>> FindCore(
                IReadOnlyDictionary<(string FunctorName, int FunctorArity), IReadOnlyCollection<Rule>> programRules,
                ImmutableList<ComplexTerm> queries,
                ImmutableHashSet<string> mentionedVariableNames,
                ImmutableDictionary<Variable, Term> variableInstantiations,
                bool useCutMode,
                int nestingLevel)
        {
            if (!queries.Any())
            {
                yield return Unification.Success(variableInstantiations);
            }
            else
            {
                var currentQueryRaw = queries.First().Trace(nestingLevel, "currentQueryRaw");
                if (currentQueryRaw == Fail)
                {
                    yield return Unification.Failure;
                }
                else if (currentQueryRaw == Cut)
                {
                    foreach (var solution in FindCore(
                            programRules, 
                            queries.RemoveAt(0),
                            mentionedVariableNames,
                            variableInstantiations,
                            useCutMode: false,
                            nestingLevel: nestingLevel + 1))
                    {
                        yield return solution;
                    }                    
                }
                else
                {
                    var currentQuery = CallFunctor.Equals(currentQueryRaw.Functor)
                        ? UnwrapCall(currentQueryRaw, variableInstantiations) 
                        : currentQueryRaw;
                    var matchingRules = FindMatchingRules(programRules, currentQuery);
                    var extendedSetOfMentionedVariableNames = mentionedVariableNames.Union(variableInstantiations.Select(vi => vi.Key.Name));
                    foreach (var matchingRule in matchingRules.Trace(nestingLevel, "matching rules"))
                    {
                        var ruleWithRenamedVariables = RenameRuleVariablesToMakeThemDifferentFromAlreadyUsedNames(matchingRule, extendedSetOfMentionedVariableNames).Trace(nestingLevel, "ruleWithRenamedVariables");
                        var ruleConclusionUnificationResult = ruleWithRenamedVariables.Conclusion.Functor switch 
                            {
                                BinaryPredicate binaryOperator => binaryOperator.Invoke(currentQuery.Arguments),
                                MetaFunctor metaFunctor => metaFunctor.Invoke(programRules, currentQuery.Arguments),
                                _ => Unification.CarryOut(currentQuery, ruleWithRenamedVariables.Conclusion)
                            };
                        if (ruleConclusionUnificationResult.Trace(nestingLevel, "ruleConclusionUnificationResult").HasValue)
                        {
                            var updatedQueries = queries.RemoveAt(0).InsertRange(0, ruleWithRenamedVariables.Premises);
                            var updatedQueriesWithSubstitutedVariables = updatedQueries.Select(p => ApplyVariableInstantiations(p, ruleConclusionUnificationResult.Value!));
                            var matchingRuleContainsCut = matchingRule.Premises.Contains(Cut);
                            var encounteredAtLeastOneProof = false;
                            foreach (var solution in FindCore(
                                programRules, 
                                ImmutableList.CreateRange(updatedQueriesWithSubstitutedVariables),
                                extendedSetOfMentionedVariableNames.Union(ListAllMentionedVariableNames(ruleWithRenamedVariables.Premises)),
                                variableInstantiations.AddRange(ruleConclusionUnificationResult.Value!),
                                useCutMode: useCutMode || matchingRuleContainsCut,
                                nestingLevel: nestingLevel + 1))
                            {
                                encounteredAtLeastOneProof = true;
                                yield return solution;
                            }

                            if ((useCutMode || matchingRuleContainsCut) && encounteredAtLeastOneProof)
                            {
                                break;
                            }
                        }
                    }
                }
            }

            static ComplexTerm UnwrapCall(ComplexTerm currentQueryRaw, IReadOnlyDictionary<Variable, Term> variableInstantiations) =>
                currentQueryRaw.Arguments.SingleOrDefault() switch 
                {
                    ComplexTerm complexTerm => complexTerm,
                    Variable variableToCall => variableInstantiations.TryGetValue(variableToCall, out var term)
                        ? (term is ComplexTerm result
                            ? result
                            : throw new InvalidOperationException($"Variable {variableToCall.Name} is instantiated to {term} while it is supposed to be instantiated to a complex term in order to be callable."))
                        : throw new InvalidOperationException($"Variable {variableToCall.Name} is supposed to be instantiated in order to be callable."),
                    _ => throw new InvalidOperationException($"Can not apply call() to {currentQueryRaw.Arguments}. Only single variable is accepted.")
                };

            static IEnumerable<Rule> FindMatchingRules(
                IReadOnlyDictionary<(string FunctorName, int FunctorArity), IReadOnlyCollection<Rule>> programRules, 
                ComplexTerm query) =>
                programRules
                    .TryFind((query.Functor.Name, query.Functor.Arity))
                    .Map(rules => rules.Where(rule => Unification.IsPossible(rule.Conclusion, query)))
                    .OrElse(() => throw new InvalidOperationException($"procedure '{query.Functor.Name}/{query.Functor.Arity}' does not exist"));

                // programRules.Where(rule => Unification.IsPossible(rule.Conclusion, query));

            static Rule RenameRuleVariablesToMakeThemDifferentFromAlreadyUsedNames(Rule rule, IReadOnlySet<string> usedNames)
            {
                var renamedVariables = new Dictionary<string, Variable>();
                return Rule(
                    conclusion: RenameVariablesToMakeThemDifferentFromAlreadyUsedNames(rule.Conclusion, usedNames, renamedVariables),
                    premises: rule.Premises.Select(
                            ct => RenameVariablesToMakeThemDifferentFromAlreadyUsedNames(ct, usedNames, renamedVariables)));
            }

            static ComplexTerm RenameVariablesToMakeThemDifferentFromAlreadyUsedNames(
                ComplexTerm complexTerm, 
                IReadOnlySet<string> usedNames,
                IDictionary<string, Variable> renamedVariables) =>
                ComplexTerm(
                    complexTerm.Functor, 
                    complexTerm.Arguments.Select(
                        argument => argument switch
                        {
                            Variable v when v == _ => Variable.MakeNew(),
                            Variable v when !usedNames.Contains(v.Name) => v,
                            Variable v => renamedVariables.TryGetValue(v.Name, out var renamedVariable)
                                    ? renamedVariable
                                    : renamedVariables.AddAndReturnValue(v.Name, Variable.MakeNew()),
                            ComplexTerm ct => 
                                    RenameVariablesToMakeThemDifferentFromAlreadyUsedNames(ct, usedNames, renamedVariables),
                            _ => argument
                        }));
        }

        private static IReadOnlySet<string> ListAllMentionedVariableNames(IEnumerable<ComplexTerm> queries) =>
            new HashSet<string>(queries.SelectMany(ListAllMentionedVariableNames));

        private static IEnumerable<string> ListAllMentionedVariableNames(ComplexTerm complexTerm) =>
            complexTerm.Arguments.SelectMany(
                argument => argument switch 
                {
                    Variable v => v == _ ? Array.Empty<string>() : new[] { v.Name },
                    ComplexTerm ct => ListAllMentionedVariableNames(ct),
                    _ => Enumerable.Empty<string>()
                });

        private static ComplexTerm ApplyVariableInstantiations(
            ComplexTerm complexTerm, 
            IReadOnlyDictionary<Variable, Term> instantiations,
            bool keepUninstantiatedVariables = true) =>
            ComplexTerm(
                complexTerm.Functor,
                complexTerm.Arguments.Select(argument => ApplyVariableInstantiationsCore(argument, instantiations, keepUninstantiatedVariables)));

        internal static Term ApplyVariableInstantiationsCore(
            Term term, 
            IReadOnlyDictionary<Variable, Term> instantiations, 
            bool keepUninstantiatedVariables = true) =>
            term switch
            {
                Variable variable when !(keepUninstantiatedVariables || instantiations.ContainsKey(variable)) => _,
                Variable variable => !instantiations.TryGetValue(variable, out var value) 
                    ? term 
                    : ApplyVariableInstantiationsCore(value, instantiations, keepUninstantiatedVariables),
                ComplexTerm complexTerm => ApplyVariableInstantiations(complexTerm, instantiations, keepUninstantiatedVariables),
                _ => term
            };

        private static UnificationResult ResolveInternalInstantiations(
            UnificationResult instantiations, 
            IReadOnlySet<string> variableNames) 
        =>
            new (
                instantiations
                    .Where(it => variableNames.Contains(it.Key.Name))
                    .Select(it => KeyValuePair.Create(
                                    it.Key, 
                                    ApplyVariableInstantiationsCore(
                                        it.Value, 
                                        instantiations, 
                                        keepUninstantiatedVariables: false))));

        private static IEnumerable<Rule> CheckProgramRules(IReadOnlyCollection<Rule> programRules) =>
            programRules
                .Select(r => Builtin.TryResolveFunctor(r.Conclusion))
                .Where(conclusion => conclusion != null)
                .AsImmutable() switch 
                    { 
                        var invalidConclusions when !invalidConclusions.Any() => programRules,
                        var invalidConclusions => throw new InvalidOperationException(
                            string.Join(
                                Environment.NewLine, 
                                from c in invalidConclusions
                                select $"It's impossible to re-define built-in procedure '{c.Functor.Name}/{c.Functor.Arity}'")) 
                    };

        private static T Trace<T>(this T @this, int nestingLevel, string? description = null)
        {
            ProofEvent?.Invoke(description, nestingLevel, @this!);
            return @this;
        }
    }
}