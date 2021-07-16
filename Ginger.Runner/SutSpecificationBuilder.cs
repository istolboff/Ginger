using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Ginger.Runner.Solarix;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using SentenceMeaning = Either<IReadOnlyCollection<Rule>, IReadOnlyCollection<ComplexTerm>>;
    
    using static DomainApi;

    internal sealed class SutSpecificationBuilder
    {
        public SutSpecificationBuilder(IRussianGrammarParser grammarParser, SentenceUnderstander sentenceUnderstander)
        {
            _grammarParser = grammarParser;
            _sentenceUnderstander = sentenceUnderstander;
        }

        public void DefineEntity(string phrasing)
        {
            var entityDefinitions = Understand(phrasing)
                    .Fold(
                        rules => rules,
                        _ => throw new InvalidOperationException(
                            "When defining entities, only rule-defining sentences are alowed. " +
                            $"You're trying to use the sentence '{phrasing}' which is understood as a set of statements."));
            
            var nonFactRules = entityDefinitions.Where(ed => !ed.IsFact).AsImmutable();
            if (nonFactRules.Any())
            {
                throw new InvalidOperationException("When defining entities, only facts are allowed. " + 
                            $"The sentence '{phrasing}' produces the following non-fact rule(s): " +
                            string.Join(Environment.NewLine, nonFactRules));
            }

            _entityDefinitions.AddRange(entityDefinitions.Select(ed => ed.Conclusion));
        }

        public void DefineEffect(string phrasing)
        {
            var effectAsRules = Understand(phrasing)
                    .Fold(
                        rules => rules,
                        _ => throw new InvalidOperationException(
                            "When defining effects, only rule-defining sentences are alowed. " +
                            $"You're trying to use the sentence '{phrasing}' which is understood as a set of statements."));

            _effects.AddRange(effectAsRules.Select(MoveEntityDefinitionsFromConclusionToPremises));

            Rule MoveEntityDefinitionsFromConclusionToPremises(Rule rule)
            {
                var (conclusion, extraPremises) = RemoveEntityDefinitions(rule.Conclusion, ImmutableList<ComplexTerm>.Empty);
                if (conclusion is null)
                {
                    throw new InvalidOperationException(
                        $"Effect definition '{phrasing}' was understood as a rule that is just a set of " + 
                        $"entity definitions {string.Join(",", extraPremises)} " + 
                        "Please rephrase the effect definition so that it's understood as one or more non-trivial rules.");
                }

                if (!extraPremises.Any())
                {
                    return rule;
                }

                return Rule(conclusion, rule.Premises.Concat(extraPremises));
            }

            (ComplexTerm?, ImmutableList<ComplexTerm>) RemoveEntityDefinitions(
                ComplexTerm complexTerm,
                ImmutableList<ComplexTerm> entityDefinitions)
            {
                if (entityDefinitions.Contains(complexTerm))
                {
                    return (null, entityDefinitions);
                }

                if (IsVariableEqualityCheck(complexTerm) || 
                    IsEntityDefinition(complexTerm))
                {
                    return (null, entityDefinitions.Add(complexTerm));
                }

                var (adjustedArgumentList, extendedEntityDefinitions) = complexTerm.Arguments.Aggregate(
                    (AdjustedArgumentsList: ImmutableList<Term>.Empty, EntityDefinitions: entityDefinitions),
                    (accumulator, argument) => 
                    {
                        if (argument is ComplexTerm ct)
                        {
                            var (adjustedComplexTerm, updatedEntityDefinitions) = RemoveEntityDefinitions(ct, accumulator.EntityDefinitions);
                            return (
                                adjustedComplexTerm is null ? accumulator.AdjustedArgumentsList : accumulator.AdjustedArgumentsList.Add(adjustedComplexTerm),
                                updatedEntityDefinitions
                                );
                        }

                        return (accumulator.AdjustedArgumentsList.Add(argument), entityDefinitions);
                    });

                var complexTermWasRemovedFromList = 
                    complexTerm.Functor == Builtin.DotFunctor && adjustedArgumentList.Count != Builtin.DotFunctor.Arity;

                return complexTermWasRemovedFromList
                            ? (
                                (ComplexTerm)adjustedArgumentList.Single(), 
                                extendedEntityDefinitions
                              )
                            : (
                                complexTerm with 
                                {
                                    Functor = complexTerm.Functor with { Arity = adjustedArgumentList.Count },
                                    Arguments = new (adjustedArgumentList) 
                                }, 
                                extendedEntityDefinitions
                              );
            }
        }

        public void DefineBusinessRule(string phrasing)
        {
            var businessRules = Understand(phrasing)
                    .Fold(
                        rules => rules,
                        _ => throw new InvalidOperationException(
                            "When defining business rule, only rule-defining sentences are alowed. " +
                            $"You're trying to use the sentence '{phrasing}' which is understood as a set of statements."));

            _businessRules.AddRange(businessRules.Select(TransformBusinessRule));

            Rule TransformBusinessRule(Rule rule)
            {
                var (partialStateTerms, remainingPremises) = SplitPremises(rule.Premises);
                return Rule(
                    ComplexTerm(
                        Functor("конечноеСостояние", 2), 
                        rule.Conclusion,
                        List(partialStateTerms)),
                    remainingPremises);
            }

            (IReadOnlyCollection<ComplexTerm> PartialState, IReadOnlyCollection<ComplexTerm> RemainingPremises) SplitPremises(
                IReadOnlyCollection<ComplexTerm> businssRulePremises)
            {
                var partialStateTerms = new List<ComplexTerm>();
                var remainingPremises = new List<ComplexTerm>();
                foreach (var premise in businssRulePremises)
                {
                    if (IsVariableEqualityCheck(premise) || IsEntityDefinition(premise))
                    {
                        remainingPremises.Add(premise);
                    }
                    else
                    {
                        partialStateTerms.Add(premise);
                    }
                }

                return (partialStateTerms, remainingPremises);
            }
        }

        public void DefineBoundaryCondition(string phrasing)
        {
            throw new NotImplementedException();
        }

        public SutSpecification BuildDescription() =>
            new (
                _effects.Concat(_effects).AsImmutable(),
                Array.Empty<BusinessRule>(),
                _effects,
                new ());

        private SentenceMeaning Understand(string phrasing) =>
            _sentenceUnderstander
                .Understand(_grammarParser.ParsePreservingQuotes(phrasing))
                .OrElse(() => throw new InvalidOperationException($"Could not understand the phrase {phrasing}"))
                .Meaning;

        private static bool IsVariableEqualityCheck(ComplexTerm complexTerm) =>
            Builtin.BinaryOperators.Keys.Contains(complexTerm.Functor.Name) &&
            complexTerm.Arguments.All(argument => argument is Atom || argument is Variable);

        private bool IsEntityDefinition(ComplexTerm complexTerm) =>
            _entityDefinitions.Any(ed => Unification.IsPossible(ed, complexTerm));

        private readonly IRussianGrammarParser _grammarParser;
        private readonly SentenceUnderstander _sentenceUnderstander;
        private readonly List<ComplexTerm> _entityDefinitions = new ();
        private readonly List<Rule> _effects = new ();
        private readonly List<Rule> _businessRules = new ();
    }
}