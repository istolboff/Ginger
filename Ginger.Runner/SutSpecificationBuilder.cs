using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Ginger.Runner.Solarix;
using Prolog.Engine;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using static DomainApi;
    using static Either;
    using static MayBe;
    using static TextManipulation;

    internal sealed record EntityDefiniton(
        ComplexTerm Definition, 
        MayBe<WordOrQuotation<DisambiguatedWord>> FunctorNameWords);

    internal sealed class SutSpecificationBuilder
    {
        public SutSpecificationBuilder(IRussianGrammarParser grammarParser, SentenceUnderstander sentenceUnderstander)
        {
            _grammarParser = grammarParser;
            _sentenceUnderstander = sentenceUnderstander;
        }

        public void DefineEntity(string phrasing)
        {
            var understoodSentence = Understand(phrasing); 

            var rules = understoodSentence.MeaningWithRecipe.Fold(
                        rules => rules,
                        _ => throw new InvalidOperationException(
                            "When defining entities, only rule-defining sentences are alowed. " +
                            $"You're trying to use the sentence '{phrasing}' which is understood as a set of statements."));

            var (facts, nonFactRules) = rules.Segregate<RuleWithRecipe, RuleWithRecipe, Rule>(
                                    r => r.Rule.IsFact switch { true => Left(r), _ => Right(r.Rule)});
            if (nonFactRules.Any())
            {
                throw new InvalidOperationException("When defining entities, only facts are allowed. " + 
                            $"The sentence '{phrasing}' produces the following non-fact rule(s): " +
                            Print(nonFactRules));
            }

            _entityDefinitions.AddRange(
                facts.Select(fact => 
                    new EntityDefiniton(
                        fact.Rule.Conclusion, 
                        from recipe in fact.Recipe.ConclusionBuildingRecipe.FunctorNameRecipe
                        from subTree in recipe.TryLocateCompleteSubTreeOfNameElements(understoodSentence.Sentence)
                        select subTree)));
        }

        public void DefineEffect(string phrasing)
        {
            var effectAsRules = Understand(phrasing).MeaningWithRecipe
                    .Fold(
                        rules => rules,
                        _ => throw new InvalidOperationException(
                            "When defining effects, only rule-defining sentences are alowed. " +
                            $"You're trying to use the sentence '{phrasing}' which is understood as a set of statements."));

            _effects.AddRange(effectAsRules.Select(it => MoveEntityDefinitionsFromConclusionToPremises(it.Rule)));

            Rule MoveEntityDefinitionsFromConclusionToPremises(Rule rule)
            {
                var (conclusion, extraPremises) = RemoveEntityDefinitions(rule.Conclusion, ImmutableList<ComplexTerm>.Empty);
                if (conclusion is null)
                {
                    throw new InvalidOperationException(
                        $"Effect definition '{phrasing}' was understood as a rule that is just a set of " + 
                        $"entity definitions {Print(extraPremises, ",")} " + 
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
            var businessRules = Understand(phrasing).MeaningWithRecipe
                    .Fold(
                        rules => rules,
                        _ => throw new InvalidOperationException(
                            "When defining business rule, only rule-defining sentences are alowed. " +
                            $"You're trying to use the sentence '{phrasing}' which is understood as a set of statements."));

            _businessRules.AddRange(businessRules.Select(r => TransformBusinessRule(r.Rule)));

            BusinessRule TransformBusinessRule(Rule rule)
            {
                var (partialStateTerms, remainingPremises) = SplitPremises(rule.Premises);
                return new (rule.Conclusion, partialStateTerms, remainingPremises);
            }

            (IReadOnlyCollection<ComplexTerm> PartialState, IReadOnlyCollection<ComplexTerm> RemainingPremises) SplitPremises(
                IReadOnlyCollection<ComplexTerm> businssRulePremises)
            =>
                businssRulePremises.Segregate(
                    premise => 
                        IsVariableEqualityCheck(premise) || IsEntityDefinition(premise)
                            ? Right<ComplexTerm, ComplexTerm>(premise)
                            : Left(premise));
        }

        public void DefineInitialState(string phrasing)
        {
            _initialStates.AddRange(
                Understand(phrasing).MeaningWithRecipe.Fold(
                    rules => rules.Select(r => InitialState.DeconstructRule(r.Rule)),
                    stateComponents => new InitialState(stateComponents.ConvertAll(sc => sc.ComplexTerm)).ToImmutable()));
        }

        public SutSpecification BuildDescription() =>
            new (
                _entityDefinitions.ConvertAll(ed => ed.Definition),
                _effects,
                _businessRules,
                _initialStates);

        private UnderstoodSentence Understand(string phrasing)
        {
            var sentence = _grammarParser.ParsePreservingQuotes(phrasing);
            var understandingOutcome = _sentenceUnderstander.Understand(
                    sentence, 
                    CreateMeaningBuilder());
            
            return understandingOutcome.Fold(
                failedAttempts => throw UnderstandingFailed(
                                    phrasing, 
                                    understandingOutcome.Left!.Concat(failedAttempts)),
                understoodSentence => understoodSentence);
        }

        private UnderstandingOutcome UnderstandCore(ParsedSentence sentence)
        {
            var understandingOutcome = _sentenceUnderstander.Understand(sentence, CreateMeaningBuilder());
            if (understandingOutcome.IsRight)
            {
                return understandingOutcome;
            }

            var woundSentence = TryWoundStructuresInSentence(sentence);
            if (!woundSentence.HasValue)
            {
                return understandingOutcome;
            }

            var woundSentenceUnderstanding = 
                    _sentenceUnderstander.Understand(
                        woundSentence.Value!.Sentence,
                        OverrideMeaningBuilder(
                            CreateMeaningBuilder(), 
                            woundSentence.Value!.WoundEntities));

            return woundSentenceUnderstanding.MapLeft(
                failedAttempts => understandingOutcome.Left!.Concat(failedAttempts).AsImmutable());
        }

        private static InvalidOperationException UnderstandingFailed(
            string phrasing, 
            IEnumerable<FailedUnderstandingAttempt> failedUnderstandingAttempts)
        {
            return new InvalidOperationException(
                $"Could not understand the phrase{Environment.NewLine}\t{phrasing}:{Environment.NewLine}" + 
                Print(failedUnderstandingAttempts));
        }

        private MayBe<WoundSentence> TryWoundStructuresInSentence(ParsedSentence sentence)
        {
            var woundEntities = 
               (from entityDefinition in _entityDefinitions
                where entityDefinition.Definition.Arguments.Count == 1 && 
                      entityDefinition.Definition.Arguments.Single() is Atom && 
                      entityDefinition.FunctorNameWords.HasValue
                group entityDefinition by entityDefinition.Definition.Functor.Name into g
                let ed = g.First()
                from foundStructurePosition in TryFindStructures(ed.FunctorNameWords.Value!, sentence.SentenceStructure)
                select new WoundEntity(
                        foundStructurePosition, 
                        variable => ed.Definition with { Arguments = new (variable) })
                ).AsImmutable();

            if (!woundEntities.Any())
            {
                return None;
            }

            var sentenceWithWoundSubstructures = ReplaceStructuresWithTheirRoots(
                sentence.SentenceStructure, 
                woundEntities.Select(we => we.PositionInSentence).ToHashSet());

            var oldPositionsToNewPositionsMap = sentenceWithWoundSubstructures
                .IterateByPosition()
                .Select((it, index) => (it.PositionInSentence, index))
                .ToDictionary(
                    it => it.PositionInSentence,
                    it => it.index);

            return Some(
                new WoundSentence(
                    ParsedSentence.From(
                        sentenceWithWoundSubstructures.Transform(
                            it => it with 
                                    { 
                                        PositionInSentence = oldPositionsToNewPositionsMap[it.PositionInSentence]
                                    })),
                    woundEntities.ConvertAll(
                            it => it with 
                                    { 
                                        PositionInSentence = oldPositionsToNewPositionsMap[it.PositionInSentence] 
                                    })));

            static IEnumerable<int> TryFindStructures(
                WordOrQuotation<DisambiguatedWord> structure, 
                WordOrQuotation<Word> inSentence)
            =>
                inSentence
                    .IterateDepthFirst()
                    .Where(currentSubTree => Match(structure, currentSubTree))
                    .Select(woq => woq.PositionInSentence);

            static WordOrQuotation<Word> ReplaceStructuresWithTheirRoots(
                WordOrQuotation<Word> woq,
                ICollection<int> stopSubtreesAtIndexes)
            =>
                woq with
                {
                    Children = stopSubtreesAtIndexes.Contains(woq.PositionInSentence)
                                    ? Array.Empty<WordOrQuotation<Word>>()
                                    : woq.Children.ConvertAll(c => 
                                            ReplaceStructuresWithTheirRoots(c, stopSubtreesAtIndexes))
                };

            static bool Match(WordOrQuotation<DisambiguatedWord> structure, WordOrQuotation<Word> subTree) =>
                structure.IterateDepthFirst().ToArray() is var expectedStructure &&
                subTree.IterateDepthFirst().ToArray() is var actualStructure &&
                expectedStructure.Length == actualStructure.Length &&
                expectedStructure
                    .Zip(actualStructure)
                    .Select((pair, index) => (ExpectedWord: pair.First.Word, ActualWord: pair.Second.Word, Index: index))
                    .AsImmutable() is var allPairs &&
                allPairs.All(it => 
                   (from expectedWord in it.ExpectedWord
                    from actualWord in it.ActualWord
                    select (it.Index == 0 || expectedWord.LeafLinkType == actualWord.LeafLinkType) &&
                            actualWord.LemmaVersions.Any(matchedLemmaVersion => 
                                matchedLemmaVersion.EntryId == expectedWord.LemmaVersion.EntryId))
                    .OrElse(false));
        }

        private MeaningBuilder CreateMeaningBuilder() =>
            MeaningBuilder.Create(sentence => UnderstandCore(_grammarParser.ParsePreservingQuotes(sentence)));

        private static MeaningBuilder OverrideMeaningBuilder(
            MeaningBuilder originalMeaningBuilder,
            IReadOnlyCollection<WoundEntity> woundEntities)
        {
            var involvedWoundEntities = new Dictionary<WoundEntity, Variable>();

            return originalMeaningBuilder with 
            {
                BuildAtom =
                        (meaningBuilder, atomRecipe, sentence) =>
                            (atomRecipe.AtomContentBuilder.NameComponentGetters.Count == 1 &&
                                atomRecipe.AtomContentBuilder.NameComponentGetters.Single() is var singleName &&
                                woundEntities.FirstOrDefault(we => we.PositionInSentence == singleName.PositionInSentence) is var relevantWoundEntity &&
                                relevantWoundEntity != null)
                            ? Right<FailedUnderstandingAttempt, Term>(
                                involvedWoundEntities.GetOrCreate(
                                    relevantWoundEntity, 
                                    () => Prolog.Engine.Variable.MakeNew(isTemporary: false)))
                            : originalMeaningBuilder.BuildAtom(meaningBuilder, atomRecipe, sentence),
                BuildAllMeaningStatements =
                        (meaningBuilder, statementRecipies, sentence) =>
                        {
                            var statements = originalMeaningBuilder.BuildAllMeaningStatements(meaningBuilder, statementRecipies, sentence);
                            return !involvedWoundEntities.Any()
                                    ? statements
                                    : statements.Map(s => s
                                        .Concat(involvedWoundEntities.Select(we => 
                                            new ComplexTermWithRecipe(
                                                we.Key.EntityReferenceTemplate(we.Value), 
                                                None)))
                                        .AsImmutable());
                        }
            };
        }

        private static bool IsVariableEqualityCheck(ComplexTerm complexTerm) =>
            Builtin.BinaryOperators.Keys.Contains(complexTerm.Functor.Name) &&
            complexTerm.Arguments.All(argument => argument is Atom || argument is Variable);

        private bool IsEntityDefinition(ComplexTerm complexTerm) =>
            _entityDefinitions.Any(ed => Unification.IsPossible(ed.Definition, complexTerm));

        private readonly IRussianGrammarParser _grammarParser;
        private readonly SentenceUnderstander _sentenceUnderstander;
        private readonly List<EntityDefiniton> _entityDefinitions = new ();
        private readonly List<Rule> _effects = new ();
        private readonly List<BusinessRule> _businessRules = new ();
        private readonly List<InitialState> _initialStates = new ();

        private record WoundSentence(ParsedSentence Sentence, IReadOnlyCollection<WoundEntity> WoundEntities);

        private record WoundEntity(int PositionInSentence, Func<Variable, ComplexTerm> EntityReferenceTemplate);
    }
}