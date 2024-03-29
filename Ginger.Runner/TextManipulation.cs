using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using Prolog.Engine.Miscellaneous;

namespace Ginger.Runner
{
    using static MayBe;

    internal readonly record struct SubstringLocation(int Start, int Length);

    internal static class TextManipulation
    {
        public static IEnumerable<string> SplitAtUpperCharacters(this string text)
        {
            var currentWordStart = 0;
            for (var i = 1; i < text.Length; ++i)
            {
                if (char.IsUpper(text[i]))
                {
                    yield return text.Substring(currentWordStart, i - currentWordStart);
                    currentWordStart = i;
                }
            }

            if (currentWordStart < text.Length)
            {
                yield return text.Substring(currentWordStart);
            }
        }

        public static string Join<T>(Func<T, T, string> getSeparator, IEnumerable<T> sequence)
        {
            var result = new StringBuilder();
            MayBe<T> previousElement = None;
            foreach (var element in sequence)
            {
                result.Append(previousElement.Map(pe => getSeparator(pe, element)).OrElse(string.Empty));
                result.Append(element?.ToString() ?? string.Empty);
                previousElement = Some(element);
            }

            return result.ToString();
        }

        public static string BuildIdentifier(
            this IEnumerable<string> @this, 
            TextInfo textInfo, 
            bool useCamelCase = false) =>
            string.Join(
                string.Empty,
                @this.Select((s, i) => useCamelCase && i == 0 ? s : textInfo.ToTitleCase(s)));

        public static string Print<T>(T value, string? enumSeparator = null, int nestingLevel = default) =>
                Prolog.Engine.PrettyPrinting.Print(
                    value, 
                    enumSeparator ?? Environment.NewLine,
                    nestingLevel,
                    Prolog.Engine.PrettyPrinting.CustomPrinter((UnderstandingFailure uf) =>
                        $"Failed to understand phrase '{uf.Sentence.Fold(parsedSentence => parsedSentence.Sentence, sentenceText => sentenceText)}'" +
                        Environment.NewLine +
                        Print(uf.FailedAttempts, enumSeparator: Environment.NewLine, nestingLevel: nestingLevel + 1)),
                    Prolog.Engine.PrettyPrinting.CustomPrinter((FailedUnderstandingAttempt failedAttempt) => 
                        $"PatternId\t{failedAttempt.PatternId}: {TextManipulation.Print(failedAttempt.FailureReason)}"),
                    Prolog.Engine.PrettyPrinting.CustomPrinter((WrongNumberOfElements r) =>
                        $"PatternText: {r.PatternText}; WrongNumberOfElements(Expected={r.ExpectedNumber}, Actual={r.ActualNumber}, Reason={r.MismatchReason})"),
                    Prolog.Engine.PrettyPrinting.CustomPrinter((MultipleUnderstandingFailureReasons r) =>
                        "MultipleUnderstandingFailureReasons [" + Environment.NewLine + "\t" +
                        TextManipulation.Print(r.Reasons).Replace(Environment.NewLine, Environment.NewLine + "\t") + 
                        Environment.NewLine + "]"),
                    Prolog.Engine.PrettyPrinting.CustomPrinter((MetaUnderstandFailure r) =>
                        $"MetaUnderstandFailure({r.Reason}, " +
                        r.CallDetails.Fold(ct => TextManipulation.Print(ct), s => s) + 
                        ")"),
                    // Prolog.Engine.PrettyPrinting.CustomPrinter((RuleBuildingRecipe ruleBuildingRecipe) =>
                    //     ?????),
                    Prolog.Engine.PrettyPrinting.CustomPrinter((ComplexTermBuildingRecipe complexTermBuildingRecipe) =>
                        complexTermBuildingRecipe.ConcreteBuilder.Fold(
                            functor => $"Functor Name: {Print(functor.FunctorBuildingRecipe)}; Arguments: {Print(functor.ArgumentBuildingRecipies, ", ")}",
                            quoteLocator => $"@understand({Print(quoteLocator)})")),
                    Prolog.Engine.PrettyPrinting.CustomPrinter((MeaningBuildingRecipe meaningBuildingRecipe) =>
                        meaningBuildingRecipe.Fold(
                            rules => Print(rules, Environment.NewLine, nestingLevel: 1), 
                            statements => Print(statements, Environment.NewLine, nestingLevel: 1))));

    }
}