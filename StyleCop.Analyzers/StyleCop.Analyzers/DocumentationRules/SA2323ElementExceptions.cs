// Licensed under the Apache License, Version 2.0. See LICENSE in the project root for license information.

namespace StyleCop.Analyzers.DocumentationRules
{
    using System;
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using System.Linq;
    using Microsoft.CodeAnalysis;
    using Microsoft.CodeAnalysis.CSharp;
    using Microsoft.CodeAnalysis.CSharp.Syntax;
    using Microsoft.CodeAnalysis.Diagnostics;
    using StyleCop.Analyzers.Helpers;

    /// <summary>
    /// The documentation describing the exceptions thrown by a method does not match the actually thrown exceptions.
    /// </summary>
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal sealed class SA2323ElementExceptions : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "SA2323";
        private const string Title = "All exceptions must be documented";
        private const string Description = "The documentation describing the exceptions of a C# method, constructor, delegate or indexer element does not match the actual exceptions thrown by the element.";
        private const string HelpLink = "https://github.com/DotNetAnalyzers/StyleCopAnalyzers/blob/master/documentation/SA1612.md";

        private const string MissingExceptionDocumentationMessageFormat = "The exception '{0}' is not documented.";
        private const string ParamWrongOrderMessageFormat = "The parameter documentation for '{0}' should be at position {1}.";

        private static readonly DiagnosticDescriptor MissingExceptionRefDescriptor =
            new DiagnosticDescriptor(DiagnosticId, Title, MissingExceptionDocumentationMessageFormat, AnalyzerCategory.DocumentationRules, DiagnosticSeverity.Warning, AnalyzerConstants.EnabledByDefault, Description, HelpLink);

        private static readonly DiagnosticDescriptor OrderDescriptor =
                   new DiagnosticDescriptor(DiagnosticId, Title, ParamWrongOrderMessageFormat, AnalyzerCategory.DocumentationRules, DiagnosticSeverity.Warning, AnalyzerConstants.EnabledByDefault, Description, HelpLink);

        private static readonly Action<CompilationStartAnalysisContext> CompilationStartAction = HandleCompilationStart;
        // private static readonly Action<SyntaxNodeAnalysisContext> DocumentationTriviaAction = HandleDocumentationTrivia;
        private static readonly Action<SyntaxNodeAnalysisContext> ThrowStatementAction = HandleThrowStatement;

        /// <inheritdoc/>
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        { get; } =
            ImmutableArray.Create(MissingExceptionRefDescriptor);

        /// <inheritdoc/>
        public override void Initialize(AnalysisContext context)
        {
            context.RegisterCompilationStartAction(CompilationStartAction);
        }

        private static void HandleCompilationStart(CompilationStartAnalysisContext context)
        {
            context.RegisterSyntaxNodeActionHonorExclusions(ThrowStatementAction, SyntaxKind.ThrowStatement);
            // context.RegisterSyntaxNodeActionHonorExclusions(DocumentationTriviaAction, SyntaxKind.SingleLineDocumentationCommentTrivia);
        }

        private static void HandleThrowStatement(SyntaxNodeAnalysisContext context)
        {
            ThrowStatementSyntax syntax = context.Node as ThrowStatementSyntax;

            var exception = (ObjectCreationExpressionSyntax)syntax.Expression;
            TypeSyntax extype = exception.Type;
            SyntaxToken exnameToken = ((IdentifierNameSyntax)extype).Identifier;
            string exname = exnameToken.ValueText;

            DocumentationCommentTriviaSyntax doc = GetDocNode(syntax);
            IEnumerable<XmlElementSyntax> exnodes = doc.Content
                .Where(node => GetName(node)?.ToString() == XmlCommentHelper.ExceptionXmlTag)
                .Select(xnode => (XmlElementSyntax)xnode);

            if (exnodes.FirstOrDefault() == null)
            {
                return;
            }
            int i = 0;
            foreach (XmlElementSyntax exnode in exnodes)
            {
                HandleElement(context, exnode, exname, i, extype.GetLocation());
                ++i;
            }
        }
        /*
        private static void HandleDocumentationTrivia(SyntaxNodeAnalysisContext context)
        {
            DocumentationCommentTriviaSyntax syntax = context.Node as DocumentationCommentTriviaSyntax;

            // Find the type parameters of the parent node
            IEnumerable<string> parentParametersEnumerable = GetDocNode(syntax);

            if (parentParametersEnumerable == null)
            {
                return;
            }

            ImmutableArray<string> parentParameters = parentParametersEnumerable.ToImmutableArray();

            ImmutableArray<XmlNodeSyntax> nodes = syntax.Content
                .Where(node => string.Equals(GetName(node)?.ToString(), XmlCommentHelper.ParamXmlTag))
                .ToImmutableArray();

            for (int i = 0; i < nodes.Length; i++)
            {
                HandleElement(context, nodes[i], parentParameters, i, GetName(nodes[i])?.GetLocation());
            }
        }
        */
        private static XmlNameSyntax GetName(XmlNodeSyntax element)
        {
            return (element as XmlElementSyntax)?.StartTag?.Name
                ?? (element as XmlEmptyElementSyntax)?.Name;
        }

        private static void HandleElement(SyntaxNodeAnalysisContext context, XmlElementSyntax element, string exceptionType, int index, Location alternativeDiagnosticLocation)
        {
            var nameAttribute = XmlCommentHelper.GetFirstAttributeOrDefault<XmlCrefAttributeSyntax>(element);
            string ex = nameAttribute?.Cref.ToString();
            if (string.IsNullOrWhiteSpace(ex))
            {
                return;
            }

            // TODO: more than one
            // TODO: namespaces
            if (ex != exceptionType)
            {
                context.ReportDiagnostic(
                    Diagnostic.Create(
                        MissingExceptionRefDescriptor,
                        alternativeDiagnosticLocation ?? nameAttribute.GetLocation(),
                        exceptionType));
            }
        }

        /// <summary>
        /// Checks if the given <see cref="SyntaxNode"/> has a <see cref="BaseMethodDeclarationSyntax"/>, <see cref="IndexerDeclarationSyntax"/> or a <see cref="DelegateDeclarationSyntax"/>
        /// as one of its parent. If it finds one of those three with a valid type parameter list it returns a <see cref="IEnumerable{T}"/> containing the names of all parameters.
        /// </summary>
        /// <param name="node">The node the analysis should start at.</param>
        /// <returns>
        /// A <see cref="IEnumerable{T}"/> containing all parameters or null, of no valid parent could be found.
        /// </returns>
        private static DocumentationCommentTriviaSyntax GetDocNode(SyntaxNode node)
        {
            var methodParent = node.FirstAncestorOrSelf<MethodDeclarationSyntax>();
            if (methodParent != null)
            {
                return methodParent.GetDocumentationCommentTriviaSyntax();
            }
            /*
            var delegateParent = node.FirstAncestorOrSelf<DelegateDeclarationSyntax>();
            if (delegateParent != null)
            {
                return delegateParent.ParameterList?.Parameters.Select(x => x.Identifier.ValueText) ?? Enumerable.Empty<string>();
            }

            var indexerParent = node.FirstAncestorOrSelf<IndexerDeclarationSyntax>();
            if (indexerParent != null)
            {
                return indexerParent.ParameterList?.Parameters.Select(x => x.Identifier.ValueText) ?? Enumerable.Empty<string>();
            }
            */
            return null;
        }
    }
}