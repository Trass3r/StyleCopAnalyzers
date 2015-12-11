// Licensed under the Apache License, Version 2.0. See LICENSE in the project root for license information.

namespace StyleCop.Analyzers.Test.DocumentationRules
{
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Analyzers.DocumentationRules;
    using Microsoft.CodeAnalysis.Diagnostics;
    using TestHelper;
    using Xunit;

    /// <summary>
	/// This class contains unit tests for <see cref="SA2323ElementExceptions"/>.
    /// </summary>
    public class SA2323UnitTests : DiagnosticVerifier
    {
        [Fact]
        public async Task TestMemberWithoutDocumentationAsync()
        {
            var testCode = @"
using System;
class ClassName
{
    /// <exception cref=""Application Exception""></exception>
    public void ExceptionsNotCommentedExceptionCreated()
    {
        throw new ApplicationException(""some exception"");
    }
}";
            var expected = new[]
            {
                this.CSharpDiagnostic().WithLocation(8, 19).WithArguments("ApplicationException")
            };

            await this.VerifyCSharpDiagnosticAsync(testCode, expected, CancellationToken.None).ConfigureAwait(false);
        }

        protected override IEnumerable<DiagnosticAnalyzer> GetCSharpDiagnosticAnalyzers()
        {
            yield return new SA2323ElementExceptions();
        }
    }
}
