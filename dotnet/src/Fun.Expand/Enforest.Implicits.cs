using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Enforest
{
    /// <summary>
    /// A bracket group in expression position: <c>[A : Type, B] -&gt; body</c>,
    /// one implicit arrow per binder. A binder written without a type ranges over
    /// <c>Unit</c>, as in the prototype. The codomain reads to the end of the terms.
    /// </summary>
    private (Syntax, Terms) ParseBracketPrimary(TokenTree.Group group, Terms rest)
    {
        var after = DropSeparators(rest);
        if (!IsToken(after.Head, TokenKind.ThinArrow))
            throw new ExpandException("bare bracket expression is not supported");

        var binders = ParseParamGroup(new Terms(group.Items), Explicitness.Implicit);
        var codomain = ParseAll(after.Tail);
        var arrow = binders.Reverse().Aggregate(codomain, (acc, p) =>
            new Syntax.Arrow(Explicitness.Implicit, p.Name, p.Type ?? UnitType(p.Name.Span), null, acc,
                SourceSpan.Between(group.Span, acc.Span)));
        return (arrow, Terms.Empty);
    }

    /// <summary><c>f[A, B]</c>: implicit arguments, curried. The list must touch its callee.</summary>
    private Syntax ParseImplicitApplication(Syntax fn, TokenTree.Group group)
    {
        RequireAdjacent(fn.Span, group.Span, "implicit argument list");
        var span = SourceSpan.Between(fn.Span, group.Span);
        return SplitCommas(new Terms(group.Items))
            .Select(ParseAll)
            .Aggregate(fn, (f, arg) => new Syntax.Ap(f, Explicitness.Implicit, arg, span));
    }
}
