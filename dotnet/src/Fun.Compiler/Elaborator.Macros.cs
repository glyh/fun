using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary><c>quote(…)</c> is an <c>Expr</c>; <c>quote { … }</c> is the list of its items.</summary>
    private static (Term, Value) InferQuote(Context ctx, Syntax quote)
    {
        var r = Reflection.OfPrelude;
        return quote switch
        {
            Syntax.Quote q => (QuoteTerm(ctx, r.ReflectExpr(q.Template), q.Holes), r.ExprType),
            Syntax.QuoteDecls q => (QuoteTerm(ctx, r.ReflectDecls(q.Items), q.Holes), r.DeclsType),
            _ => throw new InvalidOperationException($"not a quote: {quote.GetType().Name}"),
        };
    }

    /// <summary>
    /// <c>quote { … }</c> where one <c>Decl</c> is expected -- a <c>: Decl</c> macro's body --
    /// is that one declaration; anywhere else it is the list of its items.
    /// </summary>
    private static Term? CheckQuoteDecl(Context ctx, Syntax.QuoteDecls quote, Value expected)
    {
        var r = Reflection.OfPrelude;
        if (!Nbe.Convertible(ctx.Metas, ctx.Width, expected, r.DeclType)) return null;
        return quote.Items is [var item] and not [Binding.Hole]
            ? QuoteTerm(ctx, r.ReflectDecl(item), quote.Holes)
            : throw new FunException($"quote {{ … }} where one declaration is expected holds {quote.Items.Length}");
    }

    /// <summary>
    /// A quote's template and holes: each hole is checked at the reflection type its
    /// positions in the template give it; one hole in positions of two kinds is an error.
    /// </summary>
    private static Term QuoteTerm(Context ctx, Value template, EquatableArray<(string Hole, Syntax Value)> holes)
    {
        var r = Reflection.OfPrelude;
        var occurrences = QuoteHoles.Occurrences(template);
        return new Term.Quote(template, [.. holes.Select(h =>
        {
            Value expected = occurrences.Where(o => o.Hole == h.Hole).Select(o => o.Kind).Distinct().ToList() switch
            {
                [QuoteHoles.Kind.Expr] => r.ExprType,
                [QuoteHoles.Kind.Pattern] => r.PatternType,
                [QuoteHoles.Kind.Decl] => r.DeclsType,
                [QuoteHoles.Kind.Id] => r.IdType,
                _ => throw new FunException($"the quote hole {h.Hole} stands in positions of different kinds"),
            };
            return (h.Hole, Check(ctx, h.Value, expected));
        })]);
    }
}
