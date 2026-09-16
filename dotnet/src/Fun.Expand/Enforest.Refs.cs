using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary><c>ref(e)</c>, or <c>ref e</c> reading one tight expression.</summary>
    private static (Syntax, Terms) ParseRef(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is TokenTree.Group { Delimiter: Delimiter.Paren } group)
            return (new Syntax.RefNew(ParseRefArg(group), SourceSpan.Between(startSpan, group.Span)), terms.Tail);
        if (terms.IsEmpty) throw new ExpandException("ref requires an argument");
        var (tight, rest) = ParseExprPrec(terms, Prec.Tight);
        return (new Syntax.RefNew(tight, SourceSpan.Between(startSpan, tight.Span)), rest);
    }

    /// <summary><c>deref(r)</c>: the argument is always parenthesised.</summary>
    private static (Syntax, Terms) ParseDeref(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree.Group { Delimiter: Delimiter.Paren } group)
            throw new ExpandException("deref requires a parenthesized argument");
        return (new Syntax.RefGet(ParseRefArg(group), SourceSpan.Between(startSpan, group.Span)), terms.Tail);
    }

    private static Syntax ParseRefArg(TokenTree.Group group)
    {
        var items = DropSeparators(new Terms(group.Items));
        return items.IsEmpty ? Unit(group.Span) : ParseAll(items);
    }
}
