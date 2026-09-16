using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Enforest
{
    /// <summary><c>import "path"</c>.</summary>
    private (Syntax, Terms) ParseImport(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Str path } leaf)
            throw new ExpandException("import is written import \"path\"");
        return (new Syntax.Import(path.Value, SourceSpan.Between(startSpan, leaf.Span)), terms.Tail);
    }

    /// <summary>
    /// A compilation unit read as its items, left unread for expansion. A unit is
    /// strict: nothing is open in it but what it opens itself.
    /// </summary>
    public static Syntax ParseUnit(string source, string? file = null)
    {
        var items = Reader.Read(source, file);
        return new Syntax.Module([new Binding.Items(items)], new Terms(items).Span);
    }
}
