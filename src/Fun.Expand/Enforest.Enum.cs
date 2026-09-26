using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Enforest
{
    /// <summary><c>enum { Name, Name(Type, …), … }</c>.</summary>
    private (Syntax, Terms) ParseEnumExpr(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
            throw new ExpandException("enum is written enum { … }");

        var constructors = SplitCommas(DropSeparators(new Terms(body.Items)))
            .Select(DropSeparators)
            .Where(item => !item.IsEmpty)
            .Select(item => item switch
            {
                { Count: 1 } when TokenText(item[0]) is { } name && item[0] is TokenTree.Leaf { Token.Kind: TokenKind.Ident } =>
                    new EnumConstructor(name, []),
                { Count: 2 } when item[0] is TokenTree.Leaf { Token.Kind: TokenKind.Ident i }
                                  && item[1] is TokenTree.Group { Delimiter: Delimiter.Paren } payloads =>
                    new EnumConstructor(i.Name, SplitCommas(DropSeparators(new Terms(payloads.Items)))
                        .Where(p => !DropSeparators(p).IsEmpty)
                        .Select(ParseAll)
                        .ToEquatableArray()),
                _ => throw new ExpandException("an enum constructor is written Name or Name(Type, …)"),
            })
            .ToEquatableArray();

        return (new Syntax.Enum(constructors, SourceSpan.Between(startSpan, body.Span)), terms.Tail);
    }
}
