using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary>
    /// <c>P {x = p, y; _}</c>'s fields, separated by <c>,</c> or <c>;</c>. A lone
    /// label writes a binder of its name; <c>_</c> makes the pattern partial.
    /// </summary>
    private static Pattern.Record ParseRecordPattern(Syntax type, TokenTree.Group group)
    {
        var fields = new List<(string, Pattern)>();
        var partial = false;
        var items = new Terms(group.Items);
        var start = 0;
        for (var i = 0; i <= items.Count; i++)
        {
            if (i < items.Count && !IsToken(items[i], TokenKind.Comma) && !IsSeparator(items[i])) continue;
            var part = Slice(items, start, i);
            start = i + 1;
            if (part.IsEmpty) continue;

            switch (part.Head)
            {
                case TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "_" } } when part.Count == 1:
                    partial = true;
                    break;
                case TokenTree.Leaf { Token: { Kind: TokenKind.Ident label } token } leaf when part.Count == 1:
                    fields.Add((label.Name, new Pattern.Bind(new Id(label.Name, leaf.Span, token.Scope))));
                    break;
                case TokenTree.Leaf { Token.Kind: TokenKind.Ident named } when part.Count > 2 && IsToken(part[1], TokenKind.Eq):
                    fields.Add((named.Name, ParsePattern(part.Drop(2))));
                    break;
                default:
                    throw new ExpandException("expected record pattern field of the form name or name = pat");
            }
        }
        return new Pattern.Record(type, [.. fields], partial);
    }

    /// <summary>The primitive type a pattern names by its spelling: the prototype's syntactic rule for type-case heads.</summary>
    private static AtomTy? PrimitiveTypeHead(string name) => name switch
    {
        "I64" => AtomTy.I64,
        "Unit" => AtomTy.Unit,
        "Char" => AtomTy.Char,
        "String" => AtomTy.String,
        "Absurd" => AtomTy.Absurd,
        _ => null,
    };

    /// <summary><c>struct { x : p; _ }</c> after its keyword: each field's type pattern; <c>_</c> makes it partial.</summary>
    private static (Pattern, Terms) ParseStructTypePattern(Terms rest)
    {
        rest = DropSeparators(rest);
        if (rest.Head is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
            throw new ExpandException("struct type pattern is written struct { field: pattern; _ }");

        var fields = new List<(string, Pattern)>();
        var partial = false;
        var items = new Terms(body.Items);
        var start = 0;
        for (var i = 0; i <= items.Count; i++)
        {
            if (i < items.Count && !IsSeparator(items[i])) continue;
            var part = Slice(items, start, i);
            start = i + 1;
            if (part.IsEmpty) continue;

            switch (part.Head)
            {
                case TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "_" } } when part.Count == 1:
                    partial = true;
                    break;
                case TokenTree.Leaf { Token.Kind: TokenKind.Ident field } when part.Count > 2 && IsToken(part[1], TokenKind.Colon):
                    fields.Add((field.Name, ParsePattern(part.Drop(2))));
                    break;
                default:
                    throw new ExpandException("expected struct type pattern field");
            }
        }
        return (new Pattern.StructType([.. fields], partial), rest.Tail);
    }
}
