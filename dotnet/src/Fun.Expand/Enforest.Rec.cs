using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary>
    /// <c>rec a = … and b : T = …</c>: every member, a typed one's value annotated
    /// with its type. Null unless the statement is a <c>rec</c> with at least two
    /// members; a single <c>rec</c> is an ordinary recursive binding.
    /// </summary>
    private static EquatableArray<RecMember>? ParseRecGroup(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Rec)) return null;

        var segments = SplitOnAnd(stmt.Tail);
        if (segments.Count < 2) return null;

        var members = segments.Select(segment =>
            ParseValueDeclStatement(segment) is var (name, type, value, _)
                ? new RecMember(name, type is null ? value : new Syntax.Annotated(value, type, segment.Span))
                : throw new ExpandException("expected name = value in a rec … and … group")).ToList();

        if (members.GroupBy(m => m.Name.Name).FirstOrDefault(g => g.Count() > 1) is { } duplicate)
            throw new ExpandException($"duplicate name in a rec … and … group: {duplicate.Key}");
        return [.. members];
    }

    /// <summary>The statement split at every top-level <c>and</c>.</summary>
    private static List<Terms> SplitOnAnd(Terms terms)
    {
        var segments = new List<Terms>();
        var start = 0;
        for (var i = 0; i < terms.Count; i++)
        {
            if (terms[i] is not TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "and" } }) continue;
            segments.Add(Slice(terms, start, i));
            start = i + 1;
        }
        segments.Add(Slice(terms, start, terms.Count));
        return segments;
    }
}
