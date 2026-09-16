using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary>
    /// <c>struct { items }</c>. Its items are read here, in order: <c>name : Type</c>
    /// is a constructor field, anything else a binding as in a module.
    /// </summary>
    private static (Syntax, Terms) ParseStructExpr(SourceSpan startSpan, Terms terms)
    {
        var (body, rest) = BraceBody("struct", terms);
        var bindings = new List<Binding>();
        foreach (var stmt in Statements(new Terms(body.Items)))
            bindings.AddRange(ParseStructField(stmt) is { } field ? [field] : ParseModuleStatement(stmt));
        return (new Syntax.Struct([.. bindings], SourceSpan.Between(startSpan, body.Span)), rest);
    }

    /// <summary><c>name : Type</c> with no <c>=</c> in the type: a constructor field.</summary>
    private static Binding? ParseStructField(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (stmt.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident name } || !IsToken(stmt.Drop(1).Head, TokenKind.Colon))
            return null;
        var type = stmt.Drop(2);
        if (DropSeparators(type).IsEmpty) throw new ExpandException($"missing type for struct field: {name.Name}");
        return IndexOfToken(type, TokenKind.Eq) >= 0 ? null : new Binding.Field(name.Name, ParseAll(type));
    }

    /// <summary><c>sig { name : Type; … }</c>: each item a public member whose value is its type.</summary>
    private static (Syntax, Terms) ParseSigExpr(SourceSpan startSpan, Terms terms)
    {
        var (body, rest) = BraceBody("sig", terms);
        var bindings = new List<Binding>();
        foreach (var raw in Statements(new Terms(body.Items)))
        {
            var stmt = DropSeparators(raw);
            if (IsToken(stmt.Head, TokenKind.Impl) || IsToken(stmt.Drop(2).Head, TokenKind.Impl))
                throw new NotImplementedException("not ported yet: impls in a signature");
            if (NameOf(stmt.Head) is not Id name || !IsToken(stmt.Drop(1).Head, TokenKind.Colon))
                throw new ExpandException("expected signature field name : type");
            bindings.Add(new Binding.Let(name, ParseAll(stmt.Drop(2)), Public: true, Recursive: false));
        }
        return (new Syntax.Sig([.. bindings], SourceSpan.Between(startSpan, body.Span)), rest);
    }

    /// <summary><c>P{x = 1; y = 2}</c>: fields separated by <c>;</c> or <c>,</c>.</summary>
    private static Syntax ParseRecordConstruct(Syntax type, TokenTree.Group group)
    {
        var fields = new List<(string, Syntax)>();
        foreach (var raw in Statements(new Terms(group.Items)))
        {
            var stmt = DropSeparators(raw);
            if (stmt.Head is not TokenTree.Leaf { Token.Kind: TokenKind.Ident name } || !IsToken(stmt.Drop(1).Head, TokenKind.Eq))
                throw new ExpandException("expected record field of the form name = expr");
            fields.Add((name.Name, ParseAll(stmt.Drop(2))));
        }
        return new Syntax.RecordConstruct(type, [.. fields], SourceSpan.Between(type.Span, group.Span));
    }

    private static (TokenTree.Group Body, Terms After) BraceBody(string what, Terms terms)
    {
        terms = DropSeparators(terms);
        return terms.Head is TokenTree.Group { Delimiter: Delimiter.Brace } body
            ? (body, terms.Tail)
            : throw new ExpandException($"{what} is written {what} {{ … }}");
    }

    /// <summary>A body's statements, split at top-level <c>;</c> and <c>,</c>; empty ones dropped.</summary>
    private static IEnumerable<Terms> Statements(Terms terms)
    {
        while (true)
        {
            var (stmt, after) = TakeStatement(terms);
            if (!stmt.IsEmpty) yield return stmt;
            if (after.IsEmpty) yield break;
            terms = after.Tail;
        }
    }
}
