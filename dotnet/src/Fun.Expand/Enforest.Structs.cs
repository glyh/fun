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
            bindings.AddRange((ParseStructField(stmt) ?? ParseMethod(stmt)) is { } item ? [item] : ParseModuleStatement(stmt));
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

    /// <summary>
    /// <c>[pub] method name(params) [: T] { body }</c>. Unlike <c>fn()</c>, an empty
    /// parameter list binds nothing: the method is a function of <c>self</c> alone.
    /// </summary>
    private static Binding? ParseMethod(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        var isPublic = IsToken(stmt.Head, TokenKind.Pub);
        if (isPublic) stmt = stmt.Tail;
        if (!IsToken(stmt.Head, TokenKind.Method)) return null;
        if (NameOf(stmt.Drop(1).Head) is not Id name)
            throw new ExpandException("method declaration requires a name");
        if (stmt.Drop(2).Head is not TokenTree.Group { Delimiter: Delimiter.Paren } group)
            throw new ExpandException($"method declaration requires a parenthesized parameter list: {name.Name}");
        RequireAdjacent(name.Span, group.Span, "method parameter list");

        var items = DropSeparators(new Terms(group.Items));
        EquatableArray<Param> parameters = items.IsEmpty ? [] : ParseParamGroup(items, Explicitness.Explicit);
        var (result, afterResult) = ParseResultType(stmt.Drop(3));
        var (body, rest, _) = ParseBody(afterResult);
        EnsureNoRest("method declaration", rest);
        return new Binding.Method(name, parameters, result is null ? body : new Syntax.Annotated(body, result, body.Span), isPublic);
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
            {
                bindings.Add(ParseSignatureImpl(stmt));
                continue;
            }
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
