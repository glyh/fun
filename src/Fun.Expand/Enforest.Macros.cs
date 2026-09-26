using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Enforest
{
    /// <summary>
    /// <c>macro name(params) [: annotation] { body }</c>: the name, the macro's value as
    /// a lambda chain, and its annotation. A type binder with no type is a
    /// <c>Syntax.R</c>, written at the binder so it carries the binder's scopes: the
    /// macro receives each binder as the reflected type it was solved to. Null when
    /// the statement declares no macro.
    /// </summary>
    private (Id Name, Syntax Value, FormKind? Kind, Syntax? Output)? ParseMacroDecl(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Macro)) return null;
        if (NameOf(stmt.Drop(1).Head) is not Id name) throw new ExpandException("a macro is written macro name(params) { body }");

        var (parameters, afterParams) = ParseFnParams(name.Span, stmt.Drop(2));
        var (kind, output, afterAnnotation) = ParseMacroAnnotation(afterParams);
        if (kind == FormKind.Decl && parameters.Any(p => p.Explicitness == Explicitness.Implicit))
            throw new ExpandException("a Decl macro binds no type parameter");
        parameters = [.. parameters.Select(p => p.Explicitness == Explicitness.Implicit && p.Type is null
            ? p with { Type = new Syntax.FieldAccess(new Syntax.Var(new Id("Syntax", p.Name.Span, p.Name.Scope)), "R", p.Name.Span) }
            : p)];

        var (body, rest, bodySpan) = ParseBody(afterAnnotation);
        EnsureNoRest("macro declaration", rest);
        var span = SourceSpan.Between(name.Span, bodySpan);
        return (name, parameters.Reverse().Aggregate(body, (acc, p) => new Syntax.Lam(p, acc, span)), kind, output);
    }

    /// <summary>
    /// A macro's annotation: <c>: Expr(T)</c>, whose <c>T</c> is the type its output
    /// promises; <c>: Expr(_)</c>, which promises nothing; <c>: Decl</c> for one
    /// declaration and <c>: List(Decl)</c> for any number, each the type its body is
    /// checked against, <c>Decl</c> written as the prelude's <c>Syntax.Decl</c> at the
    /// annotation's own scopes.
    /// </summary>
    private (FormKind?, Syntax?, Terms) ParseMacroAnnotation(Terms terms)
    {
        terms = DropSeparators(terms);
        if (!IsToken(terms.Head, TokenKind.Colon)) return (null, null, terms);
        Syntax DeclType(TokenTree decl) =>
            new Syntax.FieldAccess(new Syntax.Var(new Id("Syntax", decl.Span, ((TokenTree.Leaf)decl).Token.Scope)), "Decl", decl.Span);

        switch (terms.Tail)
        {
            case [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "Expr" } }, TokenTree.Group { Delimiter: Delimiter.Paren } group, ..]:
            {
                var rest = terms.Drop(3);
                var items = DropSeparators(new Terms(group.Items));
                if (items is [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "_" } }]) return (FormKind.Expr, null, rest);
                return (FormKind.Expr, ParseAll(items), rest);
            }
            case [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "Decl" } } decl, ..]:
                return (FormKind.Decl, DeclType(decl), terms.Drop(2));
            case [TokenTree.Leaf { Token: { Kind: TokenKind.Ident { Name: "List" } } listToken } list, TokenTree.Group { Delimiter: Delimiter.Paren } group, ..]
                when DropSeparators(new Terms(group.Items)) is [TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "Decl" } } decl]:
                return (FormKind.Decl,
                    new Syntax.Ap(new Syntax.Var(new Id("List", list.Span, listToken.Scope)), Explicitness.Explicit, DeclType(decl), group.Span),
                    terms.Drop(3));
            default:
                throw new ExpandException("a macro annotation is : Expr(T), : Expr(_), : Decl or : List(Decl)");
        }
    }

    /// <summary>
    /// A statement <c>f(args)</c> in item position: a declaration macro's call, whose
    /// head expansion resolves. Null when the statement is not a lone call.
    /// </summary>
    private Binding.MacroCall? ParseMacroCallItem(Terms stmt, bool isPublic)
    {
        if (DropSeparators(stmt) is not [TokenTree.Leaf { Token: { Kind: TokenKind.Ident id } token } head, TokenTree.Group { Delimiter: Delimiter.Paren } group])
            return null;
        RequireAdjacent(head.Span, group.Span, "function call");
        var f = new Syntax.Var(new Id(id.Name, head.Span, token.Scope));
        var args = MacroCallArgs(f, group);
        if (args is null)
        {
            var items = DropSeparators(new Terms(group.Items));
            args = items.IsEmpty
                ? [new Capture.Expr(Unit(group.Span))]
                : [.. SplitCommas(items).Select(part => (Capture)new Capture.Expr(ParseAll(part)))];
        }
        return new Binding.MacroCall(f, args.Value, isPublic);
    }

    /// <summary>
    /// A macro call's arguments, each read as its parameter's kind (M9). Null when the
    /// head names no macro: the call is an ordinary application.
    /// </summary>
    private EquatableArray<Capture>? MacroCallArgs(Syntax head, TokenTree.Group group)
    {
        if (head is not Syntax.Var { Id: var id } || _env.Roles.Resolve(id) is not { Kind: BinderMeaning.Macro, MacroParams: { } kinds })
            return null;

        var items = DropSeparators(new Terms(group.Items));
        var parts = items.IsEmpty ? [items] : SplitCommas(items);
        if (parts.Count != kinds.Length)
            throw new ExpandException($"macro {id.Name} takes {kinds.Length} arguments, the call gives {parts.Count}");

        return [.. kinds.Zip(parts, (kind, part) => ReadMacroArg(id.Name, kind, DropSeparators(part), items.IsEmpty))];
    }

    private Capture ReadMacroArg(string macro, HoleKind kind, Terms part, bool noArguments)
    {
        ExpandException Unfit() => new($"macro {macro} takes {kind} here, not what is written at {part.Span}");
        switch (kind, part)
        {
            case (HoleKind.Expr, { IsEmpty: true }) when noArguments:
                return new Capture.Expr(Unit(part.Span));
            case (HoleKind.Expr, _):
                return new Capture.Expr(ParseAll(part));
            case (HoleKind.Id, [TokenTree.Leaf { Token: { Kind: TokenKind.Ident or TokenKind.Operator } token }]):
                return new Capture.Id(token);
            case (HoleKind.Block, [TokenTree.Group { Delimiter: Delimiter.Brace } block]):
                return _env.Eager ? new Capture.Expr(ParseAll(part)) : new Capture.Block(block.Items);
            case (HoleKind.Pattern, { IsEmpty: false }):
                return new Capture.Pattern(ParsePattern(part));
            // Declarations are captured unread, as a syntax form's are: read where they are spliced.
            case (HoleKind.Decls, [TokenTree.Group { Delimiter: Delimiter.Brace } decls]):
                return new Capture.Decls(DropSeparators(new Terms(decls.Items)).IsEmpty ? [] : [new Binding.Items(decls.Items)]);
            // Exactly one declaration, written as a group holding one item.
            case (HoleKind.Decl, [TokenTree.Group { Delimiter: Delimiter.Brace } one]) when Statements(new Terms(one.Items)).Count() == 1:
                return new Capture.Decl(new Binding.Items(one.Items));
            // The argument's tokens, unread: the macro reads them itself.
            case (HoleKind.Tokens, _):
                return new Capture.Tokens(part.ToArray());
            default:
                throw Unfit();
        }
    }

    // ---- quoted syntax --------------------------------------------------------

    /// <summary>
    /// <c>quote(form)</c> and <c>quote { items }</c>: syntax written literally (M10),
    /// read now as quoted syntax. Each <c>$x</c> becomes an id spelled <c>"$x"</c> where
    /// it stands, so the form parses as written, and a reference <c>x</c> in the hole
    /// list, so the macro's own variable is resolved like any other. A hole a rule
    /// inside the quote binds is that rule's, not the quote's.
    /// </summary>
    private (Syntax, Terms)? ParseQuote(TokenTree keyword, Terms rest)
    {
        if (keyword is not TokenTree.Leaf { Token.Kind: TokenKind.Ident { Name: "quote" } }) return null;
        if (DropSeparators(rest).Head is not TokenTree.Group { Delimiter: Delimiter.Paren or Delimiter.Brace } group) return null;
        var after = DropSeparators(rest).Tail;

        var items = RewriteHoles(new Terms(group.Items));
        var free = ReplacementHoles([], items).ToHashSet();
        var holes = new List<(string, Syntax)>();
        void Collect(Terms terms)
        {
            foreach (var term in terms)
            {
                if (HoleName(term) is string hole && free.Contains(hole) && holes.All(h => h.Item1 != "$" + hole))
                {
                    var token = ((TokenTree.Leaf)term).Token;
                    holes.Add(("$" + hole, new Syntax.Var(new Id(hole, term.Span, token.Scope))));
                }
                else if (term is TokenTree.Group g) Collect(new Terms(g.Items));
            }
        }
        Collect(items);

        var quoted = new Enforest(_env.Quoted(free));
        var span = SourceSpan.Between(keyword.Span, group.Span);
        Syntax form = group.Delimiter == Delimiter.Paren
            ? new Syntax.Quote(DropSeparators(items).IsEmpty ? Unit(group.Span) : quoted.ParseAll(items), [.. holes], span)
            : new Syntax.QuoteDecls(quoted.ReadItemsNow(items), [.. holes], span);
        return (form, after);
    }
}
