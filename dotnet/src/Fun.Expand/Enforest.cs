using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// Enforestation: token trees to <see cref="Syntax"/>. A <c>{ … }</c> body is
/// not read here -- it becomes a <see cref="Syntax.Block"/> whose statements the
/// expander reads one at a time, so a declaration can bind the syntax the
/// statements after it are read with.
/// </summary>
// Slice 1 reads only what the prelude-free conformance cases need. Every form
// the prototype supports but this does not throws by name rather than parsing
// into something else -- a silently wrong parse is the failure this port is
// most at risk of.
public static partial class Enforest
{
    /// <summary>
    /// Where an expression is read, which decides what may continue it: after
    /// <c>-&gt;</c>, a tight argument no infix operator continues, or the operand
    /// of an operator, continued only by operators that bind tighter.
    /// </summary>
    public abstract record Prec
    {
        public static readonly Prec Top = new Position("Top"), ArrowRhs = new Position("ArrowRhs"), Tight = new Position("Tight");

        private sealed record Position(string Name) : Prec;

        public sealed record Operand(string Name, Role Role) : Prec;
    }

    /// <summary>A source read as an expression: one body, read as expansion reaches it.</summary>
    public static Syntax ParseExpr(string source, string? file = null)
    {
        var terms = Reader.Read(source, file);
        return new Syntax.Block(terms, new Terms(terms).Span);
    }

    // ---- statements -------------------------------------------------------

    /// <summary>
    /// The first statement of a <c>{ … }</c> body as a form scoping over the
    /// rest, which stays unread until expansion reaches it. A trailing <c>;</c>
    /// discards the body's value.
    /// </summary>
    public static Syntax ParseBlockHead(SourceSpan span, Terms terms)
    {
        var (stmt, rest) = TakeStatement(terms);
        if (stmt.IsEmpty) throw new ExpandException("empty block");
        if (rest.IsEmpty) return ParseAll(stmt);

        rest = DropSeparators(rest);
        var body = rest.IsEmpty
            ? Unit(span)
            : (Syntax)new Syntax.Block(rest.ToArray(), rest.Span);
        return DoStatement(span, stmt, body);
    }

    /// <summary>A block statement, as the form it makes of the rest of the block.</summary>
    private static Syntax DoStatement(SourceSpan span, Terms stmt, Syntax body)
    {
        if (ParseRecGroup(stmt) is { } group) return new Syntax.LetRecGroup(group, body, span);

        if (ParseRoleDecl(stmt) is var (roleName, role)) return new Syntax.SyntaxDef(roleName, role, body, span);

        if (ParsePatternSynonym(stmt) is var (synName, synonym)) return new Syntax.Let(synName, null, synonym, body, false, span);

        var decl = ParseValueDeclStatement(stmt);
        if (decl is var (name, type, value, recursive))
            return new Syntax.Let(name, type, value, body, recursive, span);

        if (ParseOpenStatement(stmt) is { } opened)
            return new Syntax.Open(opened, body, "", span);

        if (ParseEffectDecl(stmt) is var (effectName, effectParams, ops))
            return new Syntax.EffectDef(effectName, effectParams, ops, body, span);
        if (ParseTraitOrImplStatement(span, stmt, body) is { } declared) return declared;

        // Not a binding: the statement is an expression whose value is discarded.
        return new Syntax.Let(new Id("_", span), null, ParseAll(stmt), body, false, span);
    }

    /// <summary>
    /// <c>name = value</c>, <c>name : T = value</c>, <c>rec name = value</c> or
    /// <c>fn name(params) { … }</c>. Null when the statement binds nothing.
    /// </summary>
    private static (Id Name, Syntax? Type, Syntax Value, bool Recursive)? ParseValueDeclStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        var recursive = false;
        if (stmt.Head is TokenTree.Leaf { Token.Kind: var k } && k == TokenKind.Rec)
        {
            recursive = true;
            stmt = DropSeparators(stmt.Tail);
        }

        // `fn name(params) { body }`
        if (stmt.Head is TokenTree.Leaf { Token.Kind: var fnKind } && fnKind == TokenKind.Fn
            && NameOf(stmt.Drop(1).Head) is Id fnName)
        {
            // The parameter list touches the name, not the `fn` keyword.
            var (value, rest) = ParseFn(fnName.Span, stmt.Drop(2));
            EnsureNoRest("function declaration", rest);
            return (fnName, null, value, recursive);
        }

        if (NameOf(stmt.Head) is not Id name) return null;

        var afterName = stmt.Tail;
        var eq = IndexOfToken(afterName, TokenKind.Eq);
        if (eq < 0) return null;

        var beforeEq = DropSeparators(TakeTerms(afterName, eq));
        var valueTerms = DropSeparators(afterName.Drop(eq + 1));
        if (valueTerms.IsEmpty) throw new ExpandException($"missing value for binding: {name.Name}");

        Syntax? type = null;
        if (!beforeEq.IsEmpty)
        {
            if (beforeEq.Head is not TokenTree.Leaf { Token.Kind: var colon } || colon != TokenKind.Colon)
                throw new ExpandException(
                    "binding parameters are not supported; use fn name(params) syntax");
            type = ParseAll(beforeEq.Tail);
        }

        return (name, type, ParseAll(valueTerms), recursive);
    }

    // ---- modules ----------------------------------------------------------

    /// <summary>
    /// <c>module { items }</c>. The items stay unread: expansion reads them one
    /// form at a time, so an item can bind the syntax the ones after it are read with.
    /// </summary>
    private static (Syntax, Terms) ParseModuleExpr(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
            throw new ExpandException("module is written module { … }");
        EquatableArray<Binding> items = _env is { Eager: true } ? ReadItemsNow(new Terms(body.Items)) : [new Binding.Items(body.Items)];
        return (new Syntax.Module(items, SourceSpan.Between(startSpan, body.Span)), terms.Tail);
    }

    /// <summary><c>open e</c>: the module expression, or null when the statement is not an open.</summary>
    private static Syntax? ParseOpenStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Open)) return null;
        return ParseAll(stmt.Tail);
    }

    /// <summary>
    /// One module item: <c>[pub] name [: T] = value</c>, <c>[pub] fn name(…) { … }</c>
    /// or <c>open e</c>. A typed binding's value is annotated with its type.
    /// </summary>
    public static EquatableArray<Binding> ParseModuleStatement(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (stmt.IsEmpty) return [];

        // A lone `$d` is a declaration hole: only quoted syntax spells an id with `$`.
        if (stmt is [var lone] && HoleName(lone) is not null && NameOf(lone) is Id hole) return [new Binding.Hole(hole)];

        var isPublic = IsToken(stmt.Head, TokenKind.Pub);
        var unprefixed = isPublic ? stmt.Tail : stmt;

        if (DeclFormUse(unprefixed) is { } declUse) return [new Binding.Instantiate(declUse, isPublic)];
        if (ParseRoleDecl(unprefixed) is var (roleName, role)) return [new Binding.SyntaxDecl(roleName, role, isPublic)];

        if (ParseExportStatement(isPublic, unprefixed) is { } export) return [export];

        if (ParseOpenStatement(unprefixed) is { } opened)
        {
            if (isPublic) throw new ExpandException("open is not a public item");
            return [new Binding.Open(opened, "")];
        }

        if (ParseRecGroup(unprefixed) is { } group) return [new Binding.RecGroup(group, isPublic)];

        if (ParsePatternSynonym(unprefixed) is var (synName, synonym)) return [new Binding.Let(synName, synonym, isPublic, false)];

        if (ParseEffectDecl(unprefixed) is var (effectName, effectParams, ops))
            return [new Binding.Effect(effectName, effectParams, ops, isPublic)];

        if (ParseValueDeclStatement(unprefixed) is var (name, type, value, recursive))
        {
            var annotated = type is null ? value : new Syntax.Annotated(value, type, unprefixed.Span);
            return [new Binding.Let(name, annotated, isPublic, recursive)];
        }

        if (ParseTraitOrImplItem(unprefixed, isPublic) is { } item) return [item];

        throw new NotImplementedException(
            $"not ported yet: module item starting `{(unprefixed.Head is { } head ? Describe(head) : "(empty)")}`");
    }

    private static string Describe(TokenTree term) => term switch
    {
        TokenTree.Leaf l => l.Token.Kind.Text(),
        TokenTree.Group { Delimiter: Delimiter.Paren } => "(...)",
        TokenTree.Group { Delimiter: Delimiter.Bracket } => "[...]",
        _ => "{...}",
    };

    // ---- expressions ------------------------------------------------------

    /// <summary>Reads one expression and requires that it consumed every term.</summary>
    public static Syntax ParseAll(Terms terms)
    {
        var (expr, rest) = ParseExprPrec(terms, Prec.Top);
        EnsureNoRest("expression", rest);
        return expr;
    }

    public static (Syntax, Terms) ParseExprPrec(Terms terms, Prec prec)
    {
        var (lhs, rest) = ParsePrimary(terms);
        return ParsePostfix(lhs, rest, prec);
    }

    private static (Syntax, Terms) ParsePrimary(Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree term) throw new ExpandException("expected expression");
        var rest = terms.Tail;

        if (PrefixRoleUse(term, rest) is var (roleUse, afterRoleUse)) return (roleUse, afterRoleUse);

        switch (term)
        {
            case TokenTree.Leaf { Token: var token }:
                switch (token.Kind)
                {
                    case TokenKind.Int n:
                        return (new Syntax.Atom(new Atom.I64(n.Value), term.Span), rest);
                    case TokenKind.Str s:
                        return (new Syntax.Atom(new Atom.Str(s.Value), term.Span), rest);
                    case TokenKind.Char c:
                        return (new Syntax.Atom(new Atom.Char(c.Value), term.Span), rest);
                    case TokenKind.Ident i:
                        return (new Syntax.Var(new Id(i.Name, term.Span, token.Scope)), rest);
                    case TokenKind.Word w when w == TokenKind.Fn:
                        return ParseFn(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Module:
                        return ParseModuleExpr(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Match: return ParseMatch(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Enum: return ParseEnumExpr(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Struct:
                        return ParseStructExpr(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Self:
                        return (new Syntax.Self(term.Span), rest);
                    case TokenKind.Word w when w == TokenKind.SelfType:
                        return (new Syntax.SelfType(term.Span), rest);
                    case TokenKind.Word w when w == TokenKind.Sig:
                        return ParseSigExpr(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Import:
                        return ParseImport(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Perform: return ParsePerform(term.Span, rest);
                    case TokenKind.Word w when w == TokenKind.Resume: return ParseResume(term.Span, rest);
                    case TokenKind.Word w:
                        throw new NotImplementedException($"not ported yet: the `{w.Spelling}` form");
                    case TokenKind.Operator o:
                        throw new ExpandException($"unsupported prefix operator: {o.Spelling}");
                }
                break;

            case TokenTree.Group { Delimiter: Delimiter.Bracket } bracket:
                return ParseBracketPrimary(bracket, rest);

            case TokenTree.Group g:
                return (ParseGroupExpr(g), rest);
        }
        throw new ExpandException("unexpected token in expression");
    }

    private static (Syntax, Terms) ParsePostfix(Syntax lhs, Terms terms, Prec prec)
    {
        while (true)
        {
            if (terms.Head is not TokenTree term || IsSeparator(term)) return (lhs, terms);

            // `A -> B`: a function type. A bare arrow is pure; `A ->{E} B` and
            // `A ~> B` carry a row.
            if ((IsToken(term, TokenKind.ThinArrow) || IsPolyArrow(term)) && (prec == Prec.Top || prec == Prec.ArrowRhs))
            {
                var (row, afterRow) = ParseArrowRow(term, terms.Tail);
                var (cod, afterCod) = ParseExprPrec(afterRow, Prec.ArrowRhs);
                var span = SourceSpan.Between(lhs.Span, cod.Span);
                // `(x : A) -> B` names its domain; anything else is anonymous.
                lhs = lhs is Syntax.Annotated { Inner: Syntax.Var v, Type: var dom }
                    ? new Syntax.Arrow(Explicitness.Explicit, v.Id, dom, row, cod, span)
                    : new Syntax.Arrow(Explicitness.Explicit, null, lhs, row, cod, span);
                terms = afterCod;
                continue;
            }

            if (term is TokenTree.Leaf { Token.Kind: var colon } && colon == TokenKind.Colon)
            {
                var (type, afterType) = ParseExprPrec(terms.Tail, Prec.Top);
                lhs = new Syntax.Annotated(lhs, type, SourceSpan.Between(lhs.Span, type.Span));
                terms = afterType;
                continue;
            }

            // `f(a, b)`: a call, curried. The group must touch the callee.
            if (term is TokenTree.Group { Delimiter: Delimiter.Paren } call)
            {
                RequireAdjacent(lhs.Span, call.Span, "function call");
                var callSpan = SourceSpan.Between(lhs.Span, call.Span);
                var items = new Terms(call.Items);
                var args = DropSeparators(items).IsEmpty
                    ? [Unit(call.Span)]
                    : SplitCommas(items).Select(ParseAll);
                lhs = args.Aggregate(lhs, (f, arg) =>
                    new Syntax.Ap(f, Explicitness.Explicit, arg, callSpan));
                terms = terms.Tail;
                continue;
            }

            // `e.0` projects a tuple, `e.field` reads a member.
            if (term is TokenTree.Leaf { Token.Kind: var dot } && dot == TokenKind.Dot && terms.Count > 1)
            {
                var field = terms[1];
                var span = SourceSpan.Between(lhs.Span, field.Span);
                lhs = field switch
                {
                    TokenTree.Leaf { Token.Kind: TokenKind.Int i } => new Syntax.Proj(lhs, (int)i.Value, span),
                    _ when TokenText(field) is string n => new Syntax.FieldAccess(lhs, n, span),
                    _ => throw new ExpandException("expected field name or projection after '.'"),
                };
                terms = terms.Drop(2);
                continue;
            }

            if (term is TokenTree.Group { Delimiter: Delimiter.Brace } record && lhs.Span.End == record.Span.Start
                && IndexOfToken(new Terms(record.Items), TokenKind.Eq) >= 0)
            {
                (lhs, terms) = (ParseRecordConstruct(lhs, record), terms.Tail);
                continue;
            }

            if (term is TokenTree.Group { Delimiter: Delimiter.Bracket } implicitArgs)
            {
                lhs = ParseImplicitApplication(lhs, implicitArgs);
                terms = terms.Tail;
                continue;
            }

            // `f{ e }` with no `=`: an implicit argument written in braces.
            if (term is TokenTree.Group { Delimiter: Delimiter.Brace } postfix
                && lhs.Span.End == postfix.Span.Start)
                throw new NotImplementedException("not ported yet: an implicit argument written f{ e }");

            // An infix operator is a declared role. An identifier with none ends
            // the expression; an operator with none may be the prelude's.
            if (TokenText(term) is string symbol)
            {
                if (InfixRoleUse(lhs, term, terms.Tail, prec) is var (use, afterUse)) { (lhs, terms) = (use, afterUse); continue; }
                if (_env?.Roles.FindRole(symbol, Fixity.Infix, ((TokenTree.Leaf)term).Token.Scope) is not null
                    || term is TokenTree.Leaf { Token.Kind: TokenKind.Ident }) return (lhs, terms);
                throw new NotImplementedException($"not ported yet: the infix operator `{symbol}`");
            }

            return (lhs, terms);
        }
    }

    private static Syntax ParseGroupExpr(TokenTree.Group group)
    {
        var items = DropSeparators(new Terms(group.Items));
        switch (group.Delimiter)
        {
            case Delimiter.Brace:
                return ReadBlock(group.Items, group.Span);

            case Delimiter.Bracket:
                throw new NotImplementedException("not ported yet: bracket expressions");

            default:
                if (items.IsEmpty) return Unit(group.Span);
                var colon = IndexOfToken(items, TokenKind.Colon);
                if (colon >= 0)
                    return new Syntax.Annotated(
                        ParseAll(TakeTerms(items, colon)), ParseAll(items.Drop(colon + 1)), group.Span);

                var parts = SplitCommas(items);
                // The group's own span, so `(f)(x)` sees the call as adjacent.
                return parts.Count == 1
                    ? ParseAll(parts[0]) with { Span = group.Span }
                    : new Syntax.Prod([.. parts.Select(ParseAll)], group.Span);
        }
    }

    // ---- fn ---------------------------------------------------------------

    /// <summary>
    /// <c>fn(x, y : A) { body }</c>, curried into one <see cref="Syntax.Lam"/>
    /// per parameter. <c>fn(x) : T { … }</c> annotates the whole function with
    /// its arrow type, so the body is checked against <c>T</c>.
    /// </summary>
    private static (Syntax, Terms) ParseFn(SourceSpan startSpan, Terms terms)
    {
        // `fn[A : Type](x : A)`: an implicit list, then an explicit one, each
        // touching what precedes it. Either may be absent, not both.
        terms = DropSeparators(terms);
        EquatableArray<Param> implicits = [];
        var previous = startSpan;
        if (terms.Head is TokenTree.Group { Delimiter: Delimiter.Bracket } implicitGroup)
        {
            RequireAdjacent(startSpan, implicitGroup.Span, "implicit fn parameter list");
            implicits = ParseParamGroup(new Terms(implicitGroup.Items), Explicitness.Implicit);
            previous = implicitGroup.Span;
            terms = DropSeparators(terms.Tail);
        }

        EquatableArray<Param> explicits = [];
        if (terms.Head is TokenTree.Group { Delimiter: Delimiter.Paren } group)
        {
            RequireAdjacent(previous, group.Span, "explicit fn parameter list");
            explicits = ParseParamGroup(new Terms(group.Items), Explicitness.Explicit);
            terms = terms.Tail;
        }
        else if (implicits.IsEmpty)
            throw new ExpandException("fn requires at least one parameter list");

        EquatableArray<Param> parameters = [.. implicits, .. explicits];
        var (result, row, afterResult) = ParseResult(terms);
        var (body, rest, bodySpan) = ParseBody(afterResult);
        var span = SourceSpan.Between(startSpan, bodySpan);

        var lam = parameters.Reverse().Aggregate(body, (acc, p) => new Syntax.Lam(p, acc, span));
        var value = result is null
            ? lam
            : new Syntax.Annotated(lam, FunctionType(span, parameters, result, row), span);
        return (value, rest);
    }

    private static EquatableArray<Param> ParseParamGroup(Terms items, Explicitness explicitness)
    {
        items = DropSeparators(items);
        if (items.IsEmpty)
            // `fn() { … }` takes one unit parameter; `fn[]` binds nothing and is an error.
            return explicitness == Explicitness.Explicit
                ? [new Param(new Id("_", items.Span), UnitType(items.Span), Explicitness.Explicit)]
                : throw new ExpandException("empty implicit parameter list");
        return [.. SplitCommas(items).Select(item => ParseParamItem(item, explicitness))];
    }

    private static Param ParseParamItem(Terms terms, Explicitness explicitness)
    {
        terms = DropSeparators(terms);
        if (NameOf(terms.Head) is not Id name)
            throw new ExpandException("expected parameter of the form name or name : Type");
        var rest = DropSeparators(terms.Tail);
        if (rest.IsEmpty) return new Param(name, null, explicitness);
        if (rest.Head is not TokenTree.Leaf { Token.Kind: var colon } || colon != TokenKind.Colon)
            throw new ExpandException("expected parameter of the form name or name : Type");
        // `[A : {Eq, Show}]`: the traits an implicit binder must implement.
        if (explicitness == Explicitness.Implicit && rest.Count == 2
            && rest[1] is TokenTree.Group { Delimiter: Delimiter.Brace } bounds)
            return new Param(name, ParseTraitBoundSet(bounds), explicitness);
        return new Param(name, ParseAll(rest.Tail), explicitness);
    }

    /// <summary>
    /// An optional result type before a body. Brackets decide grouping: the type
    /// ends at the first top-level <c>{ … }</c>, so a type holding braces is
    /// parenthesised.
    /// </summary>
    private static (Syntax?, Terms) ParseResultType(Terms terms)
    {
        var (type, row, rest) = ParseResult(terms);
        if (row is not null) throw new NotImplementedException("not ported yet: an effect row on a method result");
        return (type, rest);
    }

    private static (Syntax, Terms, SourceSpan) ParseBody(Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree.Group { Delimiter: Delimiter.Brace } group)
            throw new ExpandException("expected { body } after fn parameters");
        return (ReadBlock(group.Items, group.Span), terms.Tail, group.Span);
    }

    /// <summary>
    /// <c>fn(p1 : A, …) : T</c>'s type: one arrow per parameter. Every parameter
    /// needs its type, since the annotation states the whole function's.
    /// </summary>
    private static Syntax FunctionType(SourceSpan span, EquatableArray<Param> parameters, Syntax result, EffectRow? row)
    {
        if (parameters.IsEmpty) throw new ExpandException("a result type needs a parameter list");
        var type = result;
        // The innermost arrow - the one that runs the body - carries the row.
        var innermost = true;
        foreach (var p in parameters.Reverse())
        {
            var domain = p.Type
                ?? throw new ExpandException($"a result type needs every parameter's type: {p.Name.Name}");
            type = new Syntax.Arrow(p.Explicitness, p.Name, domain, innermost ? row : null, type, span);
            innermost = false;
        }
        return type;
    }

    // ---- term helpers -----------------------------------------------------

    private static Syntax Unit(SourceSpan span) => new Syntax.Atom(Atom.Unit.Instance, span);

    private static Syntax UnitType(SourceSpan span) => new Syntax.Var(new Id("Unit", span));

    private static bool IsSeparator(TokenTree term) =>
        term is TokenTree.Leaf { Token.Kind: var k } && k == TokenKind.Semi;

    public static Terms DropSeparators(Terms terms)
    {
        var n = 0;
        while (n < terms.Count && IsSeparator(terms[n])) n++;
        return terms.Drop(n);
    }

    /// <summary>The first statement of a definition context, and the terms after it.</summary>
    public static (Terms Stmt, Terms After) TakeStatement(Terms terms)
    {
        terms = DropSeparators(terms);
        var n = 0;
        while (n < terms.Count && !IsSeparator(terms[n]) && !IsToken(terms[n], TokenKind.Comma)) n++;
        return (TakeTerms(terms, n), terms.Drop(n));
    }

    private static List<Terms> SplitCommas(Terms terms)
    {
        var parts = new List<Terms>();
        var start = 0;
        for (var i = 0; i < terms.Count; i++)
        {
            if (!IsToken(terms[i], TokenKind.Comma)) continue;
            parts.Add(Slice(terms, start, i));
            start = i + 1;
        }
        parts.Add(Slice(terms, start, terms.Count));
        return parts;
    }

    private static bool IsToken(TokenTree? term, TokenKind kind) =>
        term is TokenTree.Leaf { Token.Kind: var k } && k == kind;

    private static int IndexOfToken(Terms terms, TokenKind kind)
    {
        for (var i = 0; i < terms.Count; i++) if (IsToken(terms[i], kind)) return i;
        return -1;
    }

    private static Terms TakeTerms(Terms terms, int count) => Slice(terms, 0, count);

    private static Terms Slice(Terms terms, int from, int to)
    {
        var items = ImmutableArray.CreateBuilder<TokenTree>(to - from);
        for (var i = from; i < to; i++) items.Add(terms[i]);
        return new Terms(new EquatableArray<TokenTree>(items.ToImmutable()));
    }

    /// <summary>The name a term binds, when it is a bare identifier.</summary>
    private static Id? NameOf(TokenTree? term) =>
        term is TokenTree.Leaf { Token: { Kind: TokenKind.Ident i } t }
            ? new Id(i.Name, term.Span, t.Scope)
            : null;

    private static string? TokenText(TokenTree term) => term switch
    {
        TokenTree.Leaf { Token.Kind: TokenKind.Ident i } => i.Name,
        TokenTree.Leaf { Token.Kind: TokenKind.Operator o } => o.Spelling,
        _ => null,
    };

    /// <summary>
    /// A call's argument list must touch its callee: whitespace application is
    /// not the language.
    /// </summary>
    private static void RequireAdjacent(SourceSpan lhs, SourceSpan rhs, string what)
    {
        if (!lhs.IsSynthetic && !rhs.IsSynthetic && lhs.End != rhs.Start)
            throw new ExpandException($"{what} must be adjacent to the callee; whitespace application is not supported");
    }

    private static void EnsureNoRest(string what, Terms rest)
    {
        if (!DropSeparators(rest).IsEmpty) throw new ExpandException($"{what} has trailing terms");
    }
}
