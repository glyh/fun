using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary><c>match (scrutinee) { pattern => result, … }</c>.</summary>
    private static (Syntax, Terms) ParseMatch(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Count < 2
            || terms[0] is not TokenTree.Group { Delimiter: Delimiter.Paren } scrutinee
            || terms[1] is not TokenTree.Group { Delimiter: Delimiter.Brace } arms)
            throw new ExpandException("match is written match (scrutinee) { pattern => result, … }");

        var branches = SplitMatchBranches(new Terms(arms.Items)).Select(arm =>
        {
            var arrow = IndexOf(arm, IsFatArrow);
            if (arrow < 0) throw new ExpandException("match arm requires => between pattern and result");
            var pattern = Slice(arm, 0, arrow);
            if (IsToken(DropSeparators(pattern).Head, TokenKind.Effect))
                throw new NotImplementedException("not ported yet: effect branches");
            return new MatchBranch(ParsePattern(pattern), ParseAll(arm.Drop(arrow + 1)));
        }).ToEquatableArray();
        if (branches.IsEmpty) throw new ExpandException("match requires at least one arm");

        return (new Syntax.Match(ParseAll(new Terms([scrutinee])), branches, SourceSpan.Between(startSpan, arms.Span)),
                terms.Drop(2));
    }

    /// <summary><c>=&gt;</c> separates a pattern from its result. It lexes as an ordinary operator but is reserved.</summary>
    private static bool IsFatArrow(TokenTree term) =>
        term is TokenTree.Leaf { Token.Kind: TokenKind.Operator { Spelling: "=>" } };

    private static int IndexOf(Terms terms, Func<TokenTree, bool> pred)
    {
        for (var i = 0; i < terms.Count; i++) if (pred(terms[i])) return i;
        return -1;
    }

    /// <summary>
    /// A match body's arms. An arm's result ends at the <c>,</c> before the next
    /// arm, or at its <c>}</c> when the result is a brace group.
    /// </summary>
    private static List<Terms> SplitMatchBranches(Terms terms)
    {
        var arms = new List<Terms>();
        while (true)
        {
            terms = DropSeparators(terms);
            if (terms.IsEmpty) return arms;
            if (IsToken(terms.Head, TokenKind.Bar))
                throw new ExpandException("an arm does not begin with |: write pattern => result, … (| is pattern union)");

            var arrow = IndexOf(terms, IsFatArrow);
            if (arrow < 0)
            {
                arms.Add(terms);
                return arms;
            }

            var result = terms.Drop(arrow + 1);
            if (result.Head is TokenTree.Group { Delimiter: Delimiter.Brace })
            {
                var after = result.Tail;
                if (IsToken(after.Head, TokenKind.Comma)) after = after.Tail;
                if (!DropSeparators(after).IsEmpty && IndexOf(after, IsFatArrow) < 0)
                    throw new ExpandException(
                        "an arm whose result is { … } ends at its }: parenthesise a longer result, pattern => ({ … } …)");
                arms.Add(Slice(terms, 0, arrow + 2));
                terms = after;
                continue;
            }

            var comma = IndexOf(result, t => IsToken(t, TokenKind.Comma));
            var armResult = comma < 0 ? result : Slice(result, 0, comma);
            if (IndexOf(armResult, IsFatArrow) >= 0)
                throw new ExpandException("an arm's result ends at , before the next arm: pattern => result, pattern => …");
            arms.Add(Slice(terms, 0, arrow + 1 + armResult.Count));
            terms = comma < 0 ? Terms.Empty : result.Drop(comma + 1);
        }
    }

    // ---- patterns ---------------------------------------------------------

    /// <summary>A whole pattern: alternatives separated by <c>|</c>.</summary>
    public static Pattern ParsePattern(Terms terms)
    {
        var bar = IndexOf(terms, t => IsToken(t, TokenKind.Bar));
        if (bar >= 0)
            return new Pattern.Or(ParsePattern(Slice(terms, 0, bar)), ParsePattern(terms.Drop(bar + 1)));

        var juxtapose = DropSeparators(terms).Head is not TokenTree.Group;
        var (head, rest) = ParsePatternAtom(terms);
        (head, rest) = ParsePatternPostfix(head, rest, juxtapose);
        if (!DropSeparators(rest).IsEmpty) throw new ExpandException("unconsumed terms after pattern");
        return head;
    }

    private static (Pattern, Terms) ParsePatternAtom(Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is not TokenTree term) throw new ExpandException("expected pattern");
        var rest = terms.Tail;

        switch (term)
        {
            case TokenTree.Leaf { Token.Kind: TokenKind.Int n }:
                return (new Pattern.Atom(new Atom.I64(n.Value)), rest);
            case TokenTree.Leaf { Token.Kind: TokenKind.Char c }:
                return (new Pattern.Atom(new Atom.Char(c.Value)), rest);
            case TokenTree.Leaf { Token: { Kind: TokenKind.Ident i } token }:
                if (i.Name == "_") return (Pattern.Wild.Instance, rest);
                if (PrimitiveTypeHead(i.Name) is { } primitive) return (new Pattern.AtomType(primitive), rest);
                // A capitalised name is a constructor, anything else a binder:
                // the prototype's syntactic rule.
                var id = new Id(i.Name, term.Span, token.Scope);
                return (char.IsAsciiLetterUpper(i.Name[0]) ? new Pattern.Con(new Syntax.Var(id), []) : new Pattern.Bind(id), rest);
            case TokenTree.Group { Delimiter: Delimiter.Paren } group:
            {
                var items = DropSeparators(new Terms(group.Items));
                if (items.IsEmpty) return (new Pattern.Atom(Atom.Unit.Instance), rest);
                var parts = SplitCommas(items);
                return (parts.Count == 1
                    ? ParsePattern(parts[0])
                    : new Pattern.Prod([.. parts.Select(ParsePattern)]), rest);
            }
            case TokenTree.Leaf { Token.Kind: var kind } when kind == TokenKind.Struct:
                return ParseStructTypePattern(rest);
            default:
                throw new ExpandException("unsupported pattern");
        }
    }

    /// <summary>
    /// A constructor's dotted path, its argument list, or - unless the pattern
    /// began with a group - juxtaposed arguments.
    /// </summary>
    private static (Pattern, Terms) ParsePatternPostfix(Pattern lhs, Terms terms, bool juxtapose)
    {
        while (true)
        {
            terms = DropSeparators(terms);
            if (terms.Head is not TokenTree term) return (lhs, terms);

            if (IsToken(term, TokenKind.Dot) && terms.Count > 1)
            {
                var field = TokenText(terms[1]) ?? throw new ExpandException("expected pattern name after '.'");
                var span = SourceSpan.Between(term.Span, terms[1].Span);
                lhs = lhs switch
                {
                    Pattern.Con { Args.IsEmpty: true } c => c with { Head = new Syntax.FieldAccess(c.Head, field, SourceSpan.Between(c.Head.Span, span)) },
                    Pattern.Bind b => new Pattern.Con(new Syntax.FieldAccess(new Syntax.Var(b.Name), field, SourceSpan.Between(b.Name.Span, span)), []),
                    _ => throw new ExpandException("only constructor patterns can be qualified"),
                };
                terms = terms.Drop(2);
                continue;
            }

            if (term is TokenTree.Group { Delimiter: Delimiter.Paren } args)
            {
                var items = DropSeparators(new Terms(args.Items));
                var parsed = items.IsEmpty ? [] : SplitCommas(items).Select(ParsePattern).ToEquatableArray();
                lhs = lhs is Pattern.Con { Args.IsEmpty: true } c
                    ? c with { Args = parsed }
                    : throw new ExpandException("only constructor patterns can take arguments");
                terms = terms.Tail;
                juxtapose = false;
                continue;
            }

            if (term is TokenTree.Group { Delimiter: Delimiter.Brace } fields)
            {
                lhs = lhs is Pattern.Con { Args.IsEmpty: true } c
                    ? ParseRecordPattern(c.Head, fields)
                    : throw new ExpandException("record pattern fields must follow a type name");
                terms = terms.Tail;
                juxtapose = false;
                continue;
            }

            if (juxtapose && lhs is Pattern.Con con && IsPatternArgumentStart(term))
            {
                var (arg, after) = ParsePatternAtom(terms);
                lhs = con with { Args = con.Args.Add(arg) };
                terms = after;
                continue;
            }

            return (lhs, terms);
        }
    }

    /// <summary>What can begin a juxtaposed constructor argument: an expression start that is a pattern atom.</summary>
    private static bool IsPatternArgumentStart(TokenTree term) => term switch
    {
        TokenTree.Leaf { Token.Kind: TokenKind.Int or TokenKind.Char or TokenKind.Str or TokenKind.Ident } => true,
        TokenTree.Group { Delimiter: Delimiter.Paren } => true,
        _ => false,
    };
}
