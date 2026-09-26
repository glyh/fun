using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Enforest
{
    /// <summary>
    /// <c>effect Name(Params) = sig { op : A -&gt; B; … }</c> (or <c>module { op = A -&gt; B }</c>).
    /// Null when the statement is not an effect declaration.
    /// </summary>
    private (Id Name, EquatableArray<Id> Params, EquatableArray<EffectOp> Ops)? ParseEffectDecl(Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Effect)) return null;
        if (NameOf(stmt.Drop(1).Head) is not Id name) throw new ExpandException("effect declaration requires a name");

        var afterName = stmt.Drop(2);
        var eq = IndexOfToken(afterName, TokenKind.Eq);
        if (eq < 0) throw new ExpandException("effect binding requires =");

        EquatableArray<Id> parameters = [];
        var paramTerms = DropSeparators(TakeTerms(afterName, eq));
        if (!paramTerms.IsEmpty)
        {
            if (paramTerms.Count != 1 || paramTerms.Head is not TokenTree.Group { Delimiter: Delimiter.Paren } group)
                throw new ExpandException("effect parameters must be written as (A, B)");
            RequireAdjacent(name.Span, group.Span, "effect parameter list");
            var items = DropSeparators(new Terms(group.Items));
            if (items.IsEmpty) throw new ExpandException("effect parameter list cannot be empty");
            parameters = [.. SplitCommas(items).Select(item => NameOf(DropSeparators(item) is { Count: 1 } one ? one.Head : null)
                ?? throw new ExpandException("effect parameter list expects identifiers"))];
        }

        return (name, parameters, ParseEffectOps(afterName.Drop(eq + 1)));
    }

    /// <summary>An effect's operations: each an explicit arrow with no row of its own.</summary>
    private EquatableArray<EffectOp> ParseEffectOps(Terms terms)
    {
        terms = DropSeparators(terms);
        var signature = IsToken(terms.Head, TokenKind.Sig);
        if (terms.Count != 2 || !(signature || IsToken(terms.Head, TokenKind.Module))
            || terms[1] is not TokenTree.Group { Delimiter: Delimiter.Brace } body)
            throw new ExpandException("effect requires a module { … } or sig { … } block");

        var ops = new List<EffectOp>();
        var rest = new Terms(body.Items);
        while (true)
        {
            var (stmt, after) = TakeStatement(rest);
            rest = DropSeparators(after is { IsEmpty: false } && IsToken(after.Head, TokenKind.Comma) ? after.Tail : after);
            if (!stmt.IsEmpty)
            {
                var separator = signature ? TokenKind.Colon : TokenKind.Eq;
                if (NameOf(stmt.Head) is not Id opName || !IsToken(stmt.Drop(1).Head, separator))
                    throw new ExpandException(signature
                        ? "expected effect sig field of the form name : Type"
                        : "expected effect module field of the form name = Type");
                ops.Add(ParseEffectOp(opName.Name, ParseAll(stmt.Drop(2))));
            }
            if (rest.IsEmpty) return [.. ops];
        }
    }

    private EffectOp ParseEffectOp(string name, Syntax type) => type switch
    {
        Syntax.Arrow { Row: not null } => throw new ExpandException("effect operation types cannot have latent effects"),
        Syntax.Arrow { Explicitness: Explicitness.Explicit } arrow => new EffectOp(name, arrow.Domain, arrow.Codomain),
        Syntax.Arrow => throw new ExpandException("effect operation types must be explicit function types"),
        _ => throw new ExpandException("effect operation requires a function type"),
    };

    /// <summary><c>perform E.op arg</c>: a dotted path, then the argument, read tightly.</summary>
    private (Syntax, Terms) ParsePerform(SourceSpan startSpan, Terms terms)
    {
        var (operation, rest) = ParseOperationPath(terms);
        var (arg, after) = ParseExprPrec(rest, Prec.Tight);
        return (new Syntax.Perform(operation, arg, SourceSpan.Between(startSpan, arg.Span)), after);
    }

    /// <summary>
    /// An operation path <c>E.op</c> or <c>M.E.op</c>: a bare head, then members.
    /// The last member is the operation.
    /// </summary>
    private (Syntax.FieldAccess, Terms) ParseOperationPath(Terms terms)
    {
        terms = DropSeparators(terms);
        if (NameOf(terms.Head) is not Id head) throw new ExpandException("expected dotted identifier");
        Syntax path = new Syntax.Var(head);
        var rest = terms.Tail;
        while (IsToken(rest.Head, TokenKind.Dot) && NameOf(rest.Drop(1).Head) is Id member)
        {
            path = new Syntax.FieldAccess(path, member.Name, SourceSpan.Between(path.Span, member.Span));
            rest = rest.Drop(2);
        }
        return path is Syntax.FieldAccess operation
            ? (operation, rest)
            : throw new ExpandException("an effect operation is written E.op");
    }

    /// <summary><c>resume(arg)</c> or <c>resume arg</c>.</summary>
    private (Syntax, Terms) ParseResume(SourceSpan startSpan, Terms terms)
    {
        terms = DropSeparators(terms);
        if (terms.Head is TokenTree.Group { Delimiter: Delimiter.Paren } group)
        {
            var items = DropSeparators(new Terms(group.Items));
            var arg = items.IsEmpty ? Unit(group.Span) : ParseAll(items);
            return (new Syntax.Resume(arg, SourceSpan.Between(startSpan, group.Span)), terms.Tail);
        }
        if (terms.IsEmpty) throw new ExpandException("resume requires an argument");
        var (tight, rest) = ParseExprPrec(terms, Prec.Tight);
        return (new Syntax.Resume(tight, SourceSpan.Between(startSpan, tight.Span)), rest);
    }

    /// <summary>An effect branch's head: <c>effect E.op pattern</c>.</summary>
    private (Syntax.FieldAccess, Pattern) ParseEffectBranchHead(Terms terms)
    {
        var (operation, rest) = ParseOperationPath(DropSeparators(terms).Tail);
        return (operation, ParsePattern(rest));
    }

    /// <summary>
    /// A row: <c>E, F</c>, <c>E | r</c>, <c>| r1, r2</c>, <c>_</c> (inferred) or
    /// <c>E | _</c>. An empty row is pure.
    /// </summary>
    private EffectRow ParseEffectRow(TokenTree.Group group)
    {
        var terms = DropSeparators(new Terms(group.Items));
        if (terms.IsEmpty) return new EffectRow([], [], Inferred: false, Polymorphic: false);

        bool IsWildcard(Terms ts) => DropSeparators(ts) is { Count: 1 } one && NameOf(one.Head) is { Name: "_" };
        EquatableArray<Syntax> Entries(Terms ts) =>
            DropSeparators(ts).IsEmpty ? [] : [.. SplitCommas(ts).Select(ParseAll)];

        var bar = IndexOfToken(terms, TokenKind.Bar);
        if (bar < 0)
            return IsWildcard(terms)
                ? new EffectRow([], [], Inferred: true, Polymorphic: false)
                : new EffectRow(Entries(terms), [], Inferred: false, Polymorphic: false);

        var effects = Entries(TakeTerms(terms, bar));
        var tails = terms.Drop(bar + 1);
        return IsWildcard(tails)
            ? new EffectRow(effects, [], Inferred: true, Polymorphic: false)
            : new EffectRow(effects, Entries(tails), Inferred: false, Polymorphic: false);
    }

    /// <summary>
    /// <c>~&gt;</c>: an arrow whose row its signature decides. It is a base role, so
    /// it is recognised by the role its token resolves to, never by spelling.
    /// </summary>
    private bool IsPolyArrow(TokenTree? term) =>
        term is TokenTree.Leaf leaf && TokenText(leaf) is string symbol
        && _env.Roles.FindRole(symbol, Fixity.Infix, leaf.Token.Scope) is { Meaning: RoleMeaning.PolyArrow };

    private EffectRow PolymorphicRow(bool inferred) => new([], [], inferred, Polymorphic: true);

    /// <summary>
    /// An arrow's row, when one is written: <c>-&gt;{E}</c> (a brace group touching
    /// the arrow) or <c>~&gt;</c>. Returns the row and the terms after it.
    /// </summary>
    private (EffectRow? Row, Terms After) ParseArrowRow(TokenTree arrow, Terms afterArrow)
    {
        if (IsPolyArrow(arrow)) return (PolymorphicRow(inferred: false), afterArrow);
        return afterArrow.Head is TokenTree.Group { Delimiter: Delimiter.Brace } row && arrow.Span.End == row.Span.Start
            ? (ParseEffectRow(row), afterArrow.Tail)
            : (null, afterArrow);
    }

    /// <summary>
    /// A result before a body: <c>: T</c> when pure, <c>-&gt;{E} T</c> or <c>~&gt; T</c>
    /// when effectful. Brackets decide grouping: the type ends at the first
    /// top-level <c>{ … }</c> after the row.
    /// </summary>
    private (Syntax? Type, EffectRow? Row, Terms After) ParseResult(Terms terms)
    {
        var start = DropSeparators(terms);
        EffectRow? row;
        Terms rest;
        string what;
        if (IsToken(start.Head, TokenKind.Colon))
            (row, rest, what) = (null, start.Tail, ":");
        else if (IsPolyArrow(start.Head))
            (row, rest, what) = (PolymorphicRow(inferred: true), start.Tail, "~>");
        else if (IsToken(start.Head, TokenKind.ThinArrow))
        {
            var (written, after) = ParseArrowRow(start.Head!, start.Tail);
            if (written is null) throw new ExpandException("a pure result is written : T; ->{E} T is for an effectful one");
            (row, rest, what) = (written, after, "->{…}");
        }
        else return (null, null, terms);

        var end = 0;
        while (end < rest.Count && rest[end] is not TokenTree.Group { Delimiter: Delimiter.Brace }) end++;
        var typeTerms = TakeTerms(rest, end);
        if (DropSeparators(typeTerms).IsEmpty) throw new ExpandException($"expected a result type after {what}");
        return (ParseAll(typeTerms), row, rest.Drop(end));
    }
}
