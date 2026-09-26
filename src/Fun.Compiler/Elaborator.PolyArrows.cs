using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// <c>~&gt;</c>: an arrow whose effects are decided by where it sits in its
/// signature (effect-arrow-syntax). Parameters mint, results collect: a <c>~&gt;</c>
/// in a parameter position gets its own row variable, bound implicitly at the
/// signature's root; a <c>~&gt;</c> in a result position carries the variables its
/// parameters minted - or, on a definition (<c>fn(…) ~&gt; T { … }</c>, row
/// inferred), what the body performs. The rewrite produces ordinary syntax: implicit
/// <c>EffectRow</c> binders and rows naming them.
/// </summary>
public static partial class Elaborator
{
    private static int _nextPolyRow;

    /// <summary>A row variable's name: <c>#</c> keeps it apart from anything written.</summary>
    private static Id FreshPolyRow(SourceSpan span) => new($"~e#{Interlocked.Increment(ref _nextPolyRow)}", span);

    private static Syntax EffectRowType(SourceSpan span) => new Syntax.Var(new Id("EffectRow", span));

    /// <summary>
    /// Whether a parameter's type names a value whose own row <c>~&gt;</c> minted - an
    /// alias like <c>Callback = Unit ~&gt; I64</c>, a type awaiting a row. Its binder is
    /// rank 1, bound at the definition that takes the parameter. A binder written out
    /// in the annotation is not one of these: writing it asks for a rank-2 callback.
    /// </summary>
    private static bool IsPolyRowAlias(Context ctx, Syntax type)
    {
        var entryType = type switch
        {
            Syntax.Var v => ctx.TypeOfName(v.Id.Name),
            Syntax.OpenChoice c => ctx.TypeOfChoice(c.Name.Name, c.Opens, c.Fallback),
            _ => null,
        };
        return entryType is not null
            && ctx.Force(entryType) is Value.VPi { Explicitness: Explicitness.Implicit } pi
            && ctx.Force(pi.Domain) is Value.VEffectRowTy
            && ctx.Force(Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ctx.RawMeta())) is Value.VU;
    }

    private static bool HasPoly(Context ctx, Syntax type) =>
        type is Syntax.Arrow a
        && (a.Row is { Polymorphic: true } || IsPolyRowAlias(ctx, a.Domain) || HasPoly(ctx, a.Domain) || HasPoly(ctx, a.Codomain));

    private static bool LambdaHasPoly(Context ctx, Syntax term) =>
        term is Syntax.Lam l
        && (l.Param.Type is { } t && (IsPolyRowAlias(ctx, t) || HasPoly(ctx, t)) || LambdaHasPoly(ctx, l.Body));

    /// <summary>
    /// Each function type is read on its own: every parameter's type is a signature in
    /// its own right, and the chain's FINAL arrow - the one that calls the parameters -
    /// carries the variables they minted. A result arrow that only returns another
    /// function carries nothing. A final <c>~&gt;</c> with no parameter to collect from
    /// mints its own variable: into <paramref name="minted"/> when it sits in a
    /// parameter (a binder of the definition taking that parameter), else into
    /// <paramref name="lifted"/> (the type becomes a function of its row).
    /// </summary>
    // ponytail: variables minted under a higher-order parameter are all bound at the
    // root (rank 1); a callback that must itself be polymorphic is written with named variables.
    private static Syntax RewritePoly(Context ctx, Syntax type, bool inParam, List<Id> minted, List<Id> lifted, List<Id> chain)
    {
        if (type is not Syntax.Arrow a) return type;

        var before = minted.Count;
        var domain = IsPolyRowAlias(ctx, a.Domain)
            ? AtFreshRow(a.Domain, minted)
            : RewritePoly(ctx, a.Domain, inParam: true, minted, lifted, []);
        var collected = chain.Concat(minted.Skip(before)).ToList();
        var final = a.Codomain is not Syntax.Arrow;

        var row = a.Row;
        if (a.Row is { Polymorphic: true } poly)
        {
            if (poly.Inferred) row = poly with { Polymorphic = false };
            else if (!final) row = null;
            else if (collected.Count == 0) row = RowOfVars([Mint(a.Span, inParam ? minted : lifted)]);
            else row = RowOfVars(collected);
        }

        return a with { Domain = domain, Row = row, Codomain = RewritePoly(ctx, a.Codomain, inParam, minted, lifted, collected) };
    }

    private static Id Mint(SourceSpan span, List<Id> into)
    {
        var v = FreshPolyRow(span);
        into.Add(v);
        return v;
    }

    /// <summary>A parameter naming a <c>~&gt;</c> alias: that alias at a variable minted here.</summary>
    private static Syntax AtFreshRow(Syntax alias, List<Id> minted) =>
        new Syntax.Ap(alias, Explicitness.Implicit, new Syntax.Var(Mint(alias.Span, minted)), alias.Span);

    private static EffectRow RowOfVars(IEnumerable<Id> vars) =>
        new([], [.. vars.Select(v => (Syntax)new Syntax.Var(v))], Inferred: false, Polymorphic: false);

    /// <summary>
    /// A type: its root is a result position. Variables it minted for its parameters
    /// become leading implicit binders; one it minted with no parameter to collect
    /// from makes the whole type a function of that row.
    /// </summary>
    private static Syntax PolySignature(Context ctx, Syntax type)
    {
        List<Id> minted = [], lifted = [];
        var rewritten = RewritePoly(ctx, type, inParam: false, minted, lifted, []);
        for (var i = lifted.Count - 1; i >= 0; i--)
            rewritten = new Syntax.Lam(new Param(lifted[i], EffectRowType(type.Span), Explicitness.Implicit), rewritten, type.Span);
        for (var i = minted.Count - 1; i >= 0; i--)
            rewritten = new Syntax.Arrow(Explicitness.Implicit, minted[i], EffectRowType(type.Span), null, rewritten, type.Span);
        return rewritten;
    }

    /// <summary>A lambda: its parameters' types are parameter positions; the variables they mint become leading implicit parameters.</summary>
    private static Syntax PolyLambda(Context ctx, Syntax term)
    {
        List<Id> minted = [], lifted = [];

        Syntax Params(Syntax t) => t is Syntax.Lam l
            ? l with
            {
                Param = l.Param with
                {
                    Type = l.Param.Type is not { } pt ? null
                        : IsPolyRowAlias(ctx, pt) ? AtFreshRow(pt, minted)
                        : RewritePoly(ctx, pt, inParam: true, minted, lifted, []),
                },
                Body = Params(l.Body),
            }
            : t;

        var rewritten = Params(term);
        for (var i = minted.Count - 1; i >= 0; i--)
            rewritten = new Syntax.Lam(new Param(minted[i], EffectRowType(term.Span), Explicitness.Implicit), rewritten, term.Span);
        return rewritten;
    }

    /// <summary>
    /// Checking against an implicit row parameter the term does not bind itself: the
    /// row is bound here, as <c>~&gt;</c>'s variables are, and the term checked under it.
    /// </summary>
    private static Term CheckUnderImplicitRow(Context ctx, Syntax stx, Value.VPi pi)
    {
        var inner = ctx.Bind(FreshPolyRow(stx.Span).Name, pi.Domain);
        var body = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, new Value.VVar(ctx.Width, []));
        return new Term.Lam(Check(inner, stx, body));
    }
}

public sealed partial record Context
{
    /// <summary>The type of the entry a resolved name denotes, or null where <see cref="Locate"/> would not find one.</summary>
    public Value? TypeOfName(string name) => Names.TryGetValue(name, out var entry) ? entry.Type : null;

    /// <summary>The type of the entry an open choice denotes, or null where <see cref="LocateChoice"/> would not find one.</summary>
    public Value? TypeOfChoice(string name, EquatableArray<string> opens, string? fallback)
    {
        foreach (var label in opens)
            if (Opened.TryGetValue(label, out var members) && members.TryGetValue(name, out var member))
                return member.Type;
        if (fallback is not null) return TypeOfName(fallback);
        return BaseNames.TryGetValue(name, out var based) ? based.Type : null;
    }
}
