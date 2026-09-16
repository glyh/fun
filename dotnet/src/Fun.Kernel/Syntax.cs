using System.Collections.Immutable;

namespace Fun.Kernel;

public enum Explicitness { Implicit, Explicit }

/// <summary>
/// An identifier occurrence: what was written, where, and the scopes around it.
/// Resolution picks the binder of this name whose scope set is the largest
/// subset of <see cref="Scope"/>.
/// </summary>
public sealed record Id(string Name, SourceSpan Span, ScopeSet Scope)
{
    public Id(string name, SourceSpan span) : this(name, span, ScopeSet.Empty) { }
}

public sealed record Param(Id Name, Syntax? Type, Explicitness Explicitness);

/// <summary>
/// An arrow's latent effects. <c>Inferred</c> is the row written <c>_</c>;
/// <c>Polymorphic</c> is the arrow written <c>~&gt;</c>, whose row is decided by
/// where it sits in its signature. An arrow with no row at all is pure.
/// </summary>
public sealed record EffectRow(
    EquatableArray<Syntax> Effects,
    EquatableArray<Syntax> Tails,
    bool Inferred,
    bool Polymorphic);

/// <summary>
/// The surface syntax tree, between the reader and the elaborator. Macro
/// expansion rewrites it in place of itself.
/// </summary>
// Only the node kinds the current slice reaches are here; the rest of
// `Syntax.kind` arrives as conformance cases demand them.
public abstract partial record Syntax(SourceSpan Span)
{
    public sealed record Atom(Fun.Kernel.Atom Value, SourceSpan Span) : Syntax(Span);

    public sealed record Var(Id Id) : Syntax(Id.Span);

    public sealed record Ap(Syntax Fn, Explicitness Explicitness, Syntax Arg, SourceSpan Span) : Syntax(Span);

    public sealed record Lam(Param Param, Syntax Body, SourceSpan Span) : Syntax(Span);

    public sealed record Let(Id Name, Syntax? Type, Syntax Value, Syntax Body, bool Recursive, SourceSpan Span)
        : Syntax(Span);

    public sealed record Annotated(Syntax Inner, Syntax Type, SourceSpan Span) : Syntax(Span);

    /// <summary>A function type. A null <paramref name="Row"/> is a pure arrow.</summary>
    public sealed record Arrow(
        Explicitness Explicitness, Id? Name, Syntax Domain, EffectRow? Row, Syntax Codomain, SourceSpan Span)
        : Syntax(Span);

    public sealed record Prod(EquatableArray<Syntax> Items, SourceSpan Span) : Syntax(Span);
    public sealed record ProdTy(EquatableArray<Syntax> Items, SourceSpan Span) : Syntax(Span);
    public sealed record Proj(Syntax Of, int Index, SourceSpan Span) : Syntax(Span);
    public sealed record FieldAccess(Syntax Of, string Field, SourceSpan Span) : Syntax(Span);

    /// <summary>A first-class module: its bindings, in source order.</summary>
    public sealed record Module(EquatableArray<Binding> Bindings, SourceSpan Span) : Syntax(Span);

    /// <summary>
    /// <c>open m; body</c>. <paramref name="Label"/> names this open so an open
    /// choice can refer to it: empty until expansion assigns one.
    /// </summary>
    public sealed record Open(Syntax Of, Syntax Body, string Label, SourceSpan Span) : Syntax(Span);

    /// <summary>
    /// Produced only by expansion: a bare name some open may supply. Elaboration
    /// takes the first of <paramref name="Opens"/> (innermost first) that has
    /// the member, else the binder <paramref name="Fallback"/> names, else the
    /// base context.
    /// </summary>
    public sealed record OpenChoice(Id Name, EquatableArray<string> Opens, string? Fallback) : Syntax(Name.Span);

    /// <summary>
    /// A <c>{ … }</c> body not read yet: its statements are enforested one form
    /// at a time as expansion reaches them, so a declaration in the block can
    /// bind the syntax the statements after it are read with.
    /// </summary>
    public sealed record Block(EquatableArray<TokenTree> Terms, SourceSpan Span) : Syntax(Span);

    /// <summary>
    /// Adds <paramref name="scope"/> to every identifier and unread token in the
    /// tree -- how a binder marks its body as being inside it.
    /// </summary>
    public Syntax AddScope(ScopeSet scope)
    {
        Id Mark(Id id) => id with { Scope = id.Scope.Union(scope) };
        Syntax Go(Syntax s) => s.AddScope(scope);
        Syntax? GoOpt(Syntax? s) => s?.AddScope(scope);

        return this switch
        {
            Atom or Import or Self or SelfType => this,
            Var v => v with { Id = Mark(v.Id) },
            Ap a => a with { Fn = Go(a.Fn), Arg = Go(a.Arg) },
            Lam l => l with { Param = MarkParam(l.Param, scope), Body = Go(l.Body) },
            Let l => l with { Name = Mark(l.Name), Type = GoOpt(l.Type), Value = Go(l.Value), Body = Go(l.Body) },
            Annotated a => a with { Inner = Go(a.Inner), Type = Go(a.Type) },
            Arrow a => a with
            {
                Name = a.Name is null ? null : Mark(a.Name),
                Domain = Go(a.Domain),
                Row = MarkRow(a.Row, scope),
                Codomain = Go(a.Codomain),
            },
            Prod p => p with { Items = [.. p.Items.Select(Go)] },
            ProdTy p => p with { Items = [.. p.Items.Select(Go)] },
            Proj p => p with { Of = Go(p.Of) },
            FieldAccess f => f with { Of = Go(f.Of) },
            Block b => b with { Terms = [.. b.Terms.Select(t => t.AddScope(scope))] },
            Module m => m with { Bindings = [.. m.Bindings.Select(b => b.AddScope(scope))] },
            Open o => o with { Of = Go(o.Of), Body = Go(o.Body) },
            OpenChoice c => c with { Name = Mark(c.Name) },
            Match m => m with { Scrutinee = Go(m.Scrutinee), Branches = [.. m.Branches.Select(b => new MatchBranch(b.Pattern.AddScope(scope), Go(b.Body)))] },
            Enum e => e with { Constructors = [.. e.Constructors.Select(c => c with { Payloads = [.. c.Payloads.Select(Go)] })] },
            LetRecGroup g => g with { Members = [.. g.Members.Select(m => m.AddScope(scope))], Body = Go(g.Body) },
            Struct st => st with { Bindings = [.. st.Bindings.Select(b => b.AddScope(scope))] },
            Sig sg => sg with { Bindings = [.. sg.Bindings.Select(b => b.AddScope(scope))] },
            RecordConstruct r => r with { Type = Go(r.Type), Fields = [.. r.Fields.Select(f => (f.Name, Go(f.Value)))] },
            TraitDef or ImplDef or TraitBoundSet => AddScopeTraits(scope),
            _ => throw new InvalidOperationException($"unhandled syntax {GetType().Name}"),
        };
    }

    private static Param MarkParam(Param p, ScopeSet scope) =>
        p with { Name = p.Name with { Scope = p.Name.Scope.Union(scope) }, Type = p.Type?.AddScope(scope) };

    private static EffectRow? MarkRow(EffectRow? row, ScopeSet scope) =>
        row is null ? null : row with
        {
            Effects = [.. row.Effects.Select(e => e.AddScope(scope))],
            Tails = [.. row.Tails.Select(t => t.AddScope(scope))],
        };
}

/// <summary>
/// One item written inside a module or struct. A named public binding is reached
/// from outside as a member.
/// </summary>
// Only the binding kinds the current slice reaches are here.
public abstract partial record Binding
{
    public sealed record Let(Id Name, Syntax Value, bool Public, bool Recursive) : Binding;

    /// <summary><c>open m</c>: scopes over the bindings after it. It adds no member.</summary>
    public sealed record Open(Syntax Of, string Label) : Binding;

    /// <summary>
    /// Items not read yet: a module's remaining statements, enforested one form
    /// at a time as expansion reaches them.
    /// </summary>
    public sealed record Items(EquatableArray<TokenTree> Terms) : Binding;

    public Binding AddScope(ScopeSet scope) => this switch
    {
        Let l => l with { Name = l.Name with { Scope = l.Name.Scope.Union(scope) }, Value = l.Value.AddScope(scope) },
        Open o => o with { Of = o.Of.AddScope(scope) },
        Items i => i with { Terms = [.. i.Terms.Select(t => t.AddScope(scope))] },
        RecGroup g => g with { Members = [.. g.Members.Select(m => m.AddScope(scope))] },
        Field f => f with { Type = f.Type.AddScope(scope) },
        Method m => m with
        {
            Name = m.Name with { Scope = m.Name.Scope.Union(scope) },
            Params = [.. m.Params.Select(p => p with { Name = p.Name with { Scope = p.Name.Scope.Union(scope) }, Type = p.Type?.AddScope(scope) })],
            Body = m.Body.AddScope(scope),
        },
        Trait or Impl => AddScopeTraits(scope),
        _ => throw new InvalidOperationException($"unhandled binding {GetType().Name}"),
    };
}
