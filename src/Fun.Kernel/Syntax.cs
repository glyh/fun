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

public sealed record Param(Id Name, Syntax? Type, Explicitness Explicitness, EquatableArray<Syntax> Bounds = default);

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
    public sealed record Open(Syntax Of, Syntax Body, string Label, SourceSpan Span) : Syntax(Span)
    {
        /// <summary>
        /// The syntactic roles visible in this open's region, set by expansion: an
        /// open may not supply a member of one of these names (M7).
        /// </summary>
        public EquatableArray<string> RolesInRegion { get; init; } = [];
    }

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
    public Syntax AddScope(ScopeSet scope) => Map(SyntaxMapper.Adding(scope));
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
    public sealed record Open(Syntax Of, string Label) : Binding
    {
        /// <summary>The syntactic roles visible in this open's region (see <see cref="Syntax.Open.RolesInRegion"/>).</summary>
        public EquatableArray<string> RolesInRegion { get; init; } = [];
    }

    /// <summary>
    /// Items not read yet: a module's remaining statements, enforested one form
    /// at a time as expansion reaches them.
    /// </summary>
    public sealed record Items(EquatableArray<TokenTree> Terms) : Binding;

    public Binding AddScope(ScopeSet scope) => Map(SyntaxMapper.Adding(scope));
}
