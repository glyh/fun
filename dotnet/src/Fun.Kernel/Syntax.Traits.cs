namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary>
    /// <c>trait Eq(A) = sig { eq : A -&gt; A -&gt; Bool }; body</c>: a nominal trait with
    /// one parameter, whose fields are operation types over it.
    /// </summary>
    public sealed record TraitDef(Id Name, Id Param, EquatableArray<(string Name, Syntax Type)> Fields, Syntax Body, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// <c>impl [name :] Trait(Arg) = module { … }; body</c>: evidence that
    /// <paramref name="Arg"/> implements <paramref name="Trait"/>, in scope for
    /// <paramref name="Body"/>.
    /// </summary>
    public sealed record ImplDef(
        Id? Name, Syntax TraitPath, Syntax Arg, EquatableArray<(string Name, Syntax Value)> Fields, Syntax Body, SourceSpan Span)
        : Syntax(Span);

    /// <summary><c>{Eq, Show}</c> as an implicit binder's bound: the traits it must implement.</summary>
    public sealed record TraitBoundSet(EquatableArray<Syntax> Traits, SourceSpan Span) : Syntax(Span);

    private Syntax AddScopeTraits(ScopeSet scope) => this switch
    {
        TraitDef t => t with
        {
            Name = t.Name with { Scope = t.Name.Scope.Union(scope) },
            Param = t.Param with { Scope = t.Param.Scope.Union(scope) },
            Fields = [.. t.Fields.Select(f => (f.Name, f.Type.AddScope(scope)))],
            Body = t.Body.AddScope(scope),
        },
        ImplDef i => i with
        {
            Name = i.Name is null ? null : i.Name with { Scope = i.Name.Scope.Union(scope) },
            TraitPath = i.TraitPath.AddScope(scope),
            Arg = i.Arg.AddScope(scope),
            Fields = [.. i.Fields.Select(f => (f.Name, f.Value.AddScope(scope)))],
            Body = i.Body.AddScope(scope),
        },
        TraitBoundSet b => b with { Traits = [.. b.Traits.Select(t => t.AddScope(scope))] },
        _ => throw new InvalidOperationException($"unhandled syntax {GetType().Name}"),
    };
}

public abstract partial record Binding
{
    /// <summary><c>[pub] trait Eq(A) = sig { … }</c> as a module item.</summary>
    public sealed record Trait(Id Name, Id Param, EquatableArray<(string Name, Syntax Type)> Fields, bool Public) : Binding;

    /// <summary>
    /// <c>[pub] impl [name :] Trait(Arg) = module { … }</c> as a module or struct item.
    /// In a signature (<c>name : impl Trait(Arg)</c>) it has no fields: it is the
    /// impl the described module must provide.
    /// </summary>
    public sealed record Impl(Id? Name, Syntax TraitPath, Syntax Arg, EquatableArray<(string Name, Syntax Value)>? Fields, bool Public) : Binding;

    private Binding AddScopeTraits(ScopeSet scope) => this switch
    {
        Trait t => t with
        {
            Name = t.Name with { Scope = t.Name.Scope.Union(scope) },
            Param = t.Param with { Scope = t.Param.Scope.Union(scope) },
            Fields = [.. t.Fields.Select(f => (f.Name, f.Type.AddScope(scope)))],
        },
        Impl i => i with
        {
            Name = i.Name is null ? null : i.Name with { Scope = i.Name.Scope.Union(scope) },
            TraitPath = i.TraitPath.AddScope(scope),
            Arg = i.Arg.AddScope(scope),
            Fields = i.Fields is { } fields ? [.. fields.Select(f => (f.Name, f.Value.AddScope(scope)))] : null,
        },
        _ => throw new InvalidOperationException($"unhandled binding {GetType().Name}"),
    };
}
