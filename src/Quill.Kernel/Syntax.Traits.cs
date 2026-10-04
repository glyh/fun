namespace Quill.Kernel;

public abstract partial record Syntax
{
    /// <summary>
    /// <c>trait Eq(A) = sig { eq : A -&gt; A -&gt; Bool }; body</c>: a nominal trait with
    /// one parameter, whose fields are operation types over it.
    /// </summary>
    public sealed record TraitDef(Id Name, Id Param, EquatableArray<(string Name, Syntax Type)> Fields, Syntax Body, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// <c>impl [name] [binders] [:] Trait(Arg) = module { … }; body</c>: evidence
    /// that <paramref name="Arg"/> implements <paramref name="Trait"/>, in scope for
    /// <paramref name="Body"/>. <paramref name="Binders"/> are the head's written
    /// bounds (<c>[a : Eq]</c>): when any is written they are the whole truth about
    /// the impl's bounds; an impl writing none infers them from its body.
    /// </summary>
    public sealed record ImplDef(
        Id? Name, EquatableArray<Param> Binders, Syntax TraitPath, Syntax Arg, EquatableArray<(string Name, Syntax Value)> Fields, Syntax Body, SourceSpan Span)
        : Syntax(Span);

    /// <summary><c>{Eq, Show}</c> as an implicit binder's bound: the traits it must implement.</summary>
    public sealed record TraitBoundSet(EquatableArray<Syntax> Traits, SourceSpan Span) : Syntax(Span);

}

public abstract partial record Binding
{
    /// <summary><c>[pub] trait Eq(A) = sig { … }</c> as a module item.</summary>
    public sealed record Trait(Id Name, Id Param, EquatableArray<(string Name, Syntax Type)> Fields, bool Public) : Binding;

    /// <summary>
    /// <c>[pub] impl [name] [binders] [:] Trait(Arg) = module { … }</c> as a module or
    /// struct item. In a signature (<c>name : impl Trait(Arg)</c>) it has no fields and
    /// no binders: it is the impl the described module must provide.
    /// </summary>
    public sealed record Impl(Id? Name, EquatableArray<Param> Binders, Syntax TraitPath, Syntax Arg, EquatableArray<(string Name, Syntax Value)>? Fields, bool Public) : Binding;

}
