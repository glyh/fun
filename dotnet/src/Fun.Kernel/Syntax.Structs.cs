namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary>
    /// <c>struct { x : I64; pub f = … }</c>: a record type and a namespace in one --
    /// constructor fields alongside bindings, in source order.
    /// </summary>
    public sealed record Struct(EquatableArray<Binding> Bindings, SourceSpan Span) : Syntax(Span);

    /// <summary><c>P{x = 1; y = 2}</c>: a record of the struct <paramref name="Type"/>.</summary>
    public sealed record RecordConstruct(Syntax Type, EquatableArray<(string Name, Syntax Value)> Fields, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// <c>sig { T : Type; empty : T }</c>: a signature value. Each binding is a
    /// public member whose value is its type.
    /// </summary>
    public sealed record Sig(EquatableArray<Binding> Bindings, SourceSpan Span) : Syntax(Span);

    /// <summary><c>self</c>: inside a method, the value the method was called on.</summary>
    public sealed record Self(SourceSpan Span) : Syntax(Span);

    /// <summary><c>Self</c>: inside a struct, the struct being defined, as the fields written so far.</summary>
    public sealed record SelfType(SourceSpan Span) : Syntax(Span);
}

public abstract partial record Binding
{
    /// <summary>
    /// A struct's constructor field <c>name : type</c>. A label, not a binder: the
    /// items after it do not see it.
    /// </summary>
    public sealed record Field(string Name, Syntax Type) : Binding;

    /// <summary>
    /// <c>[pub] method m(params) [: T] { body }</c>: a function of <c>self</c>, then
    /// of its parameters. A result type annotates the body. Pure: a method that
    /// declares no row performs nothing.
    /// </summary>
    /// <remarks>
    /// <paramref name="Row"/> is the row its result declares (<c>-&gt;{E} T</c>); none
    /// means the method is pure (E3). It is read in the parameters' scope and sits
    /// on the innermost arrow.
    /// </remarks>
    public sealed record Method(Id Name, EquatableArray<Param> Params, Syntax Body, bool Public, EffectRow? Row = null) : Binding;
}
