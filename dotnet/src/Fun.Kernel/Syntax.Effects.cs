namespace Fun.Kernel;

/// <summary>An effect operation as declared: <c>op : Input -&gt; Output</c>.</summary>
public sealed record EffectOp(string Name, Syntax Input, Syntax Output);

public abstract partial record Syntax
{
    /// <summary>
    /// <c>effect Name(Params) = sig { op : A -&gt; B; … }</c> scoped over the rest
    /// of a block. The parameters scope over the operations; the name scopes over
    /// the body only.
    /// </summary>
    public sealed record EffectDef(Id Name, EquatableArray<Id> Params, EquatableArray<EffectOp> Ops, Syntax Body, SourceSpan Span)
        : Syntax(Span);

    /// <summary>
    /// <c>perform E.op(arg)</c>. <paramref name="Operation"/> is the path written:
    /// its last member is the operation, the rest names the effect.
    /// </summary>
    public sealed record Perform(FieldAccess Operation, Syntax Arg, SourceSpan Span) : Syntax(Span);

    /// <summary><c>resume(arg)</c>: lexically scoped to an effect branch (E9).</summary>
    public sealed record Resume(Syntax Arg, SourceSpan Span) : Syntax(Span);

}

public abstract partial record Binding
{
    /// <summary>An effect family as a module item.</summary>
    public sealed record Effect(Id Name, EquatableArray<Id> Params, EquatableArray<EffectOp> Ops, bool Public) : Binding;
}
