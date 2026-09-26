namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary><c>ref(e)</c>: a new reference holding <c>e</c>, on a heap of its own.</summary>
    public sealed record RefNew(Syntax Arg, SourceSpan Span) : Syntax(Span);

    /// <summary><c>deref(r)</c>: what the reference holds.</summary>
    public sealed record RefGet(Syntax Ref, SourceSpan Span) : Syntax(Span);

    /// <summary><c>r &lt;- e</c>: stores <c>e</c> in the reference.</summary>
    public sealed record RefSet(Syntax Ref, Syntax Value, SourceSpan Span) : Syntax(Span);

}
