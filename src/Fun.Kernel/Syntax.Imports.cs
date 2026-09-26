namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary>
    /// <c>import "path"</c>: the compilation unit at that path, as a value.
    /// <see cref="Scope"/> is where it is written, the scope set its keyword
    /// carries: the roles an open of it brings bind from there.
    /// </summary>
    public sealed record Import(string Path, SourceSpan Span) : Syntax(Span)
    {
        public ScopeSet Scope { get; init; } = ScopeSet.Empty;
    }
}
