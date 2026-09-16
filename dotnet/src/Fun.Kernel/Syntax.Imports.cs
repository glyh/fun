namespace Fun.Kernel;

public abstract partial record Syntax
{
    /// <summary><c>import "path"</c>: the compilation unit at that path, as a value.</summary>
    public sealed record Import(string Path, SourceSpan Span) : Syntax(Span);
}
