namespace Fun.Kernel;

/// <summary>
/// Where a piece of syntax came from. Offsets are UTF-16 code units into the
/// source string (the OCaml prototype counts UTF-8 bytes; spans are only ever
/// displayed, never compared across implementations). <see cref="Synthetic"/>
/// is the span of syntax the compiler made up, which no source position
/// describes.
/// </summary>
public sealed record SourceSpan(
    string? File,
    int Start,
    int End,
    int? StartLine,
    int? StartCol,
    int? EndLine,
    int? EndCol,
    bool IsSynthetic)
{
    public static readonly SourceSpan Synthetic =
        new(null, 0, 0, null, null, null, null, IsSynthetic: true);

    public static SourceSpan Make(int start, int end, string? file = null,
        int? startLine = null, int? startCol = null, int? endLine = null, int? endCol = null) =>
        new(file, start, end, startLine, startCol, endLine, endCol, IsSynthetic: false);

    /// <summary>The span running from the start of <c>a</c> to the end of <c>b</c>.</summary>
    public static SourceSpan Between(SourceSpan a, SourceSpan b) =>
        a.IsSynthetic || b.IsSynthetic
            ? Synthetic
            : new(a.File, a.Start, b.End, a.StartLine, a.StartCol, b.EndLine, b.EndCol, false);

    public override string ToString()
    {
        if (IsSynthetic) return "<synthetic>";
        var file = File ?? "<unknown>";
        return (StartLine, StartCol, EndLine, EndCol) is (int sl, int sc, int el, int ec)
            ? $"{file}:{sl}:{sc}-{el}:{ec}"
            : $"{file}:{Start}-{End}";
    }
}
