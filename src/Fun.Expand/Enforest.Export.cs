using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Enforest
{
    /// <summary>
    /// <c>export M</c> or <c>export M.{a, b}</c>, the selection being a trailing
    /// <c>.{ … }</c>. Null when the statement is not an export.
    /// </summary>
    private Binding.Export? ParseExportStatement(bool isPublic, Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Export)) return null;
        if (isPublic) throw new ExpandException("export is not a public item: an export already publishes", stmt.Span);

        var (rest, names) = TakeSelection(DropSeparators(stmt.Tail));
        return new Binding.Export(ParseAll(rest), names, Public: true);
    }

    /// <summary>
    /// A trailing <c>.{ a, b }</c> selection after a module path -- the one list
    /// <c>export</c> and <c>open</c> both take (decided 2026-09-28), so one parse
    /// serves both and no member kind is asymmetric between them. Free only where
    /// a module path is already expected: elsewhere <c>.{</c> spells record
    /// construction.
    /// </summary>
    private (Terms Remaining, EquatableArray<string>? Names) TakeSelection(Terms rest)
    {
        if (rest.Count < 2
            || rest[rest.Count - 1] is not TokenTree.Group { Delimiter: Delimiter.Brace } selection
            || !IsToken(rest[rest.Count - 2], TokenKind.Dot))
            return (rest, null);
        var names = SplitCommas(DropSeparators(new Terms(selection.Items)))
            .Select(DropSeparators)
            .Where(item => !item.IsEmpty)
            .Select(item => item.Count == 1 && NameOf(item.Head) is { } name
                ? name.Name
                : throw new ExpandException("a selection .{a, b} names members", item.Span))
            .ToEquatableArray();
        return (TakeTerms(rest, rest.Count - 2), names);
    }
}
