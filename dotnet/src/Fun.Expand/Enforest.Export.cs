using Fun.Kernel;

namespace Fun.Expand;

public static partial class Enforest
{
    /// <summary>
    /// <c>export M</c> or <c>export M.{a, b}</c>, the selection being a trailing
    /// <c>.{ … }</c>. Null when the statement is not an export.
    /// </summary>
    private static Binding.Export? ParseExportStatement(bool isPublic, Terms stmt)
    {
        stmt = DropSeparators(stmt);
        if (!IsToken(stmt.Head, TokenKind.Export)) return null;
        if (isPublic) throw new ExpandException("export is not a public item: an export already publishes");

        var rest = DropSeparators(stmt.Tail);
        EquatableArray<string>? names = null;
        if (rest.Count >= 2
            && rest[rest.Count - 1] is TokenTree.Group { Delimiter: Delimiter.Brace } selection
            && IsToken(rest[rest.Count - 2], TokenKind.Dot))
        {
            names = SplitCommas(DropSeparators(new Terms(selection.Items)))
                .Select(DropSeparators)
                .Where(item => !item.IsEmpty)
                .Select(item => item.Count == 1 && NameOf(item.Head) is { } name
                    ? name.Name
                    : throw new ExpandException("export M.{a, b} names members"))
                .ToEquatableArray();
            rest = TakeTerms(rest, rest.Count - 2);
        }
        return new Binding.Export(ParseAll(rest), names, Public: true);
    }
}
