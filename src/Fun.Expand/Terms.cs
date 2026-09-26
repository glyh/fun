using System.Collections;
using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// A suffix of a term sequence. The enforester consumes terms left to right and
/// hands back what it did not read, so this stands in for the OCaml list the
/// prototype pattern-matches on -- <c>rest</c> is always the real remainder,
/// never the empty list.
/// </summary>
public readonly struct Terms(EquatableArray<TokenTree> items, int start) : IEnumerable<TokenTree>
{
    public Terms(EquatableArray<TokenTree> items) : this(items, 0) { }

    public static readonly Terms Empty = new([], 0);

    public int Count => items.Length - start;
    public bool IsEmpty => Count == 0;
    public TokenTree this[int i] => items[start + i];
    public TokenTree? Head => Count > 0 ? items[start] : null;

    /// <summary>The terms after the first <paramref name="n"/>.</summary>
    public Terms Drop(int n) => new(items, Math.Min(start + n, items.Length));

    public Terms Tail => Drop(1);

    public EquatableArray<TokenTree> ToArray() => [.. this];

    public SourceSpan Span => Count == 0
        ? SourceSpan.Synthetic
        : SourceSpan.Between(this[0].Span, this[Count - 1].Span);

    public IEnumerator<TokenTree> GetEnumerator()
    {
        for (var i = start; i < items.Length; i++) yield return items[i];
    }

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}
