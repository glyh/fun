using System.Collections;
using System.Collections.Immutable;
using System.Runtime.CompilerServices;

namespace Fun.Kernel;

/// <summary>
/// An immutable array with structural equality. Records compare their fields
/// with <c>Equals</c>, and <see cref="ImmutableArray{T}"/>'s compares the
/// underlying array by reference -- so a record holding one would call two
/// identical tuples unequal. Every sequence inside a kernel record is one of these.
/// </summary>
[CollectionBuilder(typeof(EquatableArray), nameof(EquatableArray.Create))]
public readonly struct EquatableArray<T> : IEquatable<EquatableArray<T>>, IReadOnlyList<T>
{
    private readonly ImmutableArray<T> _items;

    public EquatableArray(ImmutableArray<T> items) => _items = items;

    // `default` is the empty array, as it is for a record field left unset.
    private ImmutableArray<T> Items => _items.IsDefault ? [] : _items;

    public static readonly EquatableArray<T> Empty = new([]);

    public int Length => Items.Length;
    int IReadOnlyCollection<T>.Count => Length;
    public bool IsEmpty => Items.IsEmpty;
    public T this[int index] => Items[index];

    public EquatableArray<T> Add(T item) => new(Items.Add(item));
    public EquatableArray<T> Insert(int index, T item) => new(Items.Insert(index, item));
    public EquatableArray<T> RemoveAt(int index) => new(Items.RemoveAt(index));

    public bool Equals(EquatableArray<T> other) => Items.SequenceEqual(other.Items);
    public override bool Equals(object? obj) => obj is EquatableArray<T> other && Equals(other);
    public override int GetHashCode() => Items.Aggregate(0, (h, item) => HashCode.Combine(h, item));
    public static bool operator ==(EquatableArray<T> a, EquatableArray<T> b) => a.Equals(b);
    public static bool operator !=(EquatableArray<T> a, EquatableArray<T> b) => !a.Equals(b);

    public ImmutableArray<T>.Enumerator GetEnumerator() => Items.GetEnumerator();
    IEnumerator<T> IEnumerable<T>.GetEnumerator() => ((IEnumerable<T>)Items).GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable)Items).GetEnumerator();

    public override string ToString() => "[" + string.Join(", ", Items) + "]";
}

public static class EquatableArray
{
    public static EquatableArray<T> Create<T>(ReadOnlySpan<T> items) => new([.. items]);

    public static EquatableArray<T> ToEquatableArray<T>(this IEnumerable<T> items) => new([.. items]);
}
