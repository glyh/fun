using System.Collections.Immutable;

namespace Fun.Kernel;

/// <summary>
/// A set of scopes (Flatt 2016, sets of scopes). A token carries the scopes of
/// every definition context, template instance and binder around it; a name
/// resolves against them.
/// </summary>
// ponytail: ImmutableSortedSet is a tree where the OCaml is a sorted list;
// hand-roll a sorted array if scope-set union shows up in a profile.
public readonly struct ScopeSet : IEquatable<ScopeSet>
{
    private readonly ImmutableSortedSet<int>? _scopes;

    private ScopeSet(ImmutableSortedSet<int> scopes) => _scopes = scopes;

    private ImmutableSortedSet<int> Scopes => _scopes ?? ImmutableSortedSet<int>.Empty;

    public static readonly ScopeSet Empty = new(ImmutableSortedSet<int>.Empty);

    public static ScopeSet Singleton(int scope) => new(ImmutableSortedSet.Create(scope));
    public static ScopeSet Of(IEnumerable<int> scopes) => new(scopes.ToImmutableSortedSet());

    public bool IsEmpty => Scopes.IsEmpty;
    public int Count => Scopes.Count;
    public IEnumerable<int> Values => Scopes;

    public bool Contains(int scope) => Scopes.Contains(scope);
    public ScopeSet Add(int scope) => new(Scopes.Add(scope));
    public ScopeSet Remove(int scope) => new(Scopes.Remove(scope));
    public ScopeSet Union(ScopeSet other) => new(Scopes.Union(other.Scopes));
    public ScopeSet Intersect(ScopeSet other) => new(Scopes.Intersect(other.Scopes));
    public ScopeSet Except(ScopeSet other) => new(Scopes.Except(other.Scopes));
    public ScopeSet Where(Func<int, bool> keep) => new(Scopes.Where(keep).ToImmutableSortedSet());
    public bool IsSubsetOf(ScopeSet other) => Scopes.IsSubsetOf(other.Scopes);

    public bool Equals(ScopeSet other) => Scopes.SetEquals(other.Scopes);
    public override bool Equals(object? obj) => obj is ScopeSet s && Equals(s);
    public override int GetHashCode() => Scopes.Aggregate(0, HashCode.Combine);
    public override string ToString() => "{" + string.Join(",", Scopes) + "}";
}
