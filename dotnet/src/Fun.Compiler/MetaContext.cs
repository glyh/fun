using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// A generative nominal's sealing information (E11): the member label its declaring
/// module bound it to, and how many type parameters the former takes. Null label for
/// a nominal not bound directly as a member.
/// </summary>
public sealed record GenerativeNominal(string? Label, int NumParams);

/// <summary>The metas of one elaboration, and what they have been solved to.</summary>
public sealed class MetaContext
{
    private readonly List<Value?> _solutions = [];

    /// <summary>The evaluation budget every evaluation under these metas spends from.</summary>
    public Budget Budget { get; } = new();

    public int Fresh()
    {
        _solutions.Add(null);
        return _solutions.Count - 1;
    }

    public Value? Solution(int id) => _solutions[id];

    /// <summary>How many metas exist: the id the next one gets.</summary>
    public int Count => _solutions.Count;

    /// <summary>Every nominal declaration made during this elaboration, in order.</summary>
    public List<NominalDecl> DeclaredNominals { get; } = [];

    /// <summary>
    /// The generative nominals (E11): declared by a module whose evaluation performs.
    /// Each maps to the member label it is bound to in that module (null when it is
    /// not bound directly as a member) and, for a type former, how many parameters it
    /// takes - sealing re-applies those, since an applied nominal's captures are its
    /// parameters.
    /// </summary>
    public Dictionary<NominalDecl, GenerativeNominal> GenerativeNominals { get; } = [];

    /// <summary>Impl choices waiting on argument types (traits.md, "Resolution", rule 4).</summary>
    public List<PendingEvidence> PendingEvidence { get; } = [];

    /// <summary>The metas standing for rows written <c>_</c>: one nothing solves is an error, never a default.</summary>
    public List<int> WrittenRows { get; } = [];

    /// <summary>The solutions as they stand, to undo a trial unification with <see cref="Restore"/>.</summary>
    public Value?[] Snapshot() => [.. _solutions];

    /// <summary>
    /// Back to <paramref name="snapshot"/>: solutions made since are undone and metas
    /// created since keep existing, unsolved - an id handed out is never reused.
    /// </summary>
    public void Restore(Value?[] snapshot)
    {
        for (var i = 0; i < _solutions.Count; i++) _solutions[i] = i < snapshot.Length ? snapshot[i] : null;
    }

    /// <summary>
    /// Makes a fresh context carry the prelude's metas, solutions and nominal
    /// bookkeeping first, so a prelude value whose terms name one of its metas means
    /// the same meta here, and this context's own metas never reuse an id.
    /// </summary>
    internal void SeedFrom(MetaContext prelude)
    {
        if (ReferenceEquals(this, prelude) || _seeded) return;
        if (_solutions.Count > 0)
            throw new InvalidOperationException("a meta context must be seeded from the prelude before it creates metas");
        _solutions.AddRange(prelude._solutions);
        DeclaredNominals.AddRange(prelude.DeclaredNominals);
        foreach (var (decl, gen) in prelude.GenerativeNominals) GenerativeNominals[decl] = gen;
        _seeded = true;
    }

    private bool _seeded;

    public void Solve(int id, Value value)
    {
        if (_solutions[id] is not null)
            throw new InvalidOperationException($"meta ?{id} is already solved");
        _solutions[id] = value;
    }
}
