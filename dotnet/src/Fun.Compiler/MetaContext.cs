using Fun.Kernel;

namespace Fun.Compiler;

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
    /// Each maps to the member label it is bound to in that module, or null when it is
    /// not bound directly as a member.
    /// </summary>
    public Dictionary<NominalDecl, string?> GenerativeNominals { get; } = [];

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

    public void Solve(int id, Value value)
    {
        if (_solutions[id] is not null)
            throw new InvalidOperationException($"meta ?{id} is already solved");
        _solutions[id] = value;
    }
}
