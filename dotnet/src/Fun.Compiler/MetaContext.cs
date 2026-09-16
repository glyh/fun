using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>The metavariables of one elaboration, and what they have been solved to.</summary>
public sealed class MetaContext
{
    private readonly List<Value?> _solutions = [];

    public int Fresh()
    {
        _solutions.Add(null);
        return _solutions.Count - 1;
    }

    public Value? Solution(int id) => _solutions[id];

    public void Solve(int id, Value value)
    {
        if (_solutions[id] is not null)
            throw new InvalidOperationException($"metavariable ?{id} is already solved");
        _solutions[id] = value;
    }
}
