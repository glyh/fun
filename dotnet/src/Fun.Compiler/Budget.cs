namespace Fun.Compiler;

/// <summary>
/// The evaluation budget: how many semantic steps the checker may spend on one
/// request -- every function call (a fixpoint unfold included) and every step of a
/// unification. A request is refilled when it starts and spent by everything it
/// does before it returns, however the evaluator re-enters itself. Running a
/// program is a request with no limit. Termination is never checked: a divergent
/// evaluation while checking is a budget error, not a hang.
/// </summary>
// ponytail: the limit is a constant with no surface syntax to raise it, as in
// the prototype (checker-evaluation-budget leaves that open).
public sealed class Budget
{
    public const int DefaultLimit = 1_000_000;

    private int? _limit = DefaultLimit;
    private int _remaining = DefaultLimit;
    private int _depth;
    private string? _demand;
    private string? _calling;

    /// <summary>Whether the current request is type checking (limited) rather than running a program.</summary>
    public bool Checking => _limit is not null;

    /// <summary>One checker request, named by what demanded it; nested requests spend from the outermost.</summary>
    public T Request<T>(string demand, Func<T> work) => Start(DefaultLimit, demand, work);

    /// <summary>Runs a program: no limit.</summary>
    public T Run<T>(Func<T> work) => Start(null, "running a program", work);

    private T Start<T>(int? limit, string demand, Func<T> work)
    {
        if (_depth == 0)
        {
            (_limit, _remaining, _demand, _calling) = (limit, limit ?? 0, demand, null);
        }
        _depth++;
        try
        {
            return work();
        }
        finally
        {
            _depth--;
        }
    }

    /// <summary>The innermost macro application running under this budget, answering <c>expand_block</c> and <c>expand_decls</c>.</summary>
    public MacroApplication? Application { get; private set; }

    /// <summary>
    /// A macro application is a call (M5): it spends one step, and its body and the
    /// expansion of its output spend from the same request, so a nest of applications
    /// is bounded as a whole, breadth included.
    /// </summary>
    public T MacroApplication<T>(MacroApplication application, Func<T> work) =>
        Request($"the application of macro '{application.Macro}'", () =>
        {
            var outer = Application;
            Application = application;
            try
            {
                Spend($"macro '{application.Macro}'");
                return work();
            }
            finally
            {
                Application = outer;
            }
        });

    /// <summary>
    /// Spends one step on <paramref name="call"/>. A fixpoint unfold names itself, so an
    /// overrun in a divergent evaluation names the definition it keeps calling.
    /// </summary>
    public void Spend(string call, bool isFixpoint = false)
    {
        if (_limit is not { } limit) return;
        if (isFixpoint) _calling = call;
        if (_remaining <= 0)
            throw new FunException(
                $"evaluation exceeded the budget of {limit} calls while type checking: calling {_calling ?? call}, in {_demand ?? "an evaluation"} (the budget cannot yet be raised from source)");
        _remaining--;
    }
}

/// <summary>
/// A running macro application: <c>expand_block</c> and <c>expand_decls</c>, which
/// expand a reflected block or declaration list where the application runs (M9).
/// </summary>
public sealed record MacroApplication(string Macro, Func<Kernel.Value, Kernel.Value> ExpandBlock, Func<Kernel.Value, Kernel.Value> ExpandDecls);
