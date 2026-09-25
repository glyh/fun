using Fun.Kernel;

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
    private string? _calling;
    private (string Mode, SourceSpan Span)? _site;
    private readonly List<Frame> _frames = [];

    /// <summary>One request in the chain that spent the budget: what it was, and where.</summary>
    public sealed record Frame(string What, string? Site)
    {
        public override string ToString() => Site is { } site ? $"{What} at {site}" : What;
    }

    /// <summary>Whether the current request is type checking (limited) rather than running a program.</summary>
    public bool Checking => _limit is not null;

    /// <summary>
    /// The source form the elaborator is at while <paramref name="work"/> runs (the innermost
    /// enclosing one; synthetic spans keep the outer). An evaluation that fails while checking
    /// names it, as the prototype's <c>Eval_budget.at</c> site does.
    /// </summary>
    public T At<T>(SourceSpan span, string mode, Func<T> work)
    {
        if (span.IsSynthetic) return work();
        var outer = _site;
        _site = (mode, span);
        try { return work(); }
        finally { _site = outer; }
    }

    /// <summary>Where the checker was when it asked for the evaluation, as the prototype's <c>where</c> does.</summary>
    public string Where() => _site is { } site ? $" while {site.Mode} at {site.Span}" : "";

    /// <summary>One checker request, named by what demanded it; nested requests spend from the outermost.</summary>
    public T Request<T>(string demand, Func<T> work) => Start(DefaultLimit, new Frame(demand, null), work);

    /// <summary>Runs a program: no limit.</summary>
    public T Run<T>(Func<T> work) => Start(null, new Frame("running a program", null), work);

    private T Start<T>(int? limit, Frame frame, Func<T> work)
    {
        if (_depth == 0)
        {
            (_limit, _remaining, _calling) = (limit, limit ?? 0, null);
            _frames.Clear();
        }
        _depth++;
        _frames.Add(frame);
        try
        {
            return work();
        }
        finally
        {
            _depth--;
            _frames.RemoveAt(_frames.Count - 1);
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
        Start(DefaultLimit, FrameOf(application), () =>
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

    private static Frame FrameOf(MacroApplication application) =>
        new($"the application of macro '{application.Macro}'",
            application.Site is { IsSynthetic: false } site ? site.ToString() : null);

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
                $"evaluation exceeded the budget of {limit} calls while type checking:{CallStack()} (the budget cannot yet be raised from source)");
        _remaining--;
    }

    /// <summary>
    /// The chain of requests that spent the budget, outermost first (like a
    /// stacktrace), with the fixpoint a divergent evaluation keeps calling innermost.
    /// Both ends survive truncation: the outermost 3 and innermost 3 frames, with the
    /// middle counted in an elision -- the innermost frame is the one that overran, so
    /// a plain prefix cap would drop the culprit.
    /// </summary>
    private string CallStack()
    {
        const int k = 3;
        var frames = _calling is { } calling
            ? [.. _frames, new Frame($"calling {calling}", null)]
            : _frames;
        var shown = frames.Count <= 2 * k
            ? frames.Select(f => $"\n  {f}")
            : frames.Take(k).Concat<Frame>([new Frame($"\u2026 {frames.Count - 2 * k} more \u2026", null)]).Concat(frames.TakeLast(k))
                .Select(f => $"\n  {f}");
        return string.Concat(shown);
    }
}

/// <summary>
/// A running macro application: <c>expand_block</c> and <c>expand_decls</c>, which
/// expand a reflected block or declaration list where the application runs (M9).
/// </summary>
public sealed record MacroApplication(string Macro, Func<Kernel.Value, Kernel.Value> ExpandBlock, Func<Kernel.Value, Kernel.Value> ExpandDecls, SourceSpan? Site = null);
