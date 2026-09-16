namespace Fun.Compiler;

/// <summary>
/// What a program produced. The conformance suite may only observe this much:
/// an <c>I64</c> renders as its digits, a constructor as its name, anything
/// else as a short debug form that no case is allowed to depend on.
/// See test/conformance/cases/README.md.
/// </summary>
public abstract record Value
{
    public sealed record I64(long N) : Value;
    public sealed record Con(string Name) : Value;
    public sealed record Other(string Debug) : Value;

    public string Describe() => this switch
    {
        I64 v => v.N.ToString(),
        Con c => c.Name,
        Other o => o.Debug,
        _ => throw new InvalidOperationException($"unhandled value {GetType().Name}"),
    };
}

/// <summary>An expansion, elaboration or evaluation failure.</summary>
public sealed class FunException(string message) : Exception(message);

/// <summary>
/// The pipeline as the REPL runs it: source to a checked term, then to a value.
/// The conformance runner and the CLI are its only callers.
/// </summary>
public static class Driver
{
    /// <summary>
    /// Expands and elaborates <paramref name="source"/>, with
    /// <paramref name="units"/> importable by name (<c>import "m"</c>).
    /// Throws <see cref="FunException"/> if it does not type check.
    /// </summary>
    public static object Elaborate(string source, IReadOnlyDictionary<string, string> units) =>
        throw new NotImplementedException("elaborator not ported yet");

    /// <summary>Runs an elaborated term. Throws <see cref="FunException"/> if it does not.</summary>
    public static Value Run(object elaborated) =>
        throw new NotImplementedException("evaluator not ported yet");
}
