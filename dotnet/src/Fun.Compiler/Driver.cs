using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>An expansion, elaboration or evaluation failure.</summary>
public sealed class FunException(string message) : Exception(message);

/// <summary>A program checked and ready to run, with the context it was checked in.</summary>
public sealed record Elaborated(Term Term, Value Type, Context Context);

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
    public static Elaborated Elaborate(string source, IReadOnlyDictionary<string, string> units)
    {
        try
        {
            return Elaborator.ElaborateProgram(Fun.Expand.Expander.ExpandExpr(source), new Loader(units));
        }
        // The runner sees one failure kind: where it happened is the implementation's business.
        catch (Fun.Expand.ReaderException e)
        {
            throw new FunException(e.Message);
        }
        // ponytail: `std`'s syntax roles (`type`, `if`, operators) are not ported, and
        // the enforester reads every statement against them first, so none of its
        // errors is known to be genuine yet. Reporting one as an error would pass
        // an `error` case for a missing form. Becomes FunException when roles land.
        catch (Fun.Expand.ExpandException e)
        {
            throw new NotImplementedException($"not ported yet: prelude syntax roles (enforest said: {e.Message})");
        }
    }

    /// <summary>Runs an elaborated term. Throws <see cref="FunException"/> if it does not.</summary>
    // The program's indices count the base context's entries, so it runs in that environment.
    public static Value Run(Elaborated program) =>
        program.Context.Metas.Budget.Run(() => program.Context.Eval(program.Term));

    /// <summary>
    /// What a program produced, as far as the conformance suite may observe it:
    /// an <c>I64</c> as its digits, a constructor as its name. Anything else is
    /// a debug form no case may depend on.
    /// </summary>
    public static string Describe(Value value) => value switch
    {
        Value.VAtom { Atom: Atom.I64 n } => n.Value.ToString(),
        Value.VCon c => c.Name,
        _ => value.GetType().Name,
    };
}
