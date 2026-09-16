namespace Fun.Kernel;

/// <summary>A primitive value.</summary>
public abstract record Atom
{
    public sealed record Unit : Atom
    {
        public static readonly Unit Instance = new();
    }

    public sealed record I64(long Value) : Atom;
    public sealed record Char(char Value) : Atom;
    public sealed record Str(string Value) : Atom;

    /// <summary>
    /// A reflected id's scope set, plus the resolved name it was minted with if
    /// any. It has no literal syntax and no primitives, so a macro can move one
    /// but never make or inspect one (M11/M12).
    /// </summary>
    public sealed record Scopes(ScopeSet Set, string? ResolvedName) : Atom;

    public sealed override string ToString() => this switch
    {
        Unit => "()",
        I64 a => a.Value.ToString(),
        Char c => $"'{Escape(c.Value)}'",
        Str s => $"\"{s.Value}\"",
        Scopes s => $"<scopes {s.Set}>",
        _ => throw new InvalidOperationException($"unhandled atom {GetType().Name}"),
    };

    private static string Escape(char c) => c switch
    {
        '\'' => "\\'",
        '\\' => "\\\\",
        '\n' => "\\n",
        '\t' => "\\t",
        '\r' => "\\r",
        _ => c.ToString(),
    };
}

/// <summary>The type of an <see cref="Atom"/>.</summary>
public enum AtomTy { I64, Unit, Char, String, Scopes, Absurd }
