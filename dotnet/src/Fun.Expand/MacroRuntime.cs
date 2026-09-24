using Fun.Kernel;

namespace Fun.Expand;

/// <summary>
/// What a compilation unit gives the syntax of whatever imports it: its public roles
/// and its public macros, by name.
/// </summary>
public sealed record UnitSyntax(EquatableArray<(string Name, Role Role)> Roles)
{
    public EquatableArray<(string Name, MacroEntry Macro)> Macros { get; init; } = [];

    /// <summary>The unit this unit's public members denote, e.g. <c>pub M = import "m"</c>.</summary>
    public EquatableArray<(string Name, string Path)> UnitMembers { get; init; } = [];
}

/// <summary>
/// A compiled procedural macro: its value, the position its output goes in, each
/// explicit parameter's kind (M9), and -- for a macro whose signature promises
/// types -- that signature, elaborated where the macro is defined.
/// </summary>
public sealed record MacroEntry(Value Value, FormKind Position, EquatableArray<HoleKind> Params, CompiledSignature? Signature);

/// <summary>
/// A type-aware macro's signature: <paramref name="Type"/> is the pi type over its
/// binders, its typed parameters and its output; <paramref name="Binders"/> are the
/// type binders as written, and <paramref name="Params"/> says for each explicit
/// parameter whether the signature has a domain for it.
/// </summary>
public sealed record CompiledSignature(Value Type, EquatableArray<string> Binders, EquatableArray<(string Name, bool Typed)> Params);

/// <summary>
/// What a running macro application asks of the expansion it runs in:
/// <c>expand_block</c> and <c>expand_decls</c>, answered where the application runs (M9).
/// </summary>
public sealed record MacroExpansion(Func<Syntax, Syntax> ExpandBlock, Func<EquatableArray<Binding>, EquatableArray<Binding>> ExpandDecls);

/// <summary>
/// What expansion needs from the other side of the project boundary, where the
/// elaborator, the evaluator and the loader live. The expander takes one at
/// construction and never goes without: there is no expander that silently
/// compiles nothing.
/// </summary>
public interface IMacroRuntime
{
    /// <summary>
    /// A unit's syntax exports, loading the unit if it is not loaded yet. A missing
    /// unit and an import cycle are errors of the importing program.
    /// </summary>
    UnitSyntax LoadSyntax(string path);

    /// <summary>
    /// A compilation unit's top-level binding, just expanded. It is elaborated into the
    /// unit's context as of here, so a macro defined after it compiles against it (M3):
    /// expansion and elaboration interleave one binding at a time.
    /// </summary>
    void Advance(Binding expanded);

    /// <summary>A macro's expanded definition, elaborated where it is defined and evaluated: the macro.</summary>
    Value CompileMacro(Syntax definition);

    /// <summary>A macro's expanded signature, elaborated where the macro is defined: its type.</summary>
    Value CompileSignature(Syntax signature);

    /// <summary>
    /// Applies a macro to its arguments, each as the value of its kind, under the
    /// evaluation budget: the expression it returns.
    /// </summary>
    Syntax ApplyExpr(string macro, MacroEntry entry, EquatableArray<Capture> args, MacroExpansion expansion);

    /// <summary>Applies a declaration macro: the declarations it returns.</summary>
    EquatableArray<Binding> ApplyDecls(string macro, MacroEntry entry, EquatableArray<Capture> args, MacroExpansion expansion);
}
