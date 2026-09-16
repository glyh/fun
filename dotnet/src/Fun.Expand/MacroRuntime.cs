using Fun.Kernel;

namespace Fun.Expand;

/// <summary>What a compilation unit gives the syntax of whatever imports it: its public roles, by name.</summary>
public sealed record UnitSyntax(EquatableArray<(string Name, Role Role)> Roles);

/// <summary>
/// What expansion needs from the other side of the project boundary, where the
/// elaborator and the loader live. The expander takes one at construction and
/// never goes without: there is no expander that silently compiles nothing.
/// </summary>
// Procedural macros add their members here -- elaborating a macro body to a
// value, and applying one to a syntax object under the evaluation budget -- when
// they are ported, next to the unit loading they already need.
public interface IMacroRuntime
{
    /// <summary>
    /// A unit's syntax exports, loading the unit if it is not loaded yet. A missing
    /// unit and an import cycle are errors of the importing program.
    /// </summary>
    UnitSyntax LoadSyntax(string path);
}
