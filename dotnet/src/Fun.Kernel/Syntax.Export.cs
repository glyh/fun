namespace Fun.Kernel;

public abstract partial record Binding
{
    /// <summary>
    /// <c>export M</c> / <c>export M.{a, b}</c>: <c>M</c>'s public members - a
    /// module's fields, or an enum's constructors - become public members of the
    /// enclosing module, all of them or the <paramref name="Names"/> selected. It
    /// opens nothing locally. An unpublished export (<paramref name="Public"/>
    /// false, written by a macro for a declaration whose visibility it cannot see)
    /// is nothing until <c>pub</c> publishes it; a written one is public.
    /// </summary>
    public sealed record Export(Syntax Of, EquatableArray<string>? Names, bool Public) : Binding;
}
