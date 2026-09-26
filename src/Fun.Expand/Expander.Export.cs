using Fun.Kernel;

namespace Fun.Expand;

public sealed partial class Expander
{
    /// <summary>An export binds no name of its own: only the module it names is expanded.</summary>
    private Binding ExpandExport(Binding.Export export, ScopeSet active)
    {
        export = (Binding.Export)export.AddScope(active);
        return export with { Of = Expand(export.Of) };
    }
}
