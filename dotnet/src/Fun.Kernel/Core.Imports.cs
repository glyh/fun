namespace Fun.Kernel;

public abstract partial record Term
{
    /// <summary>
    /// A compilation unit spliced in at its import site. The unit was elaborated
    /// against the base context, so its term is anchored there and would carry the
    /// wrong indices anywhere else; its value carries its own environment, so the
    /// value is what crosses (I5). Evaluates to itself.
    /// </summary>
    public sealed record Imported(Value Value) : Term;
}
