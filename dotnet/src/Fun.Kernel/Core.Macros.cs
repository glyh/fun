namespace Fun.Kernel;

public abstract partial record Term
{
    /// <summary>
    /// Quoted syntax: <paramref name="Template"/> is the reflection value of the quoted
    /// form, holes still in place; evaluating fills each hole with its term's value.
    /// </summary>
    public sealed record Quote(Value Template, EquatableArray<(string Hole, Term Value)> Holes) : Term;
}
