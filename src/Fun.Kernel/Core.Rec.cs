namespace Fun.Kernel;

/// <summary>
/// A member of a recursive group: its binder's label (for errors), whether a call
/// of it is known pure, and its body, read under one entry per group member.
/// </summary>
public sealed record FixMember(string Name, bool Pure, Term Body);

public abstract partial record Term
{
    /// <summary>
    /// The <paramref name="Index"/>th member of a group of mutually recursive
    /// definitions (<c>rec f = … and g = …</c>; a single <c>rec</c> is a group of
    /// one). Every body sits under one entry per member, the first member outermost.
    /// </summary>
    public sealed record Fix(EquatableArray<FixMember> Members, int Index) : Term;
}

public abstract partial record Value
{
    /// <summary>The <paramref name="Index"/>th member of a recursive group, closed over its environment.</summary>
    public sealed record VFix(EquatableArray<FixMember> Members, Environment Environment, int Index) : Value
    {
        public FixMember Member => Members[Index];

        /// <summary>The environment a member's body runs in: every member, the first outermost.</summary>
        public Environment BodyEnvironment() =>
            Enumerable.Range(0, Members.Length).Aggregate(Environment, (env, i) => env.Push(this with { Index = i }));

        /// <summary>
        /// Whether <paramref name="other"/> is this very fixpoint: the same member of
        /// the same group closed over the same environment.
        /// </summary>
        // ponytail: environment identity is by reference, so a fixpoint re-evaluated in
        // an equal but separately built environment is not recognised; comparison then
        // unfolds, which is slower but still correct.
        public bool IsSame(VFix other) =>
            Index == other.Index && Members == other.Members && ReferenceEquals(Environment, other.Environment);
    }

    /// <summary>
    /// A known-pure fixpoint applied to <paramref name="Arg"/> while type checking,
    /// unfolded only when something inspects it: conversion compares two calls of
    /// the same fixpoint by their arguments first (lazy delta).
    /// </summary>
    public sealed record VGlued(VFix Fix, Value Arg, Lazy<Value> Unfolded) : Value;
}
