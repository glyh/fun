using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>
        /// A dictionary type's arguments and operation types are evaluated, as one
        /// tuple, arguments first: build the dictionary type.
        /// </summary>
        public sealed record TraitDictOf(TraitDecl Decl, int ArgCount, EquatableArray<string> Operations) : Kont;
    }

    /// <summary>Starts a dictionary type: its parts evaluate as a tuple beneath a <see cref="Kont.TraitDictOf"/> frame.</summary>
    private static Term StartTraitDict(Stack<Kont> stack, Term.TraitDictTy dict)
    {
        stack.Push(new Kont.TraitDictOf(dict.Decl, dict.Args.Length, [.. dict.Operations.Select(o => o.Name)]));
        return new Term.Prod([.. dict.Args, .. dict.Operations.Select(o => o.Type)]);
    }

    private static Value TraitDict(Kont.TraitDictOf f, Value parts)
    {
        var items = ((Value.VProd)parts).Items;
        return new Value.VTraitDict(f.Decl, [.. items.Take(f.ArgCount)],
            [.. f.Operations.Select((name, i) => (name, items[f.ArgCount + i]))]);
    }

    /// <summary>A trait's operation types at one argument: each operation's closure over it.</summary>
    public static EquatableArray<(string Name, Value Type)> OperationTypes(MetaContext mc, TraitDecl trait, Value arg) =>
        [.. trait.Operations.Select(o => (o.Name, ApplyClosure(mc, o.Type, arg)))];

    /// <summary>
    /// What <c>open</c> pushes for an impl: the module's <c>Index</c>th public impl,
    /// or, for a module known only by its type, its named impl projected by name.
    /// </summary>
    public static Value OpenedImpl(Value module, OpenMember.Impl impl) => module switch
    {
        Value.VModule m => m.Entries.OfType<ModuleEntry.Impl>().Where(e => e.Kind == MemberKind.Public).ElementAt(impl.Index).Value,
        _ when impl.Name is { } name => DotValue(module, name),
        _ => throw new InvalidOperationException("an anonymous impl opened from a value that is not a module"),
    };

    /// <summary>A signature's impl entry holds the dictionary type in both places: it is what the module must provide.</summary>
    private static ModuleEntry AsSignatureEntry(ModuleEntry entry) =>
        entry is ModuleEntry.Impl impl ? impl with { DictType = impl.Value } : entry;

    private static Term QuoteTraitDict(MetaContext mc, int width, Value.VTraitDict dict) =>
        new Term.TraitDictTy(dict.Decl, [.. dict.Args.Select(a => Quote(mc, width, a))],
            [.. dict.Operations.Select(o => (o.Name, Quote(mc, width, o.Type)))]);

    /// <summary>
    /// A binding of a module's value (or type, when <paramref name="partial"/>) read
    /// back at <paramref name="width"/>: a field as a <c>let</c>, an impl as an impl.
    /// </summary>
    private static BindingTerm QuoteEntry(MetaContext mc, int width, ModuleEntry entry, bool partial) => entry switch
    {
        ModuleEntry.Field f => new BindingTerm.Let(f.Name, f.Kind, Quote(mc, width, f.Value)),
        ModuleEntry.Impl i => partial
            ? new BindingTerm.Impl(i.Name, i.Kind, Quote(mc, width, i.DictType), Value.VU.Instance)
            : new BindingTerm.Impl(i.Name, i.Kind, Quote(mc, width, i.Value), i.DictType),
        _ => throw new InvalidOperationException($"unhandled module entry {entry.GetType().Name}"),
    };
}
