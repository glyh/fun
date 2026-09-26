using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>
        /// A struct's constructor field type is evaluated; evaluate the next one,
        /// or run its bindings. All field types read the struct's own environment.
        /// </summary>
        public sealed record StructField(Environment Env, Term.Struct Struct, EquatableArray<ModuleEntry> Done) : Kont;

        /// <summary>
        /// Beneath a struct's binding loop: turns the module the loop builds into
        /// the struct, its constructor fields first.
        /// </summary>
        public sealed record StructOf(EquatableArray<ModuleEntry> ConFields, bool Partial) : Kont;

        /// <summary>Beneath a signature body's binding loop: its module is partial.</summary>
        public sealed record SignatureOf : Kont;

        /// <summary>A record's struct is evaluated; evaluate its fields.</summary>
        public sealed record RecordType(Environment Env, Term.RecordConstruct Record) : Kont;

        /// <summary>A record field is evaluated; evaluate the next, or build the record.</summary>
        public sealed record RecordField(
            Environment Env, Term.RecordConstruct Record, Value Type, EquatableArray<(string Name, Value Value)> Done) : Kont;
    }

    /// <summary>What the machine does next: evaluate <c>Term</c> in <c>Env</c>, or hand on <c>Value</c>.</summary>
    private readonly record struct Step(Environment? Env, Term? Term, Value? Value)
    {
        public static Step Eval(Environment env, Term term) => new(env, term, null);
        public static Step Done(Value value) => new(null, null, value);
    }

    private static Step StartStruct(Stack<Kont> stack, Environment env, Term.Struct st)
    {
        if (st.ConFields.IsEmpty) return StartStructBindings(stack, env, st, []);
        stack.Push(new Kont.StructField(env, st, []));
        return Step.Eval(env, st.ConFields[0].Type);
    }

    private static Step ResumeStructField(Stack<Kont> stack, Kont.StructField f, Value type)
    {
        var done = f.Done.Add(new ModuleEntry.Field(f.Struct.ConFields[f.Done.Length].Name, MemberKind.Field, type));
        if (done.Length == f.Struct.ConFields.Length) return StartStructBindings(stack, f.Env, f.Struct, done);
        stack.Push(f with { Done = done });
        return Step.Eval(f.Env, f.Struct.ConFields[done.Length].Type);
    }

    private static Step StartStructBindings(Stack<Kont> stack, Environment env, Term.Struct st, EquatableArray<ModuleEntry> conFields)
    {
        stack.Push(new Kont.StructOf(conFields, st.Partial));
        return StartBindings(stack, env, st.Bindings, []) is { } next
            ? Step.Eval(next.Item1, next.Item2)
            : Step.Done(FinishModule(stack));
    }

    /// <summary>The module a binding loop finished, as the struct beneath it.</summary>
    private static Value AsStruct(Kont.StructOf f, Value module) =>
        module is Value.VModule m
            ? new Value.VStruct([.. f.ConFields, .. m.Entries], f.Partial)
            : throw new InvalidOperationException("a struct's bindings did not build a module");

    private static Value AsSignature(Value module) =>
        module is Value.VModule m
            ? m with { Entries = [.. m.Entries.Select(AsSignatureEntry)], Partial = true }
            : throw new InvalidOperationException("a signature's bindings did not build a module");

    private static Step StartRecord(Stack<Kont> stack, Environment env, Term.RecordConstruct record)
    {
        stack.Push(new Kont.RecordType(env, record));
        return Step.Eval(env, record.Type);
    }

    private static Step ResumeRecord(Stack<Kont> stack, Kont.RecordType f, Value type) =>
        NextRecordField(stack, new Kont.RecordField(f.Env, f.Record, type, []));

    private static Step ResumeRecordField(Stack<Kont> stack, Kont.RecordField f, Value value) =>
        NextRecordField(stack, f with { Done = f.Done.Add((f.Record.Fields[f.Done.Length].Name, value)) });

    private static Step NextRecordField(Stack<Kont> stack, Kont.RecordField f)
    {
        if (f.Done.Length == f.Record.Fields.Length) return Step.Done(new Value.VRecord(f.Type, f.Done));
        stack.Push(f);
        return Step.Eval(f.Env, f.Record.Fields[f.Done.Length].Value);
    }

    /// <summary>Whether a member of this kind is reachable from outside its container.</summary>
    public static bool Visible(MemberKind kind) => kind is MemberKind.Public or MemberKind.Field or MemberKind.Method;

    /// <summary>The last visible member of that name (I3).</summary>
    private static Value? VisibleMember(EquatableArray<ModuleEntry> entries, string name) =>
        entries.LastOrDefault(e => e switch
        {
            ModuleEntry.Field f => f.Name == name && Visible(f.Kind),
            ModuleEntry.Impl i => i.Name == name && Visible(i.Kind),
            _ => false,
        }) switch
        {
            ModuleEntry.Field f => f.Value,
            ModuleEntry.Impl i => i.Value,
            _ => null,
        };

    /// <summary>
    /// A struct reads back as its constructor fields, at the struct's own width,
    /// then its bindings, the ith one i entries further in, as evaluation pushes them.
    /// </summary>
    private static Term QuoteStruct(MetaContext mc, int width, Value.VStruct st)
    {
        var fields = st.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Field).ToList();
        var bindings = st.Entries.Where(e => e is not ModuleEntry.Field { Kind: MemberKind.Field }).ToList();
        return new Term.Struct(
            [.. fields.Select(f => (f.Name, Quote(mc, width, f.Value)))],
            [.. bindings.Select((e, i) => QuoteEntry(mc, width + i, e, partial: false))],
            st.Partial);
    }
}
