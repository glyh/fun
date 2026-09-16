using System.Collections.Immutable;
using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// <c>P {x = p, y}</c>: the scrutinee is a record of <c>P</c>; every named field
    /// exists, none is named twice, and unless the pattern is partial every field
    /// is named. Binders come out ordered by field label - the order the match
    /// compiler visits a record's fields in - so the arm's context lines up with
    /// the decision tree's leaf.
    /// </summary>
    private static (CorePattern, List<(string Name, Value Type)>) ElaborateRecordPattern(Context ctx, Pattern.Record record, Value type)
    {
        var structType = RecordPatternType(ctx, record);
        ctx.Unify(type, structType);

        var declared = structType.Entries.OfType<ModuleEntry.Field>().Where(f => f.Kind == MemberKind.Field).ToList();
        RejectDuplicates(record.Fields.Select(f => f.Name));
        foreach (var (name, _) in record.Fields)
            if (declared.All(d => d.Name != name)) throw new FunException($"unknown record field `{name}`");
        if (!record.Partial)
            foreach (var field in declared)
                if (record.Fields.All(f => f.Name != field.Name)) throw new FunException($"missing record field `{field.Name}`");

        var fields = new List<(string, CorePattern)>();
        var binders = new List<(string Label, List<(string, Value)> Binders)>();
        foreach (var (name, pattern) in record.Fields)
        {
            var (core, fieldBinders) = ElaboratePattern(ctx, pattern, declared.Last(d => d.Name == name).Value);
            fields.Add((name, core));
            binders.Add((name, fieldBinders));
        }
        return (new CorePattern.Record([.. fields], record.Partial),
                [.. binders.OrderBy(b => b.Label, StringComparer.Ordinal).SelectMany(b => b.Binders)]);
    }

    /// <summary>The struct type a record pattern's head names: the type of the struct value it resolves to.</summary>
    private static Value.VStruct RecordPatternType(Context ctx, Pattern.Record record)
    {
        var (_, typeType) = Infer(ctx, record.Type);
        return ctx.Force(typeType) switch
        {
            Value.VStruct structType => structType,
            Value.VMeta or Value.VVar or Value.VNeutral => throw new NotImplementedException("not ported yet: a record pattern whose head has an unknown type"),
            _ => throw new FunException("record pattern fields must follow a struct"),
        };
    }

    /// <summary>A struct's constructor fields, by label, the last of a label winning (I3).</summary>
    private static Value? FieldType(Value.VStruct structType, string name) =>
        structType.Entries.OfType<ModuleEntry.Field>().LastOrDefault(f => f.Name == name && f.Kind == MemberKind.Field)?.Value;

    // ---- type-case ------------------------------------------------------------

    /// <summary><c>struct { x : p; _ }</c>: the scrutinee is a type; each field's pattern matches a type.</summary>
    private static (CorePattern, List<(string Name, Value Type)>) ElaborateStructTypePattern(Context ctx, Pattern.StructType pattern, Value type)
    {
        if (ctx.Force(type) is not Value.VStruct) ctx.Unify(type, Value.VU.Instance);
        RejectDuplicates(pattern.Fields.Select(f => f.Name));

        var fields = new List<(string, CorePattern)>();
        var binders = new List<(string Label, List<(string, Value)> Binders)>();
        foreach (var (name, field) in pattern.Fields)
        {
            var (core, fieldBinders) = ElaboratePattern(ctx, field, Value.VU.Instance);
            fields.Add((name, core));
            binders.Add((name, fieldBinders));
        }
        return (new CorePattern.StructType([.. fields], pattern.Partial),
                [.. binders.OrderBy(b => b.Label, StringComparer.Ordinal).SelectMany(b => b.Binders)]);
    }

    /// <summary>
    /// <c>Option(p)</c> against a type: the nominal the head names - a nominal type,
    /// or a type former applied to one type per parameter - each parameter
    /// matched by its pattern.
    /// </summary>
    private static (CorePattern, List<(string Name, Value Type)>) ElaborateNominalHeadPattern(Context ctx, Pattern.Con pattern)
    {
        var (head, nominal, arity) = TypeHead(ctx, pattern.Head)
            ?? throw new FunException("a type-case head must name a type");
        if (pattern.Args.Length != arity)
            throw new FunException($"this type takes {arity} parameters, the pattern gives {pattern.Args.Length}");

        var parameters = new List<CorePattern>();
        var binders = new List<(string, Value)>();
        foreach (var arg in pattern.Args)
        {
            var (core, argBinders) = ElaboratePattern(ctx, arg, Value.VU.Instance);
            parameters.Add(core);
            binders.AddRange(argBinders);
        }
        return (new CorePattern.NominalHead(nominal.Decl, head, arity, [.. parameters]), binders);
    }

    /// <summary>
    /// What a pattern head names when it names a type: its term, the nominal it
    /// reduces to with a type former's parameters as fresh metas, and how many
    /// parameters it takes. Any function reducing to a nominal qualifies - an
    /// alias is as good as its name. Null when the head names no type.
    /// </summary>
    private static (Term Head, Value.VNominal Nominal, int Arity)? TypeHead(Context ctx, Syntax head)
    {
        var (term, type) = Infer(ctx, head);
        var value = ctx.Eval(term);
        var arity = 0;
        type = ctx.Force(type);
        while (type is Value.VPi pi)
        {
            var arg = ctx.RawMeta();
            (value, type, arity) = (Nbe.Apply(ctx.Metas, value, arg), ctx.Force(Nbe.ApplyClosure(ctx.Metas, pi.Codomain, arg)), arity + 1);
        }
        return type is Value.VU && ctx.Force(value) is Value.VNominal nominal ? (term, nominal, arity) : null;
    }

    // ---- refinement -----------------------------------------------------------

    /// <summary>
    /// The level of the type variable a match refines: a scrutinee that is a
    /// variable of type <c>Type</c>. Null for anything else.
    /// </summary>
    private static int? RefinementTarget(Context ctx, Term scrutinee, Value scrutineeType) =>
        scrutinee is Term.Var v && ctx.Force(scrutineeType) is Value.VU ? ctx.Width - 1 - v.Index : null;

    /// <summary>The type a branch's pattern pins the matched type to, or null when it pins none.</summary>
    private static Value? RefinementOf(Context ctx, Pattern pattern) => pattern switch
    {
        Pattern.AtomType t => new Value.VAtomTy(t.Ty),
        Pattern.Or o => RefinementOf(ctx, o.Left) ?? RefinementOf(ctx, o.Right),
        Pattern.Con c => TypeHead(ctx, c.Head)?.Nominal,
        _ => null,
    };

    /// <summary>
    /// The context inside a type-case branch: every entry at or after the matched
    /// type variable reads it as <paramref name="replacement"/>. Entries before it
    /// have types over a context it is not part of.
    /// </summary>
    // Only the entries that can mention the variable are rewritten, each once
    // per branch - the rule, not the prototype's walk over every value.
    private static Context RefineContext(Context ctx, int level, Value replacement) => ctx with
    {
        Names = ctx.Names.ToImmutableDictionary(n => n.Key, n => n.Value.Level < level
            ? n.Value
            : n.Value with { Type = Substitute(ctx, level, replacement, n.Value.Type) }),
        SelfEntry = ctx.SelfEntry is { } self && self.Level >= level
            ? self with { Type = Substitute(ctx, level, replacement, self.Type) }
            : ctx.SelfEntry,
    };

    /// <summary>A value with the bound variable at <paramref name="level"/> read as <paramref name="replacement"/>.</summary>
    private static Value Substitute(Context ctx, int level, Value replacement, Value value) =>
        Nbe.Eval(ctx.Metas, ctx.Environment.With(ctx.Width - 1 - level, replacement), Nbe.Quote(ctx.Metas, ctx.Width, value));
}
