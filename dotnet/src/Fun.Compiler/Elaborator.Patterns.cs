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
}
