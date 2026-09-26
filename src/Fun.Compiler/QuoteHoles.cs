using Fun.Kernel;

namespace Fun.Compiler;

/// <summary>
/// Holes in quoted syntax, found and filled on its reflection value. A hole <c>$x</c> is
/// an id spelled <c>"$x"</c> (<c>$</c> cannot begin a source identifier). Where it sits
/// decides its kind (M10): as an expression variable's id it stands for an <c>Expr</c>,
/// as a pattern binder's for a <c>Pattern</c>, as a quoted item for <c>Decls</c> -- the
/// declarations it splices into the items around it -- and anywhere else (a binder, a
/// field's id) for an <c>Id</c>. An identifier token spelled <c>$x</c>, a generated
/// rule's head, is an <c>Id</c> too, filled as the token spelling that id.
/// </summary>
public static class QuoteHoles
{
    public enum Kind { Expr, Pattern, Decl, Id }

    private static string? Spelling(Value v) => v is Value.VAtom { Atom: Atom.Str { Value: ['$', _, ..] name } } ? name : null;

    /// <summary>The hole an <c>Id</c> record spells, if it spells one.</summary>
    private static string? HoleName(Value v) =>
        v is Value.VRecord r && r.Fields.Any(f => f.Name == "scope") && r.Fields.FirstOrDefault(f => f.Name == "name").Value is { } name
            ? Spelling(name)
            : null;

    private static string? TokenHoleName(Value v) =>
        v is Value.VCon { Name: "Tok", Args: [_, Value.VCon { Name: "IdentTok", Args: [var name] }, _] } ? Spelling(name) : null;

    /// <summary>The identifier token <paramref name="token"/> was written as, spelling the <c>Id</c> <paramref name="id"/> instead.</summary>
    private static Value TokenOfId(Value token, Value id) =>
        (token, id) is (Value.VCon { Args: [var span, Value.VCon kind, _] } tok, Value.VRecord record)
            ? tok with { Args = [span, kind with { Args = [Field(record, "name")] }, Field(record, "scope")] }
            : throw new FunException("a token hole filled with a value that is not an Id");

    private static Value Field(Value.VRecord record, string name) => record.Fields.First(f => f.Name == name).Value;

    /// <summary>The list <paramref name="items"/> followed by <paramref name="tail"/>.</summary>
    private static Value Append(Value items, Value tail) => items switch
    {
        Value.VCon { Name: "Nil" } => tail,
        Value.VCon { Name: "Cons", Args: [var head, var rest] } cell => cell with { Args = [head, Append(rest, tail)] },
        _ => throw new FunException("a declaration hole filled with a value that is not a list"),
    };

    private static Value MapHoles(Func<Kind, string, Value, Value> onHole, Value v)
    {
        switch (v)
        {
            // A declaration hole splices its declarations in place of its item.
            case Value.VCon { Name: "Cons", Args: [Value.VCon { Name: "DeclHole", Args: [var id] } head, var tail] } cell when HoleName(id) is { } hole:
            {
                var filled = onHole(Kind.Decl, hole, head);
                var rest = MapHoles(onHole, tail);
                return ReferenceEquals(filled, head) ? cell with { Args = [head, rest] } : Append(filled, rest);
            }
            case Value.VCon { Name: "RawVar" or "RawPatBind", Args: [_, var id] } con when HoleName(id) is { } hole:
                return onHole(con.Name == "RawVar" ? Kind.Expr : Kind.Pattern, hole, v);
            case Value.VCon { Name: "DeclHole", Args: [var id] } when HoleName(id) is { } hole:
                return onHole(Kind.Decl, hole, v);
            case Value.VCon when TokenHoleName(v) is { } hole:
                return onHole(Kind.Id, hole, v);
            case Value.VCon con:
                return con with { Args = [.. con.Args.Select(a => MapHoles(onHole, a))] };
            case Value.VRecord record:
                return HoleName(v) is { } name
                    ? onHole(Kind.Id, name, v)
                    : record with { Fields = [.. record.Fields.Select(f => (f.Name, MapHoles(onHole, f.Value)))] };
            default:
                return v;
        }
    }

    /// <summary>Every hole occurrence, in order, with the kind its position gives it.</summary>
    public static List<(string Hole, Kind Kind)> Occurrences(Value template)
    {
        var found = new List<(string, Kind)>();
        MapHoles((kind, name, v) => { found.Add((name, kind)); return v; }, template);
        return found;
    }

    /// <summary><paramref name="template"/> with each hole <paramref name="values"/> names filled.</summary>
    public static Value Fill(Value template, IReadOnlyDictionary<string, Value> values) =>
        MapHoles((kind, name, v) => values.TryGetValue(name, out var filled)
            ? TokenHoleName(v) is not null ? TokenOfId(v, filled) : filled
            : v, template);
}
