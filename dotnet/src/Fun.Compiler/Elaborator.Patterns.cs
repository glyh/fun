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
            _ => throw new FunException("record pattern fields must follow a struct"),
        };
    }

    /// <summary>A struct's constructor fields, by label, the last of a label winning (I3).</summary>
    private static Value? FieldType(Value.VStruct structType, string name) =>
        structType.Entries.OfType<ModuleEntry.Field>().LastOrDefault(f => f.Name == name && f.Kind == MemberKind.Field)?.Value;

    // ---- pattern synonyms -----------------------------------------------------

    /// <summary>
    /// <c>pattern Flip(a, b) = Pt(b, a)</c>: the right-hand side elaborated once,
    /// where it is written, each parameter marked where its binder sits. Every
    /// binder of the right-hand side is a parameter, and every parameter is bound
    /// exactly once (both alternatives of an or-pattern count as one).
    /// </summary>
    private static (Term, Value) InferPatternSynonym(Context ctx, Syntax.PatternSynonym synonym)
    {
        var names = synonym.Params.Select(p => p.Name).ToList();
        if (names.Distinct().Count() != names.Count) throw new FunException("a pattern synonym names a parameter twice");

        var rhs = MarkSynonymParams(synonym.Rhs, names);
        var scrutineeType = RefineScrutineeType(ctx, ctx.RawMeta(), [new MatchBranch(rhs, synonym)]);
        if (HasMeta(ctx, scrutineeType))
            throw new NotImplementedException("not ported yet: a pattern synonym whose pattern does not fix its scrutinee's type");

        var (core, binders) = ElaboratePattern(ctx, rhs, scrutineeType);
        if (core.NeedsDirectMatch())
            throw new NotImplementedException("not ported yet: a pattern synonym over a type-case pattern");

        var parameters = binders.Select(b => (Index: int.Parse(b.Name[SynonymParamPrefix.Length..]), b.Type)).ToList();
        foreach (var (name, index) in names.Select((n, i) => (n, i)))
        {
            var count = parameters.Count(p => p.Index == index);
            if (count != 1)
                throw new FunException(count == 0
                    ? $"a pattern synonym's parameter `{Label(name)}` is not bound by its pattern"
                    : $"a pattern synonym's parameter `{Label(name)}` is bound twice");
        }
        if (parameters.Any(p => HasMeta(ctx, p.Type)))
            throw new NotImplementedException("not ported yet: a pattern synonym whose parameter types are not fixed");

        return (new Term.PatternSynonym(new Value.VPatternSynonym(names.Count, core, ctx.Force(scrutineeType), [.. parameters])),
                Value.VU.Instance);
    }

    private const string SynonymParamPrefix = "synonym-param#";

    /// <summary>A synonym's right-hand side with each parameter's binder marked by its position; any other binder is an error.</summary>
    private static Pattern MarkSynonymParams(Pattern pattern, List<string> names) => pattern switch
    {
        Pattern.Bind b => names.IndexOf(b.Name.Name) is var i and >= 0
            ? new Pattern.SynonymParam(i)
            : throw new FunException($"a pattern synonym's pattern binds `{Label(b.Name.Name)}`, which is not a parameter"),
        Pattern.Prod p => p with { Items = [.. p.Items.Select(x => MarkSynonymParams(x, names))] },
        Pattern.Or o => o with { Left = MarkSynonymParams(o.Left, names), Right = MarkSynonymParams(o.Right, names) },
        Pattern.Con c => c with { Args = [.. c.Args.Select(x => MarkSynonymParams(x, names))] },
        Pattern.Record r => r with { Fields = [.. r.Fields.Select(f => (f.Name, MarkSynonymParams(f.Pattern, names)))] },
        Pattern.StructType s => s with { Fields = [.. s.Fields.Select(f => (f.Name, MarkSynonymParams(f.Pattern, names)))] },
        _ => pattern,
    };

    /// <summary>
    /// The pattern synonym a pattern head resolves to - through its binder, an
    /// open, or a member path, like any other name - or null when it resolves
    /// to something else.
    /// </summary>
    private static Value.VPatternSynonym? SynonymAt(Context ctx, Syntax head) => head switch
    {
        Syntax.Var v => ctx.Force(ctx.Environment[ctx.Locate(v.Id.Name).Index]) as Value.VPatternSynonym,
        Syntax.OpenChoice c => ctx.Force(ctx.Environment[ctx.LocateChoice(c.Name.Name, c.Opens, c.Fallback).Index]) as Value.VPatternSynonym,
        Syntax.FieldAccess => ctx.Force(ctx.Eval(Infer(ctx, head).Item1)) as Value.VPatternSynonym,
        _ => null,
    };

    /// <summary>
    /// <c>Flip(first, second)</c>: each argument matched against its parameter's
    /// type and put where that parameter sits. Binders come out in the order the
    /// parameters sit in the scrutinee, as the arm's context needs.
    /// </summary>
    private static (CorePattern, List<(string Name, Value Type)>) ElaborateSynonymUse(
        Context ctx, Pattern.Con use, Value.VPatternSynonym synonym, Value type)
    {
        if (use.Args.Length != synonym.Arity)
            throw new FunException($"this pattern synonym takes {synonym.Arity} arguments, the pattern gives {use.Args.Length}");
        ctx.Unify(type, synonym.ScrutineeType);

        var args = new CorePattern[synonym.Arity];
        var binders = new List<(string, Value)>[synonym.Arity];
        foreach (var (index, paramType) in synonym.Params)
            (args[index], binders[index]) = ElaboratePattern(ctx, use.Args[index], paramType);

        return (FillSynonymParams(synonym.Rhs, args), [.. synonym.Params.SelectMany(p => binders[p.Index])]);
    }

    private static CorePattern FillSynonymParams(CorePattern pattern, CorePattern[] args) => pattern switch
    {
        CorePattern.SynonymParam p => args[p.Index],
        CorePattern.Prod p => p with { Items = [.. p.Items.Select(x => FillSynonymParams(x, args))] },
        CorePattern.Or o => o with { Left = FillSynonymParams(o.Left, args), Right = FillSynonymParams(o.Right, args) },
        CorePattern.Con c => c with { Args = [.. c.Args.Select(x => FillSynonymParams(x, args))] },
        CorePattern.Record r => r with { Fields = [.. r.Fields.Select(f => (f.Name, FillSynonymParams(f.Pattern, args)))] },
        CorePattern.StructType s => s with { Fields = [.. s.Fields.Select(f => (f.Name, FillSynonymParams(f.Pattern, args)))] },
        _ => pattern,
    };

    /// <summary>Whether a type still holds an unsolved meta anywhere its shape reveals; a shape it cannot see into counts as holding one.</summary>
    private static bool HasMeta(Context ctx, Value value) => ctx.Force(value) switch
    {
        Value.VMeta => true,
        Value.VU or Value.VAtomTy or Value.VAtom => false,
        Value.VNominal n => n.Captures.Any(c => HasMeta(ctx, c)),
        Value.VProdTy p => p.Items.Any(i => HasMeta(ctx, i)),
        Value.VProd p => p.Items.Any(i => HasMeta(ctx, i)),
        Value.VStruct s => s.Entries.OfType<ModuleEntry.Field>().Any(f => HasMeta(ctx, f.Value)),
        Value.VVar v => v.Spine.Any(a => HasMeta(ctx, a)),
        _ => true,
    };

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
        var (head, _, decl, arity) = TypeHead(ctx, pattern.Head)
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
        return (new CorePattern.NominalHead(decl, head, arity, [.. parameters]), binders);
    }

    /// <summary>
    /// What a pattern head names when it names a type: its term, the value it denotes -
    /// the nominal instance, or the projection on a sealed binder a generative module
    /// minted - the declaration, and how many parameters it takes. Any function reducing
    /// to a nominal qualifies - an alias is as good as its name. Null when the head names
    /// no type.
    /// </summary>
    private static (Term Head, Value Value, NominalDecl Decl, int Arity)? TypeHead(Context ctx, Syntax head)
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
        if (type is not Value.VU) return null;
        if (ctx.Force(value) is Value.VNominal nominal) return (term, value, nominal.Decl, arity);
        // A member of a binder sealed at a generative module (E11): the value is a
        // projection, and the sealing context records the declaration behind it.
        return SealedDecl(ctx, head) is { } decl ? (term, value, decl, arity) : null;
    }

    /// <summary>
    /// What a path names when it is one member of a sealed binder (E11): the sealing
    /// context of that binder, by member label. Null for any other path.
    /// </summary>
    private static NominalDecl? SealedDecl(Context ctx, Syntax head)
    {
        if (head is not Syntax.FieldAccess { Of: var of, Field: var member }) return null;
        var level = of switch
        {
            Syntax.Var v => ctx.Names.TryGetValue(v.Id.Name, out var bound) ? bound.Level : null,
            Syntax.OpenChoice c => ChoiceLevel(ctx, c),
            _ => null,
        };
        return level is int l && ctx.Sealed.TryGetValue(l, out var sealedMembers) && sealedMembers.TryGetValue(member, out var decl)
            ? decl
            : null;
    }

    private static int? ChoiceLevel(Context ctx, Syntax.OpenChoice c)
    {
        foreach (var label in c.Opens)
            if (ctx.Opened.TryGetValue(label, out var members) && members.TryGetValue(c.Name.Name, out var entry))
                return entry.Level;
        if (c.Fallback is not null && ctx.Names.TryGetValue(c.Fallback, out var fallback)) return fallback.Level;
        return ctx.BaseNames.TryGetValue(c.Name.Name, out var based) ? based.Level : null;
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
        Pattern.Con c => TypeHead(ctx, c.Head)?.Value,
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
        Nbe.Eval(ctx.Metas, ctx.Environment.Replace(ctx.Width - 1 - level, replacement), Nbe.Quote(ctx.Metas, ctx.Width, value));
}
