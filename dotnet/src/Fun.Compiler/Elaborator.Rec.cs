using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Elaborator
{
    /// <summary>
    /// Whether a call of a function of type <paramref name="type"/> is known pure,
    /// so the checker may defer it (lazy delta). An unsolved type is not known pure.
    /// </summary>
    // Arrows carry no effect row in the port yet, and a bare arrow is pure; once
    // rows are ported this must read the row as closed and empty.
    private static bool PureCall(Context ctx, Value type) => ctx.Force(type) is Value.VPi;

    /// <summary>
    /// A recursive definition written as a type (<c>rec T = enum { … }</c>, a
    /// recursive record, or a type former returning one) is not a fixpoint.
    /// </summary>
    // Enum and struct nodes arrive with other forks, so the shape is read by name.
    private static void RejectRecursiveType(Syntax value)
    {
        while (value is Syntax.Lam lam) value = lam.Body;
        if (value.GetType().Name is "Enum" or "Struct")
            throw new NotImplementedException("not ported yet: recursive type definitions");
    }

    /// <summary>
    /// A block's <c>rec name [: T] = value</c>: the value is checked seeing its own
    /// name at the written type (or a meta), and becomes a one-member fixpoint.
    /// </summary>
    private static (Term, Value) InferRecLet(Context ctx, Syntax.Let let)
    {
        RejectRecursiveType(let.Value);
        var type = let.Type is { } written ? TypeValue(ctx, written) : ctx.RawMeta();
        var body = Check(ctx.Bind(let.Name.Name, type), let.Value, type);
        var fix = new Term.Fix([new FixMember(Label(let.Name.Name), PureCall(ctx, type), body)], 0);
        var after = ctx.Define(let.Name.Name, type, ctx.Eval(fix));
        var (bodyTerm, bodyType) = Infer(after, let.Body);
        return (new Term.Let(ctx.Quote(type), fix, bodyTerm), bodyType);
    }

    /// <summary>
    /// A module's <c>rec name = value</c>: the value is inferred seeing its own name
    /// at a meta, which its inferred type then solves.
    /// </summary>
    private static (Term, Value) InferRecMember(Context ctx, Binding.Let let)
    {
        RejectRecursiveType(let.Value is Syntax.Annotated a ? a.Inner : let.Value);
        var type = ctx.RawMeta();
        var (body, bodyType) = Infer(ctx.Bind(let.Name.Name, type), let.Value);
        ctx.Unify(type, bodyType);
        return (new Term.Fix([new FixMember(Label(let.Name.Name), PureCall(ctx, type), body)], 0), bodyType);
    }

    /// <summary>
    /// A <c>rec … and …</c> group of values: every body is checked once, seeing every
    /// member at its written type (or a meta). Member <c>i</c> is its index into one
    /// fixpoint group, read <c>i</c> entries further in than the group starts, since
    /// the members before it are pushed first.
    /// </summary>
    private static EquatableArray<(string Key, Term Term, Value Type, Value Value)> InferFixpointGroup(
        Context ctx, EquatableArray<RecMember> members)
    {
        var typed = members.Select(m =>
        {
            RejectRecursiveType(m.Value is Syntax.Annotated a ? a.Inner : m.Value);
            return m.Value is Syntax.Annotated annotated
                ? (m.Name.Name, Body: annotated.Inner, Type: TypeValue(ctx, annotated.Type))
                : (m.Name.Name, Body: m.Value, Type: ctx.RawMeta());
        }).ToList();

        var inner = typed.Aggregate(ctx, (c, m) => c.Bind(m.Name, m.Type));
        var bodies = typed.Select(m => Check(inner, m.Body, m.Type)).ToList();
        EquatableArray<FixMember> group =
            [.. typed.Zip(bodies, (m, body) => new FixMember(Label(m.Name), PureCall(ctx, m.Type), body))];

        return [.. typed.Select((m, i) =>
            (m.Name, new Term.Fix(group, i).Shift(i), m.Type, ctx.Eval(new Term.Fix(group, i))))];
    }

    /// <summary>A block's group, as nested lets pushed in member order, scoped over the rest.</summary>
    private static (Term, Value) InferLetRecGroup(Context ctx, Syntax.LetRecGroup group)
    {
        var members = InferFixpointGroup(ctx, group.Members);
        var after = members.Aggregate(ctx, (c, m) => c.Define(m.Key, m.Type, m.Value));
        var (body, bodyType) = Infer(after, group.Body);
        var term = members.Select((m, i) => (m, i)).Reverse().Aggregate(body, (acc, x) =>
            new Term.Let(Nbe.Quote(ctx.Metas, ctx.Width + x.i, x.m.Type), x.m.Term, acc));
        return (term, bodyType);
    }

    /// <summary>A module's group: one binding per member, each pushing its slot.</summary>
    private static Context InferRecGroupBinding(
        Context ctx, Binding.RecGroup group, List<BindingTerm> terms, List<ModuleEntry> entries)
    {
        var kind = group.Public ? MemberKind.Public : MemberKind.Private;
        foreach (var member in InferFixpointGroup(ctx, group.Members))
        {
            var binding = new BindingTerm.Let(Label(member.Key), kind, member.Term);
            ctx = ExtendFromSlots(ctx, binding, [(member.Key, member.Type)]);
            terms.Add(binding);
            entries.Add(new ModuleEntry.Field(binding.Name, kind, member.Type));
        }
        return ctx;
    }

    /// <summary>
    /// Applying a value whose type is not yet known to be a function: its type is
    /// taken to be an arrow from a fresh meta to a fresh meta, and unified with it.
    /// </summary>
    private static (Term, Value) InferApUnknown(Context ctx, Term fn, Value fnType, Syntax arg)
    {
        var domain = ctx.RawMeta();
        var argTerm = Check(ctx, arg, domain);
        var codomain = new Closure(ctx.Environment, ctx.Bind("_", domain).Quote(ctx.RawMeta()));
        ctx.Unify(fnType, new Value.VPi(Explicitness.Explicit, domain, codomain));
        return (new Term.Ap(fn, Explicitness.Explicit, argTerm),
                Nbe.ApplyClosure(ctx.Metas, codomain, ctx.Eval(argTerm)));
    }
}
