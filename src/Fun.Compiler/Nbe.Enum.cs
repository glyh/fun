using Fun.Kernel;

namespace Fun.Compiler;

public static partial class Nbe
{
    private abstract partial record Kont
    {
        /// <summary>A nominal's captures are evaluated (as a tuple); build the nominal.</summary>
        public sealed record NominalOf(NominalDecl Decl) : Kont;
    }

    /// <summary>
    /// A constructor as a value of <paramref name="nominal"/>: a nullary one is
    /// the constructed value itself, any other the curried function taking its
    /// payloads. Unknown tags are a type error the elaborator already reported.
    /// </summary>
    private static Value ConstructorValue(Value.VNominal nominal, string name)
    {
        var constructor = nominal.Decl.Constructor(name) ?? throw new FunException($"`{nominal.Decl.Name}` has no constructor `{name}`");
        var arity = constructor.Payloads.Length;
        if (arity == 0) return new Value.VCon(name, [], nominal);

        // fn(p0) { … fn(p_{n-1}) { Con(name, [p0 … p_{n-1}], nominal) } }, closed over
        // an environment holding just the nominal.
        Term body = new Term.Con(name, [.. Enumerable.Range(0, arity).Select(i => (Term)new Term.Var(arity - 1 - i))], new Term.Var(arity));
        for (var i = 1; i < arity; i++) body = new Term.Lam(body);
        return new Value.VLam(new Closure(Environment.Empty.Push(nominal), body));
    }

    /// <summary>
    /// What <c>open T</c> binds for a constructor of <c>T</c>: for a former of
    /// <c>k</c> parameters, <c>fn(A1 … Ak) { T(A1, …, Ak).C }</c>.
    /// </summary>
    private static Value OpenedConstructor(Value type, OpenMember.Constructor constructor)
    {
        if (constructor.FormerArity == 0) return DotValue(type, constructor.Name);
        Term applied = new Term.Var(constructor.FormerArity);
        for (var j = constructor.FormerArity - 1; j >= 0; j--) applied = new Term.Ap(applied, Explicitness.Explicit, new Term.Var(j));
        Term body = new Term.Dot(applied, constructor.Name);
        for (var j = 1; j < constructor.FormerArity; j++) body = new Term.Lam(body);
        return new Value.VLam(new Closure(Environment.Empty.Push(type), body));
    }

    /// <summary>The end of a constructor's lambda chain: every part is a variable of it.</summary>
    private static Value Construct(Environment env, Term.Con con)
    {
        Value At(Term t) => t is Term.Var v ? env[v.Index] : throw new InvalidOperationException("a constructor's parts are variables of its chain");
        return new Value.VCon(con.Name, [.. con.Args.Select(At)],
            At(con.Of) as Value.VNominal ?? throw new InvalidOperationException("a constructor builds a nominal"));
    }

    /// <summary>A constructor's payload types for one nominal instance: its payload terms over the instance's captures.</summary>
    public static EquatableArray<Value> PayloadTypes(MetaContext mc, Value.VNominal nominal, ConstructorDecl constructor)
    {
        var captures = nominal.Captures.Aggregate(Environment.Empty, (env, c) => env.Push(c));
        return [.. constructor.Payloads.Select(p => Eval(mc, captures, p))];
    }

    private static Term QuoteConstructed(MetaContext mc, int width, Value.VCon con) =>
        con.Args.Aggregate((Term)new Term.Dot(Quote(mc, width, con.Nominal), con.Name),
            (acc, arg) => new Term.Ap(acc, Explicitness.Explicit, Quote(mc, width, arg)));
}
