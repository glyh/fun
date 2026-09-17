using Fun.Kernel;

namespace Fun.Compiler;

public sealed partial record Context
{
    /// <summary>The expander of the syntax being elaborated: a typed macro call is applied and expanded with it.</summary>
    public Fun.Expand.Expander? Expander { get; init; }
}

public static partial class Elaborator
{
    /// <summary>
    /// A call to a macro whose signature promises types (macro-annotation decisions): it
    /// applies like a function over types. Its type binders, and an output that promises
    /// nothing, become metas; each <c>(x : Expr(T))</c> argument is checked at <c>T</c>; the
    /// result type meets <paramref name="expected"/>. Every binder must be solved by then,
    /// for the macro runs with each as the reflected type it was solved to. Its output is
    /// expanded in place (M6) and checked at the type it promised.
    /// </summary>
    // ponytail: a typed argument elaborates again where the output places it (the
    // prototype reuses that elaboration); reuse it if elaboration cost matters.
    private static (Term, Value) ApplyTypedMacro(Context ctx, Syntax.MacroCall call, Value? expected)
    {
        var expander = ctx.Expander ?? throw new NotImplementedException("not ported yet: a type-aware macro's call with no expander");
        var loader = ctx.Loader ?? throw new NotImplementedException("not ported yet: a type-aware macro's call with no loader");
        var key = call.Head is Syntax.Var v ? v.Id.Name : throw new InvalidOperationException("a deferred macro call's head is its key");
        var entry = expander.LookupMacro(key) ?? throw new InvalidOperationException($"a deferred call names no macro: {key}");
        var signature = entry.Signature ?? throw new InvalidOperationException($"a deferred call names an untyped macro: {key}");
        var macro = key.IndexOf('#') is var at and >= 0 ? key[..at] : key;

        var type = signature.Type;
        var metas = new List<Value>();
        while (ctx.Force(type) is Value.VPi { Explicitness: Explicitness.Implicit } implicitPi)
        {
            var meta = ctx.Eval(FreshMeta(ctx));
            metas.Add(meta);
            type = Nbe.ApplyClosure(ctx.Metas, implicitPi.Codomain, meta);
        }

        // A plain argument is syntax the macro reads; a typed one elaborates too.
        EquatableArray<Capture> args = [.. call.Args.Select(a => a is Capture.Expr { Syntax: Syntax.Stx stx } ? new Capture.Expr(stx.Inner) : a)];
        foreach (var ((param, typed), arg) in signature.Params.Zip(args))
        {
            if (!typed) continue;
            if (arg is not Capture.Expr e || ctx.Force(type) is not Value.VPi { Explicitness: Explicitness.Explicit } pi)
                throw new InvalidOperationException("a typed parameter's argument is an Expr in the signature's order");
            // What the argument performs happens where the output places it.
            var (core, _) = Collecting(ctx, c => Blamed($"the argument {param} of macro {macro}", () => Check(c, expander.Expand(e.Syntax), pi.Domain)));
            type = Nbe.ApplyClosure(ctx.Metas, pi.Codomain, ctx.Eval(core));
        }
        if (expected is not null) ctx.Unify(expected, type);

        var binders = signature.Binders.Zip(metas).Select(b => ctx.Force(b.Second) is Value.VMeta
            ? throw new FunException($"the type binder {b.First} of macro {macro} is not solved at its call")
            : ctx.Force(b.Second)).ToList();
        var promised = ctx.Force(type);
        var output = expander.ApplyTyped(args, (received, expansion) => loader.ApplyMacro(macro, entry.Value, binders, received, expansion,
            o => Reflection.OfPrelude.ReadExpr(o) ?? throw new FunException($"macro {macro} did not return syntax")));
        return (Blamed($"the output of macro {macro}", () => Check(ctx, output, promised)), promised);
    }

    /// <summary>A type error in what a typed macro call checks names the part of the call it is in.</summary>
    private static Term Blamed(string part, Func<Term> check)
    {
        try
        {
            return check();
        }
        catch (FunException e) when (e.Message.StartsWith("type mismatch: ", StringComparison.Ordinal))
        {
            throw new FunException($"type mismatch in {part}: {e.Message["type mismatch: ".Length..]}");
        }
    }

    /// <summary><c>quote(…)</c> is an <c>Expr</c>; <c>quote { … }</c> is the list of its items.</summary>
    private static (Term, Value) InferQuote(Context ctx, Syntax quote)
    {
        var r = Reflection.OfPrelude;
        return quote switch
        {
            Syntax.Quote q => (QuoteTerm(ctx, r.ReflectExpr(q.Template), q.Holes), r.ExprType),
            Syntax.QuoteDecls q => (QuoteTerm(ctx, r.ReflectDecls(q.Items), q.Holes), r.DeclsType),
            _ => throw new InvalidOperationException($"not a quote: {quote.GetType().Name}"),
        };
    }

    /// <summary>
    /// <c>quote { … }</c> where one <c>Decl</c> is expected -- a <c>: Decl</c> macro's body --
    /// is that one declaration; anywhere else it is the list of its items.
    /// </summary>
    private static Term? CheckQuoteDecl(Context ctx, Syntax.QuoteDecls quote, Value expected)
    {
        var r = Reflection.OfPrelude;
        if (!Nbe.Convertible(ctx.Metas, ctx.Width, expected, r.DeclType)) return null;
        return quote.Items is [var item] and not [Binding.Hole]
            ? QuoteTerm(ctx, r.ReflectDecl(item), quote.Holes)
            : throw new FunException($"quote {{ … }} where one declaration is expected holds {quote.Items.Length}");
    }

    /// <summary>
    /// A quote's template and holes: each hole is checked at the reflection type its
    /// positions in the template give it; one hole in positions of two kinds is an error.
    /// </summary>
    private static Term QuoteTerm(Context ctx, Value template, EquatableArray<(string Hole, Syntax Value)> holes)
    {
        var r = Reflection.OfPrelude;
        var occurrences = QuoteHoles.Occurrences(template);
        return new Term.Quote(template, [.. holes.Select(h =>
        {
            Value expected = occurrences.Where(o => o.Hole == h.Hole).Select(o => o.Kind).Distinct().ToList() switch
            {
                [QuoteHoles.Kind.Expr] => r.ExprType,
                [QuoteHoles.Kind.Pattern] => r.PatternType,
                [QuoteHoles.Kind.Decl] => r.DeclsType,
                [QuoteHoles.Kind.Id] => r.IdType,
                _ => throw new FunException($"the quote hole {h.Hole} stands in positions of different kinds"),
            };
            return (h.Hole, Check(ctx, h.Value, expected));
        })]);
    }
}
