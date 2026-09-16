namespace Fun.Kernel;

public abstract partial record Term
{
    /// <summary>
    /// This term read <paramref name="amount"/> entries further in: every index at
    /// or past <paramref name="cutoff"/> moves out by that many. Each form states how
    /// many entries it pushes before a subterm, as the evaluator does.
    /// </summary>
    public Term Shift(int amount, int cutoff = 0)
    {
        Term Go(Term t, int under = 0) => t.Shift(amount, cutoff + under);

        return this switch
        {
            Var v => v.Index >= cutoff ? new Var(v.Index + amount) : v,
            // A meta is applied to the bound entries of its mask, which spans the whole
            // context; the entries shifted in join it as defined ones, which it never read.
            InsertedMeta m => m with
            {
                EntryKinds = [.. m.EntryKinds.Take(cutoff), .. Enumerable.Repeat(EntryKind.Defined, amount), .. m.EntryKinds.Skip(cutoff)],
            },
            U or Atom or AtomTy or Prim or Meta or Imported or PatternSynonym => this,
            Lam l => new Lam(Go(l.Body, 1)),
            Ap a => a with { Fn = Go(a.Fn), Arg = Go(a.Arg) },
            Let l => new Let(Go(l.Type), Go(l.Def), Go(l.Body, 1)),
            Pi p => p with { Domain = Go(p.Domain), Codomain = Go(p.Codomain, 1) },
            Prod p => new Prod([.. p.Items.Select(i => Go(i))]),
            ProdTy p => new ProdTy([.. p.Items.Select(i => Go(i))]),
            Proj p => p with { Of = Go(p.Of) },
            Dot d => d with { Of = Go(d.Of) },
            Open o => o with { Of = Go(o.Of), Body = Go(o.Body, o.Members.Length) },
            Fix f => f with { Members = [.. f.Members.Select(m => m with { Body = Go(m.Body, f.Members.Length) })] },
            Module m => new Module(ShiftBindings(m.Bindings, amount, cutoff)),
            _ => throw new NotImplementedException($"not ported yet: shifting {GetType().Name}"),
        };
    }

    /// <summary>A binding list's terms, each read under the entries the bindings before it pushed.</summary>
    private static EquatableArray<BindingTerm> ShiftBindings(EquatableArray<BindingTerm> bindings, int amount, int cutoff)
    {
        var shifted = new List<BindingTerm>();
        foreach (var binding in bindings)
        {
            switch (binding)
            {
                case BindingTerm.Open o:
                    shifted.Add(o with { Of = o.Of.Shift(amount, cutoff) });
                    cutoff += o.Members.Length;
                    break;
                case BindingTerm.Let l:
                    shifted.Add(l with { Def = l.Def.Shift(amount, cutoff) });
                    cutoff += 1;
                    break;
                default:
                    throw new NotImplementedException($"not ported yet: shifting the binding {binding.GetType().Name}");
            }
        }
        return [.. shifted];
    }
}
