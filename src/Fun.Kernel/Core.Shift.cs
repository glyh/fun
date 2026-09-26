namespace Fun.Kernel;

public abstract partial record Term
{
    /// <summary>
    /// This term read <paramref name="amount"/> entries further in: every index at
    /// or past <paramref name="cutoff"/> moves out by that many.
    /// </summary>
    public Term Shift(int amount, int cutoff = 0) => Map((t, under) => t switch
    {
        Var v => v.Index >= under ? new Var(v.Index + amount) : v,
        // A meta is applied to the bound entries of its mask, which spans the whole
        // context; the entries shifted in join it as defined ones, which it never read.
        InsertedMeta m => m with
        {
            EntryKinds = [.. m.EntryKinds.Take(under), .. Enumerable.Repeat(EntryKind.Defined, amount), .. m.EntryKinds.Skip(under)],
        },
        _ => null,
    }, cutoff);

    /// <summary>
    /// This term with <paramref name="visit"/> applied at every subterm, outermost
    /// first: a non-null result replaces the subterm (and is not traversed further),
    /// null descends into it. <paramref name="visit"/> receives how many entries lie
    /// between this term's root and the subterm, counted from <paramref name="under"/>;
    /// each form states how many entries it pushes before a subterm, as the evaluator does.
    /// </summary>
    // The one statement of each form's binder count: Nbe.eval is what it restates,
    // and every de Bruijn traversal reads it instead of its own copy (as the
    // prototype's Core.map_subterms). A new Term kind fails
    // CoreTraversalTests.Map_covers_every_term_kind until its subterms are stated here.
    public Term Map(Func<Term, int, Term?> visit, int under = 0)
    {
        if (visit(this, under) is { } replaced) return replaced;
        Term Go(Term t, int pushed = 0) => t.Map(visit, under + pushed);
        RowTerm Row(RowTerm r, int pushed = 0) =>
            new([.. r.Effects.Select(e => Go(e, pushed))], [.. r.Tails.Select(t => Go(t, pushed))]);
        // Arm i's body sits under its binders, as many as the tree's leaves for i push.
        Term MatchArm(Term.Match match)
        {
            var binders = ArmBinders(match.Tree);
            return match with
            {
                Scrutinee = Go(match.Scrutinee),
                Bodies = binders is null ? match.Bodies : [.. match.Bodies.Select((b, i) => Go(b, binders.GetValueOrDefault(i)))],
                // An effect branch's body sits under its argument pattern's binders,
                // then the continuation innermost.
                EffectBranches = [.. match.EffectBranches.Select(b => b with
                {
                    Instance = Go(b.Instance),
                    Body = Go(b.Body, 1 + (ArmBinders(b.Argument)?.GetValueOrDefault(0)
                        ?? throw new InvalidOperationException("a handler branch's argument tree has no leaf"))),
                })],
            };
        }

        return this switch
        {
            Var or InsertedMeta or U or Atom or AtomTy or Prim or Meta or Imported
                or PatternSynonym or TraitRef or EffectDecl or EffectRowTy => this,
            Lam l => new Lam(Go(l.Body, 1)),
            Ap a => a with { Fn = Go(a.Fn), Arg = Go(a.Arg) },
            Let l => new Let(Go(l.Type), Go(l.Def), Go(l.Body, 1)),
            Pi p => p with { Domain = Go(p.Domain), Codomain = Go(p.Codomain, 1), Row = Row(p.Row, 1) },
            EffectRowLit r => new EffectRowLit(Row(r.Row)),
            Effect e => e with { Params = [.. e.Params.Select(p => Go(p))] },
            Perform p => p with { Instance = Go(p.Instance), Arg = Go(p.Arg) },
            Tunnel t => t with { Named = [.. t.Named.Select(n => Go(n))], Body = Go(t.Body) },
            RefTy r => new RefTy(Go(r.Heap), Go(r.Element)),
            RefNew r => new RefNew(Go(r.Arg)),
            RefGet r => new RefGet(Go(r.Ref)),
            RefSet r => new RefSet(Go(r.Ref), Go(r.Value)),
            Prod p => new Prod([.. p.Items.Select(i => Go(i))]),
            // The template is a closed value; only the holes are terms of this context.
            Quote q => q with { Holes = [.. q.Holes.Select(h => (h.Hole, Go(h.Value)))] },
            ProdTy p => new ProdTy([.. p.Items.Select(i => Go(i))]),
            Proj p => p with { Of = Go(p.Of) },
            Dot d => d with { Of = Go(d.Of) },
            RecordConstruct r => r with { Type = Go(r.Type), Fields = [.. r.Fields.Select(f => (f.Name, Go(f.Value)))] },
            Struct s => new Struct([.. s.ConFields.Select(f => (f.Name, Go(f.Type)))], MapBindings(s.Bindings, visit, under), s.Partial),
            Nominal n => n with { Captures = [.. n.Captures.Select(c => Go(c))] },
            Con c => c with { Args = [.. c.Args.Select(a => Go(a))], Of = Go(c.Of) },
            RecursiveOccurrence o => o with { Captures = [.. o.Captures.Select(c => Go(c))], Args = [.. o.Args.Select(a => Go(a))] },
            TraitDictTy d => d with
            {
                Args = [.. d.Args.Select(a => Go(a))],
                Operations = [.. d.Operations.Select(o => (o.Name, Go(o.Type)))],
            },
            Open o => o with { Of = Go(o.Of), Body = Go(o.Body, o.Members.Length) },
            Fix f => f with { Members = [.. f.Members.Select(m => m with { Body = Go(m.Body, f.Members.Length) })] },
            Module m => new Module(MapBindings(m.Bindings, visit, under)),
            Sig s => new Sig(Go(s.Body, 1)),
            Match m => MatchArm(m),
            _ => throw new NotImplementedException($"not ported yet: traversing {GetType().Name}"),
        };
    }

    /// <summary>
    /// How many binders each arm's body sits under, read off the decision tree's leaves;
    /// null for a tree that holds terms of its own (arms tried in order).
    /// </summary>
    private static Dictionary<int, int>? ArmBinders(DecisionTree tree)
    {
        var binders = new Dictionary<int, int>();
        bool Set(int branch, int count)
        {
            binders[branch] = count;
            return true;
        }

        bool Walk(DecisionTree? t) => t switch
        {
            null => true,
            DecisionTree.Leaf leaf => Set(leaf.Branch, leaf.Bindings.Length),
            DecisionTree.Destruct d => d.Cases.All(c => Walk(c.Tree)) && Walk(d.Default),
            DecisionTree.Switch s => s.Cases.All(c => Walk(c.Tree)) && Walk(s.Default),
            DecisionTree.TypeSwitch s => s.Cases.All(c => Walk(c.Tree)) && Walk(s.Default),
            // Arms tried in order: each arm is its own branch, counted from its pattern.
            DecisionTree.Sequential s => Enumerable.Range(0, s.Arms.Length).All(i => Set(i, s.Arms[i].Binders())),
            _ => false,
        };
        return Walk(tree) ? binders : null;
    }

    /// <summary>A binding list's terms, each read under the entries the bindings before it pushed.</summary>
    private static EquatableArray<BindingTerm> MapBindings(EquatableArray<BindingTerm> bindings, Func<Term, int, Term?> visit, int under)
    {
        var mapped = new List<BindingTerm>();
        foreach (var binding in bindings)
        {
            mapped.Add(MapBinding(binding, visit, under));
            under += BindingWidth(binding);
        }
        return [.. mapped];
    }

    /// <summary>One binding's terms, read under the entries the bindings before it pushed.</summary>
    private static BindingTerm MapBinding(BindingTerm binding, Func<Term, int, Term?> visit, int under) => binding switch
    {
        BindingTerm.Open o => o with { Of = o.Of.Map(visit, under) },
        BindingTerm.Let l => l with { Def = l.Def.Map(visit, under) },
        BindingTerm.Impl i => i with { Def = i.Def.Map(visit, under) },
        _ => throw new NotImplementedException($"not ported yet: traversing the binding {binding.GetType().Name}"),
    };

    /// <summary>How many entries a binding pushes before the binding after it.</summary>
    private static int BindingWidth(BindingTerm binding) => binding switch
    {
        BindingTerm.Open o => o.Members.Length,
        _ => 1,
    };
}
