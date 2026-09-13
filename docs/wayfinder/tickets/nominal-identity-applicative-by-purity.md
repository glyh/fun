---
title: Nominal identity is applicative by purity
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
  - refs-in-effect-rows.md
---

# Nominal identity is applicative by purity

## Decision

A nominal's identity is its declaration plus the values of its own free
variables, compared by conversion — **applicative**. A declaration evaluated
under a run-time effect is **generative**: each evaluation is a new type.
Which applies is inferred from purity as shown in the effect row, never
declared. Vocabulary: **Nominal** in [`CONTEXT.md`](../../../CONTEXT.md).

## Use cases

Applicative — sharing required:

```fun
Set = fn(Elem : Type, cmp : Elem -> Elem -> Ordering) -> module
  pub type T = Leaf | Node(T, Elem, T)
  pub union = fn(a : T, b : T) -> …
end
a = Set(I64, compare_i64); b = Set(I64, compare_i64)
a.union(x_from_a, y_from_b)            -- must typecheck
```

Also forced by dependent types: the checker re-evaluates `Set(I64, cmp).T`
during conversion, so a type minted per evaluation would not equal itself.

Generative — sharing forbidden:

```fun
SymbolTable = fn(u : Unit) -> module
  table = ref(empty)
  pub type Symbol = private Sym(I64)
  pub intern = fn(s : String) -> …
  pub name   = fn(x : Symbol) -> …
end
st1 = SymbolTable(()); st2 = SymbolTable(())
st2.name(st1.intern("x"))              -- must be rejected
```

## Footguns this rules out

1. Generativity without an effect makes conversion non-deterministic.
2. Applicativity with an effect breaks abstraction (symbols cross tables).
3. User-declared applicative/generative is unsound: eta-expanding a generative
   maker into an applicative one subverts it (Moscow ML). Hence inferred.
4. Syntactic inference is too strict (Shao: a datatype body became
   generative). Hence purity, not transparency.
5. "Same argument" by path is brittle (OCaml: `Y = X` gives `F(X).t ≠ F(Y).t`).
   Hence conversion — which also correctly separates `Set(I64, cmp1)` from
   `Set(I64, cmp2)`. Conversion is intensional: eta matters.
6. Identity over *all* captures makes unused variables split types. Hence the
   declaration's own free variables only.
7. A purity flip (one added `ref`) must change the maker's type, or clients
   break far away. Hence [refs-in-effect-rows](refs-in-effect-rows.md).

Effect-polymorphic makers (`mk : Unit -> S can {| r}`, which is also what a bare
`Unit -> S` means) are treated as possibly generative: conservative, sound,
exact for concrete effects.

Pure code that wants a unique type per call uses a brand (rank-2 quantification,
as Haskell's `runST` or Rust's `generativity`/GhostCell), not a generative
nominal.

## Evidence today

A nominal declared under a binder does not evaluate:

```
do mk = fn(u : Unit) -> module pub type T = A | B end;
   m1 = mk(()); f = fn(x : m1.T) -> 1; f(m1.A) end
=> Nbe_error.EvalError("unbound constructor/type: T")
```

The same module bound directly (`m1 = module … end`) answers `1`.
`nominal_id` is minted once per declaration at elaboration, so neither
behaviour is implemented.

## Sources

- Dreyer, *The Design Space of ML Modules*, thesis ch. 1 §1.2.5–1.2.8
- Leroy, *Applicative functors and fully transparent higher-order modules*
- Rossberg, *1ML with Special Effects*
- OCaml manual, *Generative functors*
- Agda manual, *Module system* (module parameters become datatype parameters)
