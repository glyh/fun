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

Effect-polymorphic makers (`mk : Unit -> S can _`; a bare `Unit -> S` is pure since
[bare-arrow-is-pure](bare-arrow-is-pure.md)) are treated as possibly generative: conservative, sound,
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

## Implemented: the applicative half (2026-09-15, branch applicative-nominals)

- `VNominal` carries `captures`: the values of the declaration's own free
  variables (the variables its payload types mention), compared by conversion
  with the params. `NomRef { id; name; num_params; captures; params }` builds the
  nominal directly - the environment scan for the template by id is deleted.
  Constructors are read from the declaration (`Core.nominal_decls`) over an
  instance's captures.
- `TypeBind` and `NominalDef` carry capture terms and build the nominal and its
  constructors in the scope they are evaluated in, so a type declared under a
  binder evaluates (`mk(())`, `mk(I64)(3)`).
- Works: `Set(I64, less)` twice shares `T`; `mk(I64).T` vs `mk(Bool).T` and
  `mk(0).T` vs `mk(1).T` (a value capture, by conversion) differ; a parameter
  the declaration does not mention does not split the type.
- Fixed on the way: quoting a module value quoted every binding at one depth,
  though evaluating the `Module` pushes one entry per binding, so a later
  member's type read an earlier member instead of an outer variable.

## Open: the generative half — how an effectful maker hides its nominal

Generativity does **not** fall out of opacity. The maker's result type still
names the nominal concretely, so a generative module cannot be used with its own
type:

```fun
SymbolTable = fn(u : Unit) { module {
  table = ref(0);
  pub type Symbol = Sym(I64);
  pub intern = fn(s : I64) { table <- deref(table) + s; Sym(deref(table)) } } };
st1 = SymbolTable(()); st2 = SymbolTable(());
g = fn(x : st1.Symbol) { 1 }; g(st1.intern(5))   // rejected today: st1 is opaque
                                                  // (effectful let), so st1.Symbol is
                                                  // neutral, but intern returns the
                                                  // concrete Symbol of the maker's type
g2 = fn(x : st2.Symbol) { 1 }; g2(st1.intern(5)) // rejected - for the same wrong reason
```

Needed: when the maker performs a run-time effect, its result type abstracts the
nominal - the module type becomes a dependent signature over the result
(`sig { Symbol : Type; intern : I64 -> Symbol }`, `self.Symbol`), so
`st1.intern(5) : st1.Symbol`. Undecided: (1) where the maker's purity is read
(the lambda's inferred row at its boundary, after refs discharge?) and which rows
count (an open `can _` row: conservative generative), (2) whether sealing happens
at the lambda (its codomain) or at the effectful `let`, (3) the run-time side
(a fresh stamp capture per evaluation, so type-case also separates instances).

Also not done: a `rec` struct type under a binder still mints one identity per
elaboration (`fresh_record_id`, `ponytail:`); the same captures representation
applies uniformly (`RecOcc` + captures, `finished_records` as terms).

## Grilled (2026-09-15), part 1: captures are what the enclosing module uses

A nominal's identity captures the free variables of the **enclosing module** (the
module expression the declaration lives in), not only those its constructors
mention. So `Set(I64, less).T ≠ Set(I64, greater).T` because `union` uses `cmp`,
while a parameter nothing in the module mentions still does not split the type.
A nominal declared directly in a function body (no enclosing module) captures
what that body uses.

## Grilled (2026-09-15), part 2: generative types are named at the binding

- **A call to an effectful maker seals its result at the binding.**
  `st1 = SymbolTable(())` makes `st1.Symbol` a type unique to `st1`;
  `st2.Symbol` is distinct. Inside `st1`, `intern : String -> st1.Symbol`.
  (OCaml generative-functor behaviour.)
  ```fun
  g = fn(x : st1.Symbol) { … };
  g(st1.intern("x"))   // ok
  g(st2.intern("x"))   // rejected
  st2.name(st1.intern("x"))   // rejected
  ```
- An unbound effectful result may be used within its expression, but its fresh
  type may not escape it (avoidance: an error naming the type).
- **Purity is read from the maker's effect row at the call**; `can _` (and any
  open row) counts as possibly generative.
- **Each generative evaluation also gets a run-time stamp**, so type-case
  distinguishes `st1.Symbol` from `st2.Symbol`.
- Apply the same captures rule (part 1) to `rec` struct identities under a binder.
