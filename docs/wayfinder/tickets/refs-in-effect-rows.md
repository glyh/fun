---
title: Refs belong in effect rows
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Refs belong in effect rows

## Decision

Mutation is **three heap effects** — `Alloc(h)`, `Read(h)`, `Write(h)` — each
parameterised by the heap it acts on, Koka's shape. References are branded by
their heap: surface `Ref(A)` abbreviates `Ref(h, A)`. Decided in the effects
domain-model pass
([core-tt-domain-model-effects](../topics/core-tt-domain-model-effects.md));
vocabulary: **Heap**, **Reference**, **Mutation effect**, **Discharge** in the
root [`CONTEXT.md`](../../../CONTEXT.md).

Allocating, reading or writing a ref is a run-time effect that appears in the
effect row. Reopens the first-pass choice in
[references](../topics/references.md) — "no user-visible effect-row tracking
for refs" — which guards elaboration with the syntactic `compile_time_safe`
check instead.

The three-way split was chosen over a merged `Mut(h)` so read-only code earns
the weaker row (`peek : Ref(A) -> A can Read(h)`); a heap parameter over a
constant effect so discharge can be automatic. There is no `Ref` effect: `Ref`
is the type, and effect families are bare names in the same namespace.

## Why

[Nominal identity](nominal-identity-applicative-by-purity.md) is generative
exactly under a run-time effect. If refs are invisible in types, purity is
invisible too, and:

- **A library adds a cache, clients break far away.** One `table = ref(empty)`
  inside a module maker flips its types to generative with no change to the
  maker's type; the error surfaces in client code as `a.Symbol ≠ b.Symbol`.
- **Higher-order code cannot be both sound and useful.**

  ```fun
  merge_twice = fn(mk : Unit -> SetSig can {}) ->
    do a = mk(()); b = mk(()); a.union(a.empty, b.empty) end
  ```

  Without purity in `mk`'s type the checker must either reject this for every
  caller, or accept `merge_twice(SymbolTable)` and let symbols cross tables.
  With the heap effects visible, a pure `mk` (`can {}`, or a bare arrow) is
  applicative and `SymbolTable` is a type error at the call.

## Cost

- Allocation shows in types: `counter : Unit -> Ref(I64) can Alloc(h)` — until
  discharged; `ref : A -> Ref(A) can Alloc(h)`, `deref : Ref(A) -> A can
  Read(h)`, `r <- e : Unit can Write(h)`.
- Internal-only refs discharge at generalisation — a heap that does not occur
  in the definition's type has its effects dropped
  ([Koka book §3.2.5](https://koka-lang.github.io/koka/doc/book.html)); `make =
  fn(u) -> do t = ref(0); bump(t); get(t) end` infers `Unit -> I64`, pure. An
  escaping reference carries its heap into the result type and blocks
  discharge — that is the runST condition, met by inference.
- Type-level machinery: heap variables in rows and in `Ref`'s arity (hidden in
  surface syntax), plus the escape check at generalisation.
- Generic helpers need effect polymorphism — required by effects anyway.

`compile_time_safe` may survive as the elaborator's implementation of "do not
run this during type checking", but it is no longer where purity is decided.

## Grilled (2026-09-15), part 1: the heap is never written

- **The heap `h` is an implementation detail.** Surface `Ref(A)` has one argument;
  no `Heap` kind or heap binder is ever written. The checker tracks each ref's
  heap internally and drops the heap effects when no ref of that heap escapes
  (so local mutation is pure from outside).
- **A signature says what it mutates by naming the ref**, or infers it; both are
  valid:
  ```fun
  bump = fn(r : Ref(I64)) can {Mutate(r)} { r <- !r + 1 }   // names the ref
  bump = fn(r : Ref(I64)) can _ { r <- !r + 1 }             // inferred
  ```
  `Mutate(r)` maps to `r`'s hidden heap. An effect row may therefore mention a
  parameter (see small-followups item 2: method rows are expanded in the wrong
  scope for this).
- **One surface effect, `Mutate(r)`,** covers allocation, reading and writing a
  ref's heap. A later split (`Mutate(r)` = `{Read(r), Write(r)}`) stays
  compatible; add it only when concurrency or `const`-style contracts need it.

## Grilled (2026-09-15), part 3: top-level refs, discharge, heap grouping

- **Top-level refs are allowed.** The program entry is elaborated inside a
  runtime-provided heap handler (the `runtime_handled_effects` seam of the
  top-level unhandled-effect check), so `Counter = module { pub count = ref(0) }`
  works; functions touching `count` still declare `Mutate(count)` (or `can _`).
- **Discharge site:** a ref's heap effect is dropped at any `let`, function or
  block whose result type and captured variables do not mention that heap.
- **Heap grouping:** every `ref(…)` starts its own hidden heap; heaps merge only
  when unification forces it (e.g. two refs stored in one list).
- Surface `Ref(A)` keeps one argument, so `Ref(I64)` and existing annotations
  stay valid.
