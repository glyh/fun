---
title: A generic impl's head variable carries no bound
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# A generic impl's head variable carries no bound

Found 2026-09-27 by the [library surface](design-std-library-surface.md) fork, which
stopped rather than stub around it, and reproduced by the integrator. The landed half
is the *head*: an impl whose head is a pattern over types registers and serves its
instances — [trait-op-takes-innermost-impl](trait-op-takes-innermost-impl.md) landed
that on 2026-09-27, and `test/conformance/cases/values/trait-generic-impl.fun` pins it
("an impl head's free name is its own type variable: `Option(A)` serves `Option(I64)`").
The missing half is the **bound**: the head's free name carries no trait bound, so
there is no dictionary to pass for it inside the method body.

## The measurement

Four configurations, each a one-line addition to `std/list.fun` plus
`export Lists.{probe};` in `std/stage2.fun`, then `dotnet build` and a program
asking `Cons[I64](1, Nil[I64]) == Cons[I64](1, Nil[I64])`:

| impl | body | `{ 1 }` (control) | the probe |
| --- | --- | --- | --- |
| `Eq(List(I64))` | `True` | `VALUE 1` | `VALUE True` |
| `Eq(List(A))` | `True` | `VALUE 1` | **`VALUE True`** — the head registers |
| `Eq(List(I64))` | `match (xs) { Nil => True, Cons(h, t) => Eq.eq(h, h) }` | `VALUE 1` | `VALUE True` — element evidence resolves |
| `Eq(List(A))` | same body | **fails** | `cannot choose an implementation of Eq…` |

So the head is not the problem: a generic `Eq(List(A))` serves `List(I64)` when its
body needs no evidence, and a *concrete* head can call `Eq.eq` on its elements
because `Eq(I64)` is in scope. Only the generic head *using its own variable's
evidence* fails, and the error is `ELAB cannot choose an implementation of \`Eq\`: its
argument type is never known`.

**The failure mode is what makes this urgent rather than merely missing.** The error
is raised while the *unit* elaborates, so the whole prelude fails to load: with that
impl present, `{ 1 }` fails identically. There is no way to ship this shape and fail
one program; it takes every program with it. That is why the fork could not stub it
and why the library ships without `Eq(List(A))`.

## Why it matters beyond the library

- **It blocks the library's two impls** — [Eq for List and Option](std-eq-for-list-and-option.md)
  is blocked on this and on [the recursion crash](recursive-match-on-two-lists-cores.md).
- **It is what `derive` rests on.** A derived `Eq` for a container is exactly an impl
  whose head binds the element type and whose body uses the element's evidence, so
  [trait library deriving and protocols](design-trait-library-deriving-and-protocols.md)
  inherits it: "generated impls live in the defining module" assumes the generated
  body can *call* the generated instance for its parts.
- **The nearest existing escape is a workaround, not the feature.** A named impl is a
  handle in evidence position (`same[A.C, A.eq_C](x, y)`), so a body can be handed an
  instance explicitly — but the head's own variable has no such name to hand over,
  which is the gap.

## Reading

- `src/Fun.Compiler/Elaborator.Traits.cs` — `TraitEvidence`, `PendingEvidence`, and
  the context's `Evidence` list
- `src/Fun.Compiler/Elaborator.Implicits.cs` — `ResolveEvidence`, `AddEvidence`, the
  dictionary threading this body never gets
- `test/conformance/cases/values/trait-generic-impl.fun`,
  `test/conformance/cases/imports/trait-generic-impl-in-imported-module.fun` — what
  the head half already guarantees, and the shape to extend rather than replace
- [impl visibility](../topics/impl-visibility.md) — why resolution stays scoped, and
  the named-impl escape hatch this sits beside
