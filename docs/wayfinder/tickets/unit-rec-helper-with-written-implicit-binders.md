---
title: A recursive helper with written implicit binders fails at a unit's top level
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-28
resolution: Closed 2026-09-28 (`88f862f` fix, `704e037` case, merged as `fork/unit-rec-written-implicit-binders`). Both spellings now work at a unit's top level — the binder on the declaration's type (`cannot unify VPi with VPi` → `VALUE 1`) and the typed-parameters lambda with a written result type (`VVar with VVar` → `VALUE 1`) — and the case is `imports/unit-rec-written-implicit-binders`, with its own unit covering both plus a helper serving an impl so a duplicate `open` quotes it. Suite `920` → **`921` cases, 0 failed**; xUnit `206`. **The ticket's premise was imprecise and the fork corrected it:** the boundary is a *module* member `rec` (a unit is a module) — the same spelling inside a nested `module { … }` was broken identically — and the typed-parameters spelling also failed at program and block level, so "works elsewhere" was only true of the binder-on-type form. Root cause: `InferRecMember` bound the helper's own name at a fresh **meta**, ignoring the written type, so a self-call could not insert the hidden dictionary a trait bound desugars to; the fix peels the annotation and binds at the written type, exactly as a block's `rec` does (`Elaborator.Rec.cs`, `InferRecMember` and `InferRecLet`). **The `std/list.fun` wrapper is no longer needed** — the direct helper under `impl Eq(List(I64))` with a duplicate open evaluates correctly — but `std/` was left untouched per its brief, and the stale comment there is noted on [Eq for List and Option](std-eq-for-list-and-option.md). Two residues recorded, not chased: the ticket-literal `fn[B : Type](xs, ys) : Bool` is refused **uniformly** by the params-must-be-typed rule (not unit-specific), and unforced reads of module fields holding fix applications print `VGlued` — a pre-existing lazy-delta display, both builds.
assignee:
blocked_by: []
---

# A recursive helper with written implicit binders fails at a unit's top level

Found 2026-09-28 by the fork writing `Eq(List(a))` for the prelude, while looking for a place to
put the recursion. **Reported by that fork; not yet re-measured by the integrator** — the first
thing a fork taking this should do is reproduce it.

```fun
# at a UNIT's top level (the `.unit-*.fun` form a program imports):
rec go : [B : Eq] -> List(B) -> List(B) -> Bool = fn[B : Type](xs, ys) { … };
#   -> ELAB type mismatch: cannot unify VPi with VPi
```

Its measurements, as reported: the same declaration **works at a program's top level and at block
level**, so the difference is the *unit* path. Adding the binder to the lambda instead of the type
— `rec go = fn[B : Type](xs, ys) : Bool { … }` — fails the same way.

It matters because a unit is where library code lives: `std/list.fun` wanted exactly this shape
and had to route around it with a non-recursive wrapper holding a nested `rec go`, which works and
is what shipped. So this is a papercut on the prelude's authoring surface rather than a library
blocker — but a unit's bindings are elaborated and then *evaluated* for import, and a recursive
helper with written implicit binders is evidently where the two disagree.

## To do

Reproduce first: a unit plus a program that imports it — `test/conformance/cases/imports/*.unit-*.fun`
is the precedent for that pair. Then find how the unit path elaborates the helper's binders
differently from the program path. The case belongs in `cases/imports/`, and it should cover the
binder-on-the-type and binder-on-the-lambda spellings, since both were reported failing at a unit's
top level while working elsewhere.
