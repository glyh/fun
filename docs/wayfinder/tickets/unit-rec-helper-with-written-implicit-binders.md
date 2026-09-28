---
title: A recursive helper with written implicit binders fails at a unit's top level
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
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
