---
title: Eq for List and Option — the library's two impls
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
  - generic-impl-head-var-has-no-bound.md
  - recursive-match-on-two-lists-cores.md
---

# `Eq(List(A))` and `Eq(Option(A))` — the library's two impls

The one part of [the library surface](design-std-library-surface.md) that did **not**
ship, because its two halves are each blocked by a compiler gap the fork found and the
integrator reproduced:

- [A generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md)
  — `Eq.eq(h, h)` inside `impl … : Eq(List(A))` has no dictionary for `A`, and the
  failing shape breaks the **prelude load**, so it cannot even be stubbed.
- [A recursive helper matching two lists cores](recursive-match-on-two-lists-cores.md)
  — the body a real structural equality needs dumps core.

## What to do when both close

1. `std/list.fun`: `pub impl list_eq : Eq(List(A)) = module { fn eq(xs, ys) { … } };`
2. `std/option.fun`: `pub impl option_eq : Eq(Option(A)) = module { fn eq(xs, ys) { … } };`
3. `std/stage2.fun`: `export Lists.{list_eq};` and `export Options.{option_eq};` —
   **and nothing else from those units**, which is the whole point of
   [the layout](../tickets/design-std-library-surface.md#12-unit-layout--one-unit-per-module-std-re-exports-only-the-impls).

Step 3's mechanism is verified but **unexercised by real code**: with a body-trivial
probe impl, `export Lists.{probe};` does put `Eq(List(I64))` in a *program's* base scope
(measured: a no-import program comparing two lists answers `VALUE True`), and it leaves
every other member of `Lists` out of scope. This ticket is its first real use, so if the
mechanism has a flaw the probe could not see, it surfaces here.

## Cases this needs

`[1, 2] == [1, 2]` → `True`; `[1] == [2]` → `False`; `[] == []` → `True`;
`[1] == [1, 2]` → `False`; `Some(1) == Some(1)` → `True`; `Some(1) == None` → `False`;
`None == None` → `True`; `Some(1) == Some(2)` → `False`. Put them in
`test/conformance/cases/std/`, beside the pairs the surface ticket added — that
directory is the only home for a source → value test.

## Not blocking, but adjacent

The library ships without these two impls and is otherwise complete (`conformance: 857
cases, 0 failed`, merged `d70567d`). Nothing else in `std/` needs them: `Lists` and
`Options` are usable, and the bare `==`/`!=` operators work on every type the bootstrap
already has an impl for.
