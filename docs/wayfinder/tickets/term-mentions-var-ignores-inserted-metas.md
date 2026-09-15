---
title: term_mentions_var treats an inserted meta as mentioning nothing
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Fixed at the shared function. `term_mentions_var` now counts an `InsertedMeta` as mentioning a slot its mask marks `Bound`, as `Nbe.closure_slots` does. Its one caller, dependent application (`Elab_apply`), used to apply a codomain closure whose body is only an inserted meta to an out-of-scope rigid, so `f : (A : Type) -> Endo(0)` gave `f(I64)` the type `#57 -> #57`. Test: binder_counts "an inserted meta in a codomain mentions its binder".
assignee:
blocked_by:
---

# `term_mentions_var` treats an inserted meta as mentioning nothing

## Defect

`InsertedMeta (id, bds)` evaluates to the meta applied to every `Bound` slot of
its environment, so it reads those slots — `Nbe.closure_slots` counts them.
`Elab_refine.term_mentions_var` answers "no" for it, so a check asking whether
a term mentions a variable says no for a term whose meta may be solved to one.
Pre-existing; noticed while centralising binder counts in `Core.map_subterms`
([core-traversals-count-binders-separately](core-traversals-count-binders-separately.md)).

## Direction

Count an inserted meta as mentioning every bound slot, as `closure_slots`
does — or, if the caller runs after metas are zonked, state that and assert it.
Instrument the callers first to learn whether an unsolved `InsertedMeta`
reaches it.
