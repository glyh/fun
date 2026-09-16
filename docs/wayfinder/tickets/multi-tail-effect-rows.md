---
title: Effect rows with several row variables (union of tails)
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-16
resolution: Implemented (branch row-unions). Core rows carry `tails : term list` (`tail_values : value list`), written `->{Log | e1, e2}`; `normalize_effect_row_value` splices a tail solved to a row, so a union disappears as its variables are solved; unification cancels the tails both sides name and solves a single remaining one, and a union of two unsolved tails against a concrete row is `EffectRowMismatch` rather than a guess. `UnsupportedRowUnion` is deleted.
decided: 2026-09-16
assignee:
blocked_by:
---

# Effect rows with several row variables (union of tails)

## Why

`f : (A ~> B) -> (C ~> D) ~> E` must mean `[e1, e2] -> (A ->{e1} B) -> (C ->{e2} D)
->{e1, e2} E` (effect-arrow-syntax: parameters mint, results collect). Today a row
is a set plus at most one tail (E2), so this is `UnsupportedRowUnion` (pinned by a
test).

## Decision (grilled 2026-09-16)

**A row is a set of known effects plus a set of row variables**: `{Log | e1, e2}`.
E2 changes accordingly. Unification solves union constraints ("`e1 ∪ e2 ⊇ {Log}`")
— set-based effect inference as in Effekt; decide the solving strategy when
implementing (e.g. constraint collection with a simplification step, or
restricting variables in positive/negative positions so union constraints only
arise at results). Written syntax for several tails: `->{Log | e1, e2}`.

Tests: the two-callback `f` with pure/Log/Exc callbacks in each slot; `->{e1, e2}`
written by hand; tunneling and discharge unchanged; E2 doc updated.

## Resolution (2026-09-16)

Strategy: **normalise, then cancel.** A tail solved to a row is spliced into its
row, so the common case - a call whose callbacks' rows are solved when its
arguments are checked - needs no constraint solving at all. Unification removes
the tails both sides name, then solves one remaining tail to whatever is left.
Only a union of two *unsolved* tails meeting a concrete row has no principal
solution; that is an error rather than a guess, and no test needed it.
