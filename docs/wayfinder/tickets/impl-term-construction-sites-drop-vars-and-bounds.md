---
title: Three more impl-term construction sites drop Vars and Bounds
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# Three more impl-term construction sites drop `Vars` and `Bounds`

Found 2026-09-28 by the fork that fixed
[Nbe's module re-evaluation drops Vars and Bounds](nbe-module-reevaluation-drops-vars-and-bounds.md).
That fix carried the two fields through the readback and rebuild sites the ticket named; it also
found three more sites that build or rewrite a `BindingTerm.Impl` without them, and left them
alone because they sit outside the file set its brief allowed:

- `Unify.RenameEntry` — reconstructs an impl term without `Vars`/`Bounds`, the same shape
  `QuoteEntry` had.
- `ElaborateImplItem` and `InferExport` — construct `BindingTerm.Impl` without them, so a
  struct-value entry built from a term slot still loses them.

**Why they were invisible:** the new fields are *defaulted*, which is what made the fix six lines
and no other construction site break — and is also why these three still compile while dropping
the data. Anything that rebuilds an entry through them loses an impl's own variables and bounds,
and the symptom at a use site is a plain `missing implementation of …`, with no hint that the
entry's route caused it. That is exactly the shape the parent ticket spent a day on.

**Not a live regression:** the full suite passes with them untouched (`904 cases, 0 failed`), so
nothing exercises them today.

## How to work it

Write the probe that shows a loss at a use site *first* — for one of the three, build the entry
through it, then use the impl and watch it fail — and only then carry the fields the way the
parent fix did (`Slots()` → `Slot` → the rebuild; `QuoteEntry` → the term). A carry with no
failing probe is unverifiable, which is this family's recorded lesson: the parent ticket's fix
was applied, tested and reverted before it was understood.
