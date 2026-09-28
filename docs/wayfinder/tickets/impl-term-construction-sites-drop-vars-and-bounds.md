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

## The probe, run 2026-09-28 by a read-only fork (its report is the evidence)

One program per route, with controls. Verdicts:

| route | verdict | evidence |
|---|---|---|
| `Unify.RenameEntry` | **reachable and observable** | a generic impl inside a module returned through a lambda-plus-binder spine, `open`ed and then used, fails `ELAB missing implementation of \`Size\``. `Unify.cs:271-272` builds `BindingTerm.Impl` without the fields; reached via `:118` → `:191` → `:255`. Controls rule out the lambda/quote machinery: the same spine with a *non-generic* impl works, the same module at top level works, and opening it directly works. |
| `ElaborateImplItem` | reachable but **invisible** | the term drops at `Elaborator.Traits.cs:284` while the entry carries at `:292`; masked because an application's result is re-evaluated from the quoted closure type (`Elaborator.cs:601`, `:620`). |
| `InferExport` | reachable but **invisible** | the term drops at `Elaborator.Export.cs:37`, the entry carries at `:38` from `:74`; same masking. `trait-generic-impl-bound-through-export.fun` passes (`VALUE 5`). |

**So the work is one site and one line:** add `i.Vars, i.Bounds` to both `BindingTerm.Impl`
constructions at `Unify.cs:271-272`. Acceptance: the probe's `a.fun` and `ab.fun` answer `3`,
while `aprime` (the same route, non-generic), `a2` (top level) and `af_direct` stay green —
`aprime` is what proves the route still runs.

**Do not carry the fields at the other two sites**: nothing evaluates a raw module or struct term
and then reads its entry's variables, so a carry there is unverifiable — the mistake this ticket
exists to prevent. That is now measured rather than assumed.

**Blocked on [`Unify.cs`](struct-former-in-written-parameter-type.md)** — that file is owned by a
running fork. Queue behind it.
