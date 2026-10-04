---
title: Three more impl-term construction sites drop Vars and Bounds
parent: ../quill-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-28
resolution: Closed 2026-09-28. The one route the probe found reachable *and* observable is fixed: both `BindingTerm.Impl` constructions in `Unify.RenameEntry` (`Unify.cs:271-272`) now carry `i.Vars, i.Bounds` — two lines. Probes: `a` and `ab` went from `missing implementation of \`Size\`` to `VALUE 3` / `VALUE 5`, `af` from failing to `VALUE 3`, and the controls `aprime` (the same route, non-generic), `a2` and `af_direct` stayed green — `aprime` is what proves the route still runs. Suite `906` → **`907` cases, 0 failed**; xUnit `206`; case `values/trait-generic-impl-through-function-application`. The other two sites (`ElaborateImplItem`, `InferExport`) were **not** touched, as the probe's verdict requires: they are reachable but their loss is masked, so a carry there is unverifiable. Two residues recorded rather than chased — this ticket's acceptance text said `ab` answers `3` when it answers `5` (its `impl Size(I64)` body returns 5 and `Some(5)` routes through it), and `a3.qll` (an explicit `Pick[Unit]`) still errors, unmeasured as to whether it is another site or an intended error.
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
| `InferExport` | reachable but **invisible** | the term drops at `Elaborator.Export.cs:37`, the entry carries at `:38` from `:74`; same masking. `trait-generic-impl-bound-through-export.qll` passes (`VALUE 5`). |

**So the work is one site and one line:** add `i.Vars, i.Bounds` to both `BindingTerm.Impl`
constructions at `Unify.cs:271-272`. Acceptance: the probe's `a.qll` and `ab.qll` answer `3`,
while `aprime` (the same route, non-generic), `a2` (top level) and `af_direct` stay green —
`aprime` is what proves the route still runs.

**Do not carry the fields at the other two sites**: nothing evaluates a raw module or struct term
and then reads its entry's variables, so a carry there is unverifiable — the mistake this ticket
exists to prevent. That is now measured rather than assumed.

**Blocked on [`Unify.cs`](struct-former-in-written-parameter-type.md)** — that file is owned by a
running fork. Queue behind it.

## Landed 2026-09-28 (`51ae973`, `65abcb2`)

Two lines at `Unify.cs:271-272`: both constructions of `BindingTerm.Impl` in `RenameEntry` now
carry `i.Vars, i.Bounds`. The parent fix made those fields **defaulted**, which is what let this
sit unnoticed — and what makes the fix this small.

Probe table, re-run by the integrator on the merged tree (`/tmp/probe/*.qll`):

| probe | before | after |
|---|---|---|
| `a` | `ELAB missing implementation of \`Size\`` | `VALUE 3` |
| `ab` | same error | `VALUE 5` (not the `3` this ticket's prose claimed) |
| `af` | same error | `VALUE 3` |
| `aprime` — the same route, non-generic | `VALUE 3` | `VALUE 3` |
| `a2` — top level | `VALUE 3` | `VALUE 3` |
| `af_direct` — opened directly | `VALUE 3` | `VALUE 3` |

Suite `906` → **`907` cases, 0 failed**; xUnit `206`; the case is
`values/trait-generic-impl-through-function-application`.

**Two residues, recorded rather than chased:**

1. The acceptance text here said `ab.qll` answers `3`. It answers `5`: that probe's `impl
   Size(I64)` body returns `5` and its `Some(5)` routes through it. The number in the prose was
   stale, not the program.
2. `a3.qll` — an explicit `Pick[Unit]` — still errors. It was not in the acceptance list, and
   whether it is a third site or an intended error is **unmeasured**. A probe comes before any
   fix, which is this ticket's own rule.
