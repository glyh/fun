---
title: An evaluation error inside a macro body carries no application site
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: The budget's per-application error became an `application` record (overrun and failure), installed by `Expand_ctx.macro_application`. Every evaluator failure goes through `Nbe_support.fail mc`, which raises that application's `Expand_error.EvalFailed` with its site, or `EvalError` outside an application. Primitive reducers return `Reduced | Stuck | Failed` instead of raising. Tests - panic and division by zero in an operator body report the use span.
closed_date: 2026-09-14
blocked_by:
---

# An evaluation error inside a macro body carries no application site

## Defect

Remainder of [expansion-errors-reach-the-user-raw](expansion-errors-reach-the-user-raw.md).
A budget overrun inside a macro application now raises the application's own
`Expand_error.BudgetExceeded` with its site, because the budget carries the
error to raise. Every other evaluation error raised while a macro body runs —
`EvalError` from `panic`, a missing field, division by zero — still carries no
operator use/declaration span.

## Direction

The same mechanism: the evaluation request the application opens already
knows its site; `Nbe`'s raise sites read the current application from the
budget (`MetaContext.t`) and attach it, rather than a catch-and-rewrap around
the application. Wait for the concurrent `nbe.ml` work (closure rule /
recursion performance) to land.

## Found by

The expansion-errors change (2026-09-14).

## Resolution

- `Eval_budget.application = { exceeded; failed }` replaces the single
  `exceeded` field; `macro_application` installs it for the application's
  extent. `Expand_error.EvalFailed { macro; message }` is the new case.
- `Nbe_support.fail mc message` is the one raise path. `dot_value`,
  `unhandled_effect_error`, `try_prim_reduce` and `stuck_head_frames` now take
  `mc`; `Nbe_quote`'s "cannot quote" errors use it too. The unused `get_cont`
  is deleted.
- `Nbe_prim.Prim.reducer` returns `Reduced | Stuck | Failed`, so division by
  zero is reported by the evaluator with the request's error.
- **Not covered:** `eval_con` / `eval_nominal` / `eval_eff` take no `mc`; their
  "unbound" errors are internal invariants (the elaborator resolved the name)
  and those functions were being edited concurrently. A missing field is
  rejected by the checker, so "field not found" is not reachable from source
  inside a macro body; it goes through `fail` like the rest.
- A failure while *checking* a macro's definition (e.g. `_ = 1 / 0` in its
  body, which the checker evaluates) is not an application error and stays
  `EvalError` — confirmed by instrumentation.
