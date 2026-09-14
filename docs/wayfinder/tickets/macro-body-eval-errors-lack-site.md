---
title: An evaluation error inside a macro body carries no application site
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
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
