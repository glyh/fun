---
title: A budget error names a core term, not the call or the conversion
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A budget error names a core term, not the call or the conversion

## Defect

[checker-evaluation-budget](checker-evaluation-budget.md) decided the error
names the call and, because the checker evaluates implicitly, which conversion
demanded the evaluation (its footgun 1). Today `EvaluationBudgetExceeded`
carries the limit and the first 120 characters of the callee's core term:
core lambdas have no names and the error has no span.

## Where

- `lib/core_kernel/eval_budget.ml` (`Exceeded`), raised in `Nbe`.
- `Elab_entry.on_expr` / `on_expr_effects` translate it with no knowledge of
  the conversion or source position in progress.

## Direction

The call's name is the binder the fixpoint came from (a `rec` binding's
resolved name), and the demanding conversion is the elaborator frame that
called `conv`/`force`. Both need the span attachment the
"elaborator errors carry no source location" fog item describes; do them
together.

## Found by

The checker-budget implementation (2026-09-14).
