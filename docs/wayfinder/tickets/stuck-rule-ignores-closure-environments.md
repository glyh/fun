---
title: The closed-term rule does not look inside closures
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee:
resolution: `Nbe.closed` now inspects a closure through the environment slots its body reads (`Nbe.closure_slots`, binder-aware across lambdas, lets, pattern branches, binding lists and nominal and effect definitions; an inserted meta reads every bound slot). A body that extends its environment by an amount only evaluation reveals (an `open`, an `OpenBind`) is conservatively not closed, so the call stays stuck. Lambda, fixpoint, pi codomain and effect-row closures and stuck `match` frames all go through it. Regression tests in the `evaluation_budget` group; the check costs nothing measurable (a 40k-step type-level recursion over a closure checks in the same 0.11 s as before the budget).
closed_date: 2026-09-14
blocked_by:
---

# The closed-term rule does not look inside closures

## Defect

"Only closed terms evaluate" ([checker-evaluation-budget](checker-evaluation-budget.md)).
`Nbe.closed` decides whether a fixpoint may unfold at check time, and it treats
every closure as closed without inspecting its captured environment. A call
passing a closure that captures an unknown variable therefore still unfolds,
where the decision promises it stays stuck. It is bounded by the budget, so it
cannot hang — but it spends budget and can report a budget error on a term the
model says costs nothing.

Marked `ponytail:` at `lib/backend/interp/nbe.ml` (`closed`).

## Direction

Inspect captured environments (only the slots the body mentions, or all of
them). Write the failing case first: a `rec` over a function argument whose
closure mentions a lambda-bound variable, used in a type.

## Found by

The checker-budget implementation (2026-09-14).
