---
title: The closed-term rule does not look inside closures
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
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
