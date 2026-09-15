---
title: A match whose scrutinee is a closure crashes the evaluator
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A match whose scrutinee is a closure crashes the evaluator

## Defect

```fun
{ h = match (fn(u : Unit) { 1 }) { x => x }; h(()) }
// error: EvalError("if condition is not a boolean or stuck term")
```

The same happens with an effect branch present (how the audit found it). Probed
on main (35baa3c).

`Nbe` evaluates a match by dispatching on the scrutinee's value: constructors,
atoms, types, products and records get a decision tree; anything else is
assumed stuck and goes through `stuck_head_frames` (`nbe.ml:530-539`), which
accepts only neutrals, flex and rigid values. A closure (`VLam`) is neither
matchable nor stuck, so it reaches the catch-all `fail` — whose message is also
wrong (it is not an `if`).

## Direction

A scrutinee whose patterns need no inspection (variable, wildcard) matches any
value: evaluate the decision tree with an `Unknown` domain for such values
instead of treating them as stuck. Only a value that is genuinely neutral builds
a stuck match. Fix the message of the remaining invariant failure to name the
match.

## Found by

The effects domain-model audit (2026-09-15).
