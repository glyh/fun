---
title: A match whose scrutinee is a closure crashes the evaluator
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Root cause - the match dispatch listed the matchable shapes and sent everything else to the stuck path, which accepts only neutral, flex and rigid values. It now lists the stuck shapes instead; every other value (a closure, a function type, a universe) gets a decision tree whose unknown occurrences have the Unknown domain, so a variable or wildcard binds it. The dead value-returning duplicate eval_match and its helpers are deleted.
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
