---
title: "Port: recursive value definitions"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: recursive value definitions

Wave 1 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `rec f = fn(n) { … f(…) … }` in blocks and module items, and recursive groups
  `rec f = … and g = …`: `Fix` terms and fixpoint values, the checker's
  evaluation budget, and lazy unfolding of pure calls (`VGlued`) as the prototype
  decides it.
- `fn name(params) { … }` declarations that recurse.
- Recursive **type** definitions (`rec T = enum`, recursive records) are **out of
  scope**: stop at them.

## Decided rules to read first

- [recursive definitions stuck on open arguments](recursive-definitions-stuck-on-open-arguments.md),
  [checker evaluation budget](checker-evaluation-budget.md),
  [deep non-tail recursion is superlinear](deep-non-tail-recursion-is-superlinear.md)
  (why the machine exists: recursion depth is bounded by memory, never by the
  CLR stack, and a fixpoint call must go through `Kont` frames).

## Target

Failing cases that use `rec` on a value and need nothing else unported; many
also need operators or `if` from the prelude, so expect few to turn green here.
Pin the machine property with an xUnit test: a deep non-tail recursive fixpoint
does not overflow.

## Other forks

structs, match-enums, implicits, imports run concurrently.
