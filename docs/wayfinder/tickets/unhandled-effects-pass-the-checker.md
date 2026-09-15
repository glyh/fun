---
title: Unhandled effects pass the checker
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Unhandled effects pass the checker

## Defect

A program whose effects are not handled type-checks and fails only at run time.
`require_empty_effects` (`elab_effects.ml:31`) runs only where a row is written
(`can {}`), at a pure position, or in a type; nothing checks that a unit's or a
top-level expression's residual row is empty.

Probed on main (35baa3c):

```fun
{ effect Exc = sig { raise : I64 -> I64 }; perform Exc.raise(1) }
// run time: unhandled effect Exc.raise

{ effect Exc = sig { raise : I64 -> I64 };
  g = match (0) { x => fn(u : Unit) { perform Exc.raise(x) }, effect Exc.raise n => fn(u : Unit) { n } };
  g(()) }
// run time: unhandled effect Exc.raise  (a closure escaped its handler, E6)
```

## Direction

A program (a unit's top-level bindings and a REPL/entry expression) is checked
against an empty row: its residual effects are an elaboration error naming the
effect. The second case is E6 (an effectful closure may not escape its handler's
scope) and belongs to
[handlers-tunnel-callback-effects](handlers-tunnel-callback-effects.md); this
ticket is the top-level check that would at least report it statically.

## Found by

The effects domain-model audit (2026-09-15).

## Grilled (2026-09-15): an error, runtime effects via a real handler

An effect left unhandled at the top of a program (a unit's top-level bindings, the
REPL / entry expression) is an **elaboration error** naming the effect. No
warning mode.

Effects the runtime provides later (printing, the E10 heap effects) are allowed
at the top because the program entry is elaborated inside a runtime-provided
handler for them — the same mechanism a user handler uses, not an exemption list.
