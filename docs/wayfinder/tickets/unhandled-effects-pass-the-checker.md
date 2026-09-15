---
title: Unhandled effects pass the checker
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented on one-pass effects. A program's entry (an entry expression, an imported unit's top-level bindings) is checked against the row the runtime handles, empty today; an effect left there is UnhandledEffects naming it. Effects are computed during inference; the second walk is deleted.
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

## Implemented (2026-09-15, branch `one-pass-effects`)

- **One pass.** `infer`/`check` record what a form performs in a sink the
  context carries (`Ctx.sink`, `Elab_effects.emit` / `collecting`): `perform` and
  a function's latent row at an application emit (the argument is evaluated for
  the row only when the row mentions it). A lambda body, a type, a handled
  scrutinee and an imported unit elaborate in a fresh sink. The `collect_effects`
  walk, `compile_time_safe` and `deferred_outputs` are deleted: the walk was a
  second elaborator (block-local types unbound, handled effects in branch bodies
  counted, typed macro arguments elaborated again).
- **Handlers are deep in the checker too:** a match's residual is its
  scrutinee's and its branch bodies' effects less those it handles (the
  continuation's row stays the scrutinee's residual).
- **Entry.** `Elab_entry.on_expr` and loading a unit check the residual against
  `Elab_effects.runtime_handled_effects` (empty) — the seam a runtime handler
  fills.
- **Lets.** A let, module or struct member whose value performs is not evaluated
  at check time: the items after it see it opaque.
- The escaping-closure example is now caught statically, as a top-level
  unhandled effect where it is called; the scope check itself (E6) stays with
  [handlers-tunnel-callback-effects](handlers-tunnel-callback-effects.md).
- Two tests changed their annotation to `can {}`: they relied on a check-mode
  match requiring an empty residual, which a bare arrow's open row (E3 not built)
  does not.
