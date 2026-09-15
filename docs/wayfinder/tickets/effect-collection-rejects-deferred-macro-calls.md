---
title: Effect collection rejects a deferred typed macro call
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
assignee:
blocked_by:
---

# Effect collection rejects a deferred typed macro call

## Defect

A macro whose signature promises a type (`: Expr(T)`) is not expanded by the
expander: its call stays a `Syntax.MacroCall` for the elaborator to run
(`Elab_resolve.apply_typed_macro`). That node is legitimate elaborator input. But
the effect-collection pass treats every `MacroCall` as "macro-only syntax that
should not reach elaboration" and aborts:

- `elab_effect_collect.ml:52-53` (`compile_time_safe`)
- `elab_effect_collect.ml:266` (`collect_effects`)

It is hit when a deferred call sits somewhere effect collection walks before the
elaborator reaches it, e.g. inside a lambda body that a macro's output adds:

```fun
macro inner(x : Expr(I64)) : Expr(I64) { x };
macro under(x : Expr(I64)) : Expr(I64) { quote((fn(z : I64) { $x })(0)) };
under(inner(z))
// error: macro-only syntax should not reach elaboration
```

Found by the typed-args-once run (2026-09-15); may predate it.

## Not the issue

Syntax *values* at run time are fine (`quote(…)`, the `Syntax` ADTs, macros as
ordinary functions over them). The failure is about an unexpanded macro *call*
in code being type checked, and the pass also uses `failwith` as a dispatch for
a reachable case.

## Direction

A deferred `MacroCall` is an expression whose effects are those of the output
the macro produces. Either run the macro before collecting (collect over the
elaborated output), or treat its effects as unknown / an open row until it
runs. `compile_time_safe` should answer `false` (not pure until expanded)
rather than abort. The remaining truly unreachable forms should be an internal
invariant error, not a `failwith` on a reachable path.

## Resolution (2026-09-15)

The elaborator records each deferred typed call's output by the call's node
(`Elab_resolve.deferred_outputs`, identity-keyed: equal calls may run at
different expected types); `collect_effects` and `compile_time_safe` read the
output. Every effect read now follows the elaboration it reads: a type is
required pure after it elaborates and before it is evaluated
(`Elab_type_expr.require_pure`, replacing the collect-first lines in
`Elab_infer`), and the check-mode match scrutinee and impl fields collect after
they elaborate. A missing output is an internal invariant failure. Chosen over
"unknown effects": running the call is elaboration, so its effects are known
exactly once it ran, and ordering the reads guarantees it has.
Tests: `under(inner(z))`, a typed call in a type annotation.
