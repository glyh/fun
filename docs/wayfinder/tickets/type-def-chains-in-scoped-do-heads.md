---
title: Mutual type chains in scoped do-heads (TypeDef)
parent: ../fun-design-map.md
status: open
assignee:
blocked_by:
  - mutually-recursive-nominal-types.md
---

# Mutual type chains in scoped do-heads

## Question

Let `and` chains appear in scoped type heads — `do type A = … and B = …; body`
(`TypeDef`), where the group binds over a body expression rather than over
following bindings. Currently the parser's `parse_type_binding` is shared between
module items, struct items, and scoped heads (`scoped_binding_to_expr`, enforest.ml
~1313); the scoped-head site accepts exactly one type and a chain there errors.

## Context

- Deferred during the grilling of
  [mutually-recursive-nominal-types.md](mutually-recursive-nominal-types.md)
  (decision: chains in module and struct binding positions first; scoped heads
  reject chains with a targeted error until this ticket).
- Cost of full support: `TypeDefGroup`-shaped variants in both Syntax and Surface,
  and updates at ~11 sites across 9 files — the 1:1 lowering maps,
  `enforest_template.ml` captures, both `expand.ml` walkers (scope algebra for N
  names × params × ctors threaded into payloads and body), two
  `elab_effect_collect.ml` analyses, `elab_surface_rewrite.ml`, and the
  expression-position knot in `elab_infer.ml:801` (group knot inside `infer`,
  body elaborated under N defined names).
- No known use case yet; Reflect-Match needs module-level `Expr`/`Branch`, which
  does not require this.

## Resolution

_Unresolved._
