---
title: The elaborator still matches `EffectRow` and `stx_` names by spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# The elaborator still matches `EffectRow` and `stx_` names by spelling

## Defect

M12 ("no name is found by its spelling alone",
[macro model](../topics/core-tt-domain-model-macros.md)) is enforced for bare
names and path heads, but `Elab_infer.infer`'s `Var` case still dispatches on
the written name:

- `Var { name = "EffectRow"; _ }` → `EffectRowTy`, before any lookup. A local
  binder spelled `EffectRow` cannot be referenced.
- A name with prefix `stx_` is evaluated and, if it is a primitive neutral,
  replaced by `Prim pname`. The meaning of a binder depends on its spelling.

(Resolved names are fresh, so the `stx_` test now reads the *resolved* name,
which only works because the prefix survives freshening.)

## Direction

`EffectRow` becomes a prelude binding whose value is `EffectRowTy`, located
like any name. The `stx_` rewrite should be decided by the entry (a primitive
declaration, see [unify-primitive-declaration](unify-primitive-declaration.md)),
not the prefix — or deleted if `Var ix` evaluating to the primitive is enough.

## Found by

The path-heads implementation (2026-09-14).
