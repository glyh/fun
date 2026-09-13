---
title: Delete Surface.t; elaborate expanded Syntax.t
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
  - type-aware-macro-output-is-not-expanded.md
---

# Delete Surface.t; elaborate expanded Syntax.t

## Question

Remove the `Surface.t` layer and make the elaborator consume `Syntax.t`
directly.

## Why

[syntax-vs-surface-ir-layer](syntax-vs-surface-ir-layer.md) found that
`Surface.t` has the same constructors as `Syntax.t`. Lowering to it does no
desugaring; it only drops spans, scope sets, `MacroDef.kind` and
`SyntaxOperatorUse.unit`. It still carries the macro-only constructors, so it
doesn't guarantee that a tree has been expanded. What keeping it costs:

- elaborator errors have no source location;
- the port would have to carry two trees;
- one more place to update whenever a syntax field is added (`CLAUDE.md`
  checklist).

## Scope

- Delete `lib/syntax/surface.ml` and `lib/expand/lower_surface.ml`.
- Elaborator modules match on `stx.kind` and use `id.name` where they used a
  `string`. The largest are `elab_surface_rewrite.ml`,
  `elab_effect_collect.ml`, `elab_infer.ml` and `elab_match.ml`.
- Lowering call sites: `parse_expand.ml`, `macro_driver.ml` (2), `expand.ml`
  (2), plus the two sites fixed by
  [type-aware-macro-output-is-not-expanded](type-aware-macro-output-is-not-expanded.md).
- Tests that match on `Surface`: `test_macros.ml`, `test_expand_compat.ml`,
  and one case in `test_core.ml`.
- Update the `CLAUDE.md` field-propagation checklist and the pipeline diagram.

Out of scope: adding spans to `Elab_error`. This ticket only makes them
available.
