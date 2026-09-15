---
title: Role visibility gaps left by M7
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Role visibility gaps left by M7

Found by the M7 implementation (2026-09-14), recorded in the closed
[M7 ticket](template-heads-resolve-by-scope-set.md) "Known gaps". M9 run 2
did not close them.

1. **Import opens check only unit-wide roles.** `open (import "x")` has no
   written scope set to test visibility against, so `roles_in_open` notes only
   imported (unit-wide) roles against it, not roles the unit itself declared
   before the open.
2. **Driver-run opens are unchecked.** `OpenSuppliesRole` is raised where the
   elaborator holds the macro runtime (`roles_in_open` capability,
   `elab_ctx.ml`); `Macro_driver` installs that runtime only after a unit's
   bindings are elaborated, so opens elaborated during the unit's own driver run
   are never checked.
3. **Imported roles have no scope set.** A role imported inside a block stays in
   the block because the enforester copies its table (`with_operator_scope`),
   not by scope set; imported roles are visible unit-wide. An imported
   template's replacement must still see them.

## Direction

Give an import's roles the scope set of the site that imports them (the open's
or import binder's region), so all three reduce to ordinary scope-set
resolution and `with_operator_scope` can go. Write a failing test for each
first.

## Implemented (2026-09-15)

- **An imported role is bound in its import's region.** `Expand.import_roles`
  binds a unit's exported roles through `Expand_ctx.bind` - the funnel every
  binder takes, so role mixing and open noting apply - at the region of the
  `open` (its scope) or of the binder `M = import …` (its name's scope). A
  block's import no longer reaches past the block. An `import` still loads its
  unit's syntax and macros where it is written (cycles are reported there);
  a bare import not opened or bound binds no roles.
- **`Syntax.Import` carries the scope set of its `import` keyword**, reflected
  both ways (`RawImport(Option(Span), String, Scopes)`) and moved by the one
  traversal. `open (import "x")` is checked against the roles visible where it
  is written, the unit's own included.
- **The macro driver installs the macro runtime before the unit's bindings
  elaborate**, so opens it elaborates are checked.
- The table copy the ticket named (`with_operator_scope`) was already gone with
  M9 run 2; the remaining `Binding.copy` uses read quoted syntax and struct
  items, not imports.
