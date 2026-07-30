---
title: Retire load_imports_in_terms and the open_stdlib survivor
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Retire load_imports_in_terms and the open_stdlib survivor

## Question

Finish the deletions that
[Explicit prelude open for operator demotion](explicit-prelude-open-operator-demotion.md)
named in steps 2 and 3 but only *bypassed*. Two mechanisms are no longer on the
operator-delivery path yet still live in the tree:

1. **`load_imports_in_terms`** (`enforest_util.ml`) — the static, whole-body
   upfront scan for `import "…"` that eagerly harvests a file's `pub syntax`.
   Operator delivery now goes through `Enforest_forms.parse_import`'s in-order
   `load_syntax` harvest instead, so this scan is redundant for the operator path.
   It survives only at `enforest.ml:1118`, inside `parse_syntax_template_decl`, to
   pull imports referenced from a `syntax`-template body.
2. **`open_stdlib`** (`elab_entry.ml`, re-exported by `elaborate.ml`) — the
   ctx-extension that opens the prelude module into an elaboration context. The
   *user-code* implicit open is gone (`on_expr` no longer wraps), but `open_stdlib`
   is still used by `Macro_driver` (`macro_driver.ml:43`) to give the
   macro-compilation context the prelude, and conceptually overlaps with
   `Elaborate.on_macro_body` (which opens `std` via the general
   `Open (Import "std", …)` construct at the surface level).

## Why this is cleanup, not a bug

Both are *live* code paths, not dead code — deleting them naively breaks
`syntax`-template imports and macro compilation respectively. This ticket is about
reaching one uniform mechanism, so a future reader (and the C# rewrite) sees a
single story:

- **`load_imports_in_terms`**: can the template-body case at `enforest.ml:1118`
  route its imports through the same in-order `parse_import` harvest as everything
  else, letting the standalone recursive scan be deleted? Investigate what a
  `syntax`-template body actually needs (it parses a template, which may reference
  imported operators) and whether the ordinary expression/statement parse already
  covers it.
- **`open_stdlib` vs `on_macro_body`**: unify. `Macro_driver`'s
  `open_stdlib elab_ctx0` (ctx extension) and `on_macro_body` (surface-level
  `Open (Import "std", body)`) are two ways to give compiler-facing code the
  prelude. Decide on one — most likely have `Macro_driver` build its
  macro-compilation context the same way `on_macro_body` does (or vice versa), then
  delete the other, so `open_stdlib` disappears.

## Acceptance

- `load_imports_in_terms` deleted, or reduced to a single documented use with a
  note on why it can't route through `parse_import`.
- `open_stdlib` deleted (folded into `on_macro_body` / a single
  macro-compilation-context builder), or explicitly kept with a one-line rationale
  that supersedes the parent ticket's "delete implicit open_stdlib" line.
- No behavior change; the full suite stays green.

## Resolution

_Unresolved (cleanup; low risk, no user-visible change)._
