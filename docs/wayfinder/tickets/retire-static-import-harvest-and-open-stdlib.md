---
title: Retire load_imports_in_terms and the open_stdlib survivor
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
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

**Resolved — both mechanisms investigated; both are load-bearing and kept with
documented rationale (the acceptance criteria's "single documented use" /
"explicitly kept with a one-line rationale" branches). No behavior change; the
full suite (792 tests) stays green.**

### `load_imports_in_terms` — reduced to one documented use

Empirically confirmed it cannot route through the in-order
`Enforest_forms.parse_import` harvest. Removing the sole call at
`enforest.ml` `parse_syntax_template_decl` breaks three cycle-detection tests
(`syntax extension circular`, `7I: generated syntax cycle`,
`7I: generated macro cycle`), because a syntax-template body's imports must be
harvested *before* the branches are enforested — both so the replacement can use
imported operators at positions the reader reaches ahead of the `import`
statement, and so circular syntax imports unwind on the `load_syntax` visit stack
at declaration time. Restored the call with a full rationale comment at the site,
and documented the definition (`enforest_util.ml`) as the single sanctioned use.

### `open_stdlib` — kept as the ctx-builder counterpart of `on_macro_body`

Both `open_stdlib` (ctx extension) and `on_macro_body` (surface-level
`Open (import "std", body)`) bottom out in the same `open_module_value` on `std`
— they are not two divergent mechanisms but two shapes of one. `Macro_driver`
needs the *persistent* ctx-builder shape because its per-binding advancement hook
elaborates module bindings via `elab_module_binding` directly (no expression to
wrap in `Open`), and its shared `elaborate` callback would double-open (shifting
de Bruijn indices) if it used `on_macro_body`. So `open_stdlib` stays, now with a
doc comment that explains the relationship and explicitly supersedes the parent
ticket's "delete implicit open_stdlib" line (the *user-code* implicit open is
already gone via `on_expr`). Small real cleanup landed alongside: `open_stdlib`
returned `(ix, ctx)` but its only caller discarded `ix`, so it now returns just
`ctx`.
