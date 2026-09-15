---
title: Dotted paths and compiler-known names are found by first match on a spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Dotted paths and compiler-known names are found by first match on a spelling

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariants

- **I3**: a dotted path denotes the *last* member of that name.
- **M12**: no name is found by its spelling alone; compiler-known names live in
  `Compiler_names`.

## Where the code deviates

- `Elab_stdlib.resolve` (`lib/semantic/typecheck/elab_stdlib.ml:6-19`) walks
  fields with `List.find_opt`: first match. `nbe_support.ml` and
  `elab_resolve.ml` use `find_field_last`.
- Named impls (`M.eq_C`): `Core.module_impl_type_opt` / `module_impl_value_opt`
  (`lib/core_kernel/core.ml:589-601`) use `List.find_map`: first match. Callers:
  `elab_infer.ml:467`, `nbe_support.ml:38`.
- `Elab_stdlib.syntax_nominals` (`elab_stdlib.ml:25-60`) resolves literal member
  strings (`"Expr"`, `"Decl"`, `"Pattern"`, `"TokenTree"`, …) and bare
  `["Option"]`, `["List"]`; only the `Syntax` module name comes from
  `Compiler_names`.

## Direction

Follow every path with the last-member lookup. Move the prelude nominal names
into `Compiler_names`, or read them off the prelude's entries by identity.
