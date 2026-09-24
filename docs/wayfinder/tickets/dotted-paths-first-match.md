---
title: Dotted paths and compiler-known names are found by first match on a spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Every member lookup takes the last match through one helper, `Core.find_map_last` (`find_field_last` and the named-impl lookups use it; `Elab_stdlib.resolve` reads fields with `find_field_last`; the unused `struct_impl_type_opt` is deleted). The prelude nominal names `Elab_stdlib.syntax_nominals` and the quote-hole types read are in `Compiler_names` (`Syntax_name`, `Type_name.option`/`list`). Test: a module with two named impls `eq_I` resolves `M.eq_I` to the last in both the type and value views. **Superseded in part (2026-09-21):** a container's public members are unique in the C# port, so a container holds one member per name and there is nothing to choose between; last-match still governs two `open`s supplying one name and a private binding sharing a label with a public one (public-members-are-unique.md). The cited test's duplicate `eq_I` impls are now `eq_bool` / `eq_i64` in `values/elab-021`, since the port refuses the duplicates.
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
