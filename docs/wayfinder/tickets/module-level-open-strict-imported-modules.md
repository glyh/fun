---
title: Module-level open form (strict imported modules)
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Module-level open form (strict imported modules)

## Question

Add an `open <module-expr>` form usable at **module top level** (a `.fun` file /
`module … end` body), not only inside `do` blocks / expression bodies. This closes
the one gap left by
[Explicit prelude open for operator demotion](explicit-prelude-open-operator-demotion.md):
under the strict phase rule, an *expression* sees prelude operators only where
`std` is opened, but an imported **module** is still auto-opened by the loader
(`Parse_expand.parse_module … ~open_prelude:true`) because there is nowhere to
write the open. Once module-level `open` exists, imported `.fun` files can — and
should — `open (import "std")` themselves, and the loader's blanket
`~open_prelude:true` can be dropped, making modules strict like expressions.

## Context — what exists today

- `Syntax.Open` / `Surface.Open` already carry an arbitrary module *expression*
  (done in the parent ticket), and the elaborator's `Open` case opens any
  `VModule`. So the **elaboration** side already supports module-level open; the
  gap is purely **parsing**: `Syntax.Open` is only ever constructed in the do-body
  statement path (`enforest.ml` `parse_do_body_terms`, ~line 1373). Module-level
  statements go through `parse_module_binding` / `parse_module_statement`, which
  have no `open` case.
- Because of that gap, `Enforest.parse_module ~open_prelude:true` (used by the
  loader for every imported module, and by `Macro_driver.visit_macros`) *harvests*
  `std`'s operators into the parse env but does **not** wrap the module in an
  `Open` — a module has no single expression to wrap, and there is no module-level
  open statement to carry it. So imported modules are non-strict: they get prelude
  operators for free.
- The strict phase rule for expressions is enforced by
  `Enforest.parse_expr ?open_prelude` + `parse_import`'s in-order `load_syntax`
  harvest (see the parent's progress note part 4).

## Sketch of the work

1. **Parse** a module-level `open <expr>` statement in `parse_module_statement` /
   `parse_module_binding`, producing a module binding that carries the opened
   module and scopes it over the *subsequent* bindings (statement-order
   visibility, matching the expression-level rule). This likely means a new
   `struct_binding` variant (or reusing an `Open`-shaped binding) that the
   elaborator threads as an `Open` over the rest of the module's bindings.
2. **Elaborate**: an opened module at position *i* must bring its
   values/operators into scope for bindings *i+1…n* only. Operators are a parse
   concern (harvest at the open, in statement order); values are the existing
   `Open` elaboration over the remaining binding group.
3. **Make modules strict**: drop the loader's blanket `~open_prelude:true`
   (`core_loader.ml`, `macro_driver.ml`), and have the prelude / test modules that
   use operators write `open (import "std")` at their top. Audit the test-module
   fixtures (`with_modules`, `eval_with_imported_macros`) for operator usage and
   add the open where needed.

## Open questions

- Should the **prelude source** itself (`elab_prelude.stdlib_source`) gain a
  leading `open (import "std")` once module-level open exists, or stay special?
  (It currently uses no infix operators in its body, so it parses fine either
  way — but a genuine module-level open would let it drop any remaining
  special-casing.)
- Interaction with the deferred **module-as-`.fun`-file** question (parent's
  risk note): a real shipped prelude file would `open (import "std")`… but `std`
  *is* the prelude, so the prelude must not open itself. Keep `std` a reserved
  builtin for now.

## Resolution

_Unresolved (feature gap; design not yet grilled)._
