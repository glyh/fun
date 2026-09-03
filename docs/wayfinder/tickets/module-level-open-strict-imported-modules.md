---
title: Module-level open form (strict imported modules)
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
closed_date: 2026-09-01
resolution: Implemented — `open <module-expr>` is now a module/struct item (`Syntax`/`Surface.OpenBinding`, `Core.OpenBind`), scoping over the subsequent bindings only and exporting nothing. `Enforest.parse_module` lost `?open_prelude` entirely and the loader/`Macro_driver` blanket harvest is gone, so modules are strict about prelude *syntax* like expressions. 809 tests green. One loose end spun out: imported modules still elaborate in the importer's context, so prelude *values* still leak in — see imported-module-elaboration-context.md.
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

**Implemented.** `open <module-expr>` is now an item of a module or struct body,
alongside the expression-level `Open` it mirrors.

### What was built

1. **AST**: `Syntax.OpenBinding of t` / `Surface.OpenBinding of t` as a
   `struct_binding` variant, threaded through `lower_surface`,
   `surface_to_syntax`, `expand` (`add_scope` and `expand_struct_binding` — an
   open binds no name of its own, so it contributes an empty scope list),
   `enforest_template`, and `elab_surface_rewrite`.
2. **Core + runtime**: `Core.OpenBind of term`. The expression form `Open` and
   the new `OpenBind` share one `Nbe.push_opened_values` helper, which pushes
   exactly one env entry per public field and per public impl — the lockstep
   partner of the elaborator's `Elab_resolve.open_module_value`. Without that
   correspondence every de Bruijn index in the bindings after the open would be
   off by the number of opened fields.
3. **Elaboration**: an `OpenBinding` case in both `Elab_infer.elab_module_binding`
   (used by `Module` and by `Macro_driver`'s per-binding advancement hook) and
   the `Struct` binding fold. Each infers the module expression, requires a
   `VModule`, advances the context with `open_module_value`, and emits
   `OpenBind` with **no** module entry — so an open never re-exports.
4. **Parsing**: `parse_open_binding` is in the `first_some` list of both
   `parse_module_binding` and `parse_struct_binding`; `pub open` is rejected.
   The operator harvest needs no new machinery — parsing the module expression
   runs `Enforest_forms.parse_import`, which harvests through `load_syntax`, and
   statements are parsed in source order, so the operators scope over exactly
   the items that follow the open.
5. **Modules made strict**: `Enforest.parse_module`'s `?open_prelude` parameter
   was **deleted** (not merely unused) along with the blanket `~open_prelude:true`
   in `Core_loader` (3 sites) and `Macro_driver.visit_macros`. `parse_expr` keeps
   the flag: a bare expression has nowhere to write the open. Test fixtures that
   use prelude syntax in a module now write `open (import "std")` themselves —
   no harness sugar was added.

### Answers to the open questions

- **The prelude source stays special.** `stdlib_source` uses no infix operators,
  and `std` must not open itself; `import "std"` remains a reserved path
  resolved by `load_syntax`/`Elab_infer`. Nothing about module-level `open`
  changes that.
- **Struct scope boundary** (discovered while building, not in the sketch): in a
  `struct … end` an open scopes over later *bindings* but **not** over the
  record `con_fields`, because field types are elaborated as a group before the
  binding fold. Documented on `Surface.OpenBinding`.

### Known limitation left open

Strictness is now enforced on the **syntax** side only. An imported module is
still elaborated with `ops.infer ctx imported` in the *importer's* context
(`elab_infer.ml`, `Import path`), so prelude values and constructors reach it
whether or not it opens `std` — a module with `pub y = Some(1)` and no open
still elaborates. Split out as
[imported modules elaborate in the importer's context](imported-module-elaboration-context.md).

### Tests

17 new cases (792 → 809 green):

- `test_enforest`: module-level open shape, open-of-import shape, struct-level
  open shape, `pub open` rejected, module without an open has no prelude
  operators, open scopes later statements only.
- `test_elaborate` (new `module-level open` suite): module opens the prelude for
  itself; module without the open is strict; open scopes over later bindings
  only; open exposes an imported module's values; open does not re-export; open
  exposes constructors; open of a non-module is rejected.
- `test_core` (new `module-level open` suite): opened values usable in later
  bindings (the de Bruijn stress case — a binding *before* the open and names
  *after* it in one arithmetic expression), opened constructors usable later,
  inline `module open B` , struct-level open.
