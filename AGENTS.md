# AGENTS.md — project overview, recurring issues, conventions

## Project overview

`fun` is a programming language compiler/interpreter in OCaml. `(wrapped false)` everywhere — all `.ml` are flat top-level modules, no `.mli` files.

### Build & test

```sh
dune build                     # build
dune test                      # all tests
dune exec bin/main.exe         # REPL
dune exec test/backend/test_core.exe -- test macros -e 'name'  # single test
```

### Pipeline

```
source → Raw_syntax → Enforest Syntax.t → Expand + Lower → Surface.t → Elaborate → Core.term → NBE → value
```

### Source layout

- `lib/core_kernel/` — `Atom`, `Core`, `Debug`, `Syntax`
- `lib/syntax/` — `Surface.t`, `Raw_syntax` reader
- `lib/expand/` — `Enforest`, `Expand`, `Parse_expand`, `Macro_eval`, `Expand_ctx`
- `lib/semantic/typecheck/` — `Elaborate`, `Elab_infer`, `Unify`, `Elab_patterns`, `Elab_prelude`
- `lib/semantic/match/` — `Core_match_compile`, `Core_decision_tree`
- `lib/backend/interp/` — `Nbe`
- `lib/loader/` — `Core_loader`
- `test/backend/`, `test/semantic/`, `test/syntax/` — tests by pipeline stage

### Library dependency graph

```
core_tt_kernel → core_tt_syntax → core_tt_expand → core_tt_loader → core_tt_typecheck
                                  core_tt_interp ──────────────────────┘
                                  core_tt_match ───────────────────────┘
```

### Testing helpers

- `eval_with_macros` — expression-level macro tests
- `eval_decl_module` — module-level macro tests (uses `parse_module` with callbacks)
- `eval_with_imported_macros` — cross-module tests (writes temp `.fun` files)
- `check_i64_macro` — asserts macro result equals an i64

---

## Adding a new Syntax ADT (e.g. Pattern, TypeExpr)

When adding a new nominal type for macros to inspect/construct:

1. **Prelude**: define `pub type Foo = Ctor1(Tys) | Ctor2(Tys)` in `elab_prelude.ml`
2. **Builders**: add `pub foo_build = fn(args...) -> Ctor(args...)` in the prelude
3. **syntax_nominals**: add a `foo : value` field in `Macro_eval.syntax_nominals`
4. **wrap/unwrap**: add `wrap_stx_foo` / `unwrap_stx_foo` in `macro_eval.ml`
5. **Nominals construction**: update ALL sites that build `syntax_nominals`
   (`test/backend/test_core.ml`, `eval_with_macros`, `eval_decl_module`, etc.)
6. **Pattern synonyms**: if adding `pub pattern` for the new type, the elaborator
   hardcodes `Expr` as the scrutinee (`elab_infer.ml`). Must also add the
   resolve-by-constructor-name fallback.

## Lowering and scope-addition: preserve ALL fields

When adding a field to `Syntax.struct_binding` variants (e.g. `kind` to `MacroBinding`),
you MUST update EVERY constructor of that variant across the codebase:

- `lower_surface.ml` — `Syntax.Foo { ...; new_field } → Surface.Foo { ...; new_field }`
- `surface_to_syntax.ml` — the reverse
- `expand.ml` `add_scope` functions — check each `struct_binding` case
  (look for `kind = None` hardcodes — there were 3 of them)
- `enforest_template.ml` — template helpers reconstruct bindings
- `elab_surface_rewrite.ml` — surface-level rewrites

**Pattern**: `git grep` for the variant name (e.g. `MacroBinding {`) and check
every match site preserves the new field or explicitly drops it.

## Parser: `rest = []` is almost always wrong

In `enforest.ml`, when a function consumes tokens and returns remaining tokens
(`rest`), never hardcode `rest = []`. Return the unconsumed tokens so the caller
can check `ensure_no_rest` or continue parsing. Classic bugs:

- `parse_fn_parts` arrow case was `(body, [], span)` — dropped rest silently
- Any `parse_all(...)` call that discards the rest tuple

**Fix**: replace `parse_all (fun ts -> f ts) terms` with `f terms` and destructure
the `(result, rest)` tuple.

## Elaborator: constructor resolution phases

The elaborator resolves constructors via `find_nominal_template_opt(ctx, path, name)`.
This scans nominals in the elaboration context. When adding a new ADT inside a
module, its constructors become visible only AFTER the ADT binding is processed.

- **Pattern synonyms** for ADT constructors work only if the ADT was elaborated
  in a PREVIOUS binding group
- The `Expr` pattern synonyms work because they pre-date all user code
- New ADTs added to the prelude need their pattern synonyms after them

## Expander vs Elaborator context

- **Expander** (`Expand_ctx.t`): processes macros during parsing. Requires
  `elaborate` and `eval_and_apply` callbacks.
- **Elaborator** (`Elaborate.Ctx.t`): processes core terms in `Surface.t` → `Core.term`.
- Macros are compiled by the EXPANDER using the `elaborate` callback.
- Imported module macros are pre-compiled by `visit_macros` and cached in
  `macro_cache`. The `load_macros` callback pre-registers them in the expander.
- `MacroCallBinding` expansion needs `eval_and_apply` threaded through
  `Core_loader.load_elaborated → parse_runtime_module → Parse_expand.parse_module`.

## Common OCaml patterns in this codebase

- Libraries use `(wrapped false)` — all `.ml` files are flat top-level modules
- No `.mli` files — all modules export everything
- `Syntax.t` is the surface AST node, NOT an OCaml `t` type alias
- `Core.term` is the elaboration core term, `Core.value` is the evaluated value
- `and` in type definitions links mutually recursive types across files
