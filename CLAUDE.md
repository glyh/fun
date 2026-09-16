# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

`fun` is a programming language compiler/interpreter in OCaml. The core (`core_tt`) is dependently typed with bidirectional elaboration, NbE, nominal ADTs, structural records/modules, traits, algebraic effects, and a hygienic enforestation-based macro system.

Design philosophy: **Consistency > Flexibility > Correctness** — one construct for many roles (`struct` = record/module/namespace), types are values, type-case on open `Type` is acceptable.

`(wrapped false)` everywhere — all `.ml` are flat top-level modules, no `.mli` files. Library names are `core_tt_*` (see dependency graph below), distinct from directory names.

### Build & test

```sh
dune build                     # build
dune test                      # all tests
dune exec fun                  # REPL (bin/main.ml)
dune exec test/backend/test_core.exe -- test macros -e 'name'  # single test
```

Test executables: `test/backend/test_core.exe`, `test/backend/test_macro_driver_stage7.exe`, `test/semantic/test_elaborate.exe`, `test/syntax/test_syntax.exe` (a single Alcotest binary aggregating all `test/syntax/test_*.ml` suites, including `test_line_counts`).

### Documentation hierarchy

- `docs/STATUS.md` — **authoritative** snapshot of what is built; when any doc disagrees on completion status, STATUS wins.
- `docs/wayfinder/` — direction map (decided / open tickets / fog); start at `docs/wayfinder/fun-design-map.md`. Macro-system reference lives in `docs/wayfinder/macro-system/`.

### Pipeline

```
source → Raw_syntax → Enforest Syntax.t → Expand → expanded Syntax.t → Elaborate → Core.term → NBE → value
```

### Source layout

- `lib/core_kernel/` — `Atom`, `Core`, `Debug`, `Syntax`, `Compiler_names` (centralized compiler-known names), `Scope_set`
- `lib/syntax/` — `Raw_syntax` reader
- `lib/expand/` — `Enforest` (+ `enforest_forms`/`_pat`/`_template`/`_decl_helpers`), `Expand`, `Parse_expand`, `Macro_eval`, `Expand_ctx`
- `lib/semantic/typecheck/` — `Elaborate` split across many `elab_*` modules (`elab_infer`, `elab_check`, `elab_patterns`, `elab_prelude`, `elab_driver`, …), `Unify`, `Macro_driver` (type-aware macro interleaving)
- `lib/semantic/match/` — `Core_match_compile`, `Core_decision_tree`
- `lib/backend/interp/` — `Nbe`
- `lib/loader/` — `Core_loader`
- `test/backend/`, `test/semantic/`, `test/syntax/` — tests by pipeline stage
- `test/conformance/` — the shared conformance suite: `.fun` programs plus
  `.expect` results that the .NET port must pass too (`cases/README.md`)

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

A test that is only "source string → value or error" belongs in
`test/conformance/cases/` instead (two data files, no registration), so the .NET
port runs it too — **and only there**: the Alcotest copies of those cases were
deleted, so language behaviour has one source of truth. Keep a test in Alcotest
when it inspects internals: syntax shapes, reflection round trips, budget
accounting, macro nominals plumbing, an exact error constructor
(`expect_elab_error`), or a type rather than a value (`check_type`).

---

## Adding a new Syntax ADT (e.g. Pattern, TypeExpr)

When adding a new nominal type for macros to inspect/construct:

1. **Prelude**: in `Elab_prelude.stage1_source`'s `Syntax` module (stage 1 has no
   `type` macro - that is defined in stage 2), declare
   `pub rec Foo = enum { Ctor1(Tys), Ctor2(Tys) }; export Foo;` - add `open Foo;` only
   where the stage-1 source uses the constructors bare, else qualify them (`Foo.Ctor1`)
2. **Builders**: add `pub foo_build = fn(args...) { Ctor(args...) }` in the prelude
3. **syntax_nominals**: add a `foo : value` field in `Macro_eval.syntax_nominals`
4. **wrap/unwrap**: add `wrap_stx_foo` / `unwrap_stx_foo` in `macro_eval.ml`
5. **Nominals construction**: update ALL sites that build `syntax_nominals`
   (`test/backend/test_core.ml`, `eval_with_macros`, `eval_decl_module`, etc.)
6. **Pattern synonyms**: if adding `pub pattern` for the new type, the elaborator
   hardcodes `Expr` as the scrutinee (`elab_infer.ml`). Must also add the
   resolve-by-constructor-name fallback.

## Reflection and scope-addition: preserve ALL fields

When adding a field to `Syntax.struct_binding` variants (e.g. `kind` to `MacroBinding`),
you MUST update EVERY constructor of that variant across the codebase:

- `macro_eval.ml` — reflection must carry the field both ways (the round trip is the identity)
- `expand.ml` `go_kind` / `go_struct_binding` — the one traversal (`mapper`) every
  scope, intro, rename and syntax-form fill goes through; `map_binders` for the
  names a declaration binds
- `enforest_template.ml` — a syntax form's rules (patterns, matching into
  `Syntax.capture`)

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

The elaborator resolves pattern heads via `find_nominal_for_pattern_head_opt(ctx, path)`:
the path's head resolves like a bare name (binder or open choice), and the nominal is
read off the entry it lands on (never by scanning or by spelling). When adding a new ADT inside a
module, its constructors become visible only AFTER the ADT binding is processed.

- **Pattern synonyms** for ADT constructors work only if the ADT was elaborated
  in a PREVIOUS binding group
- The `Expr` pattern synonyms work because they pre-date all user code
- New ADTs added to the prelude need their pattern synonyms after them

## Expander vs Elaborator context

- **Expander** (`Expand_ctx.t`): processes macros during parsing. Requires
  `elaborate` and `eval_and_apply` callbacks.
- **Elaborator** (`Elaborate.Ctx.t`): elaborates expanded `Syntax.t` → `Core.term`.
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

## Style conventions

- **Exceptions are not control flow.** `raise`/`try-with` must never be used for
  normal program logic. They signal unrecoverable errors — malformed input, I/O
  failures, internal invariants violated. A well-formed program should not
  trigger exception-based dispatch. Use `Result`, `option`, or explicit
  sum types for recoverable or expected failure paths.
- **Line-count reduction is structural, not cosmetic.** When faced with a hard
  LoC limit, do not minify, reindent, or join lines. Instead:
  1. Identify genuinely repetitive code (e.g. near-duplicate `first_some` blocks,
     body-parsing patterns copied across `parse_fn_parts`/`parse_method_binding`/
     `parse_operator_value`).
  2. Lift the common structure into a shared helper or combinator.
  3. Or split the module at a clean interface boundary (e.g. expression parser
     vs binding parsers into separate files accessible via a small driver).
  The goal is fewer lines through less duplication, not fewer lines through
  less readability.
  **The 3000-line limit in `test_line_counts.ml` is strict. Never bump it.**
  If a file exceeds it, split or extract — do not raise the cap.
- **Debug via instrumentation, not test-case exploration.** When tracking down
  a parser or elaboration bug, do not repeatedly modify test cases to exhaust
  the input space. Instead:
  1. Add logging (`debug_tokens`, `Printf.eprintf`, etc.) to surface internal
     state at the point of failure.
  2. Or add reusable test utilities that expose intermediate representations
     (e.g. `show_token_kind`, `desc_token`, `Parse_spec.parse` with
     traceable combinators).
  3. `dune build 2>&1 && dune exec test.exe 2>/tmp/log` captures both stdout
     and stderr for inspection without scrolling through test output.
  The goal is a single diagnostic that pins the root cause, not a matrix of
  modified test inputs.
- **No test-driven special cases.** Do not introduce special-case logic whose
  sole purpose is making a test pass. Fix the code so that it genuinely and
  uniformly handles the input, conforming to the semantics of the language
  or compiler. When the intended semantics are ambiguous or under-specified,
  ask the user before committing to an interpretation.
- **Git checkout is a last resort.** Use `git checkout` only sparingly. If you
  must jump to a historical commit or branch, first record where you are
  (e.g. note the current branch/commit in the todo list or save a stash) so
  you do not lose track of the starting point and lose progress.
