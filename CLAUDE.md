# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is this

`fun` is a programming language compiler/interpreter written in OCaml 5.3. The implementation is centered on the `core_tt` dependent type core: a surface parser, bidirectional elaborator, normalization-by-evaluation runtime, nominal ADTs, structural records/modules, pattern matching, and file imports.

## Build commands

```sh
dune build                     # build
dune test                      # run all tests
dune exec fun                  # launch REPL
dune build @fmt --auto-promote # format OCaml/dune files with ocamlformat
dune build @doc                # build odoc documentation
```

There is no dedicated lint target; use `dune build`, `dune test`, and `dune build @fmt --auto-promote` as the standard validation loop.

Run a single test suite:

```sh
dune exec lib/semantic/core/test/test_core.exe
dune exec lib/semantic/core/test/test_elaborate.exe
```

Run a single test case with Alcotest's test filter syntax when needed:

```sh
dune exec lib/semantic/core/test/test_core.exe -- test eval -e 'factorial'
```

## Compilation pipeline

```
Source string
  → Core_lexer / Core_parser
  → Surface.t
  → Elaborate.on_expr
  → Core.term + Core.value type
  → NBE evaluation / Elaborate.Ctx.eval
  → Debug.pp_value_short
```

`bin/main.ml` wires the REPL. It parses each input expression, creates a `Core_loader` rooted at `Sys.getcwd ()`, elaborates through `Elaborate.on_expr ~loader`, evaluates with `Elaborate.Ctx.eval`, and prints `value: type`. Exceptions are caught per input so the REPL stays alive after parse, elaboration, or runtime errors.

## Important entrypoints

- `bin/main.ml` — executable REPL wiring for parse/elaborate/eval/print.
- `lib/semantic/core/core_lexer.ml` and `lib/semantic/core/core_parser.mly` — sedlex/menhir parser for expressions and `.fun` module files.
- `lib/semantic/core/surface.ml` — surface AST consumed by elaboration.
- `lib/semantic/core/elaborate.ml` — bidirectional elaborator, context management, implicit insertion, pattern elaboration, and match exhaustiveness checks.
- `lib/semantic/core/nbe.ml` — normalization-by-evaluation, primitive evaluation, quoting, conversion, and runtime match evaluation.
- `lib/semantic/core/unify.ml` — higher-order metavariable unification.
- `lib/semantic/core/core_match_compile.ml` and `lib/semantic/core/core_decision_tree.ml` — decision-tree match compilation.
- `lib/semantic/core/core_loader.ml` — `.fun` import resolution, caching, and circular import detection.
- `lib/semantic/core/debug.ml` — compact pretty-printers for REPL/test output.

## Type system architecture

`core_tt` uses de Bruijn indices in core terms and de Bruijn levels in semantic values. Names exist at the surface/elaboration boundary for lookup and diagnostics, but the core representation is index-based.

The current universe model is `Type : Type`. This is intentionally simple and can be replaced with a universe hierarchy later.

Elaboration is bidirectional:

- `infer` synthesizes a core term and semantic type.
- `check` verifies a surface expression against an expected semantic type.
- implicit arguments are represented with explicitness flags on Pi/application forms and inserted with metavariables during elaboration.
- let-bound unsolved metas are generalized into implicit parameters, giving HM-like let-polymorphism through the dependent core.

Metavariable solving uses Miller-style pattern unification in `unify.ml`. `MetaContext` tracks solved and unsolved metas, and NbE `force` follows solved metas during normalization/conversion.

## Structs, records, modules, and ADTs

`struct ... end` is the universal structural value for records, modules, and namespaces. A struct can contain:

- field declarations: `x: I64;`
- computed public fields: `pub let x = expr`
- private bindings: `let x = expr`
- nested public/private type bindings
- `open` / `export` members for scoped imports and re-exports
- methods with `self` and `Self` support for record-like structs

Record construction uses a struct type value:

```fun
let Point = struct x: I64; y: I64; end in
Point {x = 1; y = 2}
```

Record patterns destructure by field name:

```fun
match p with
| Point {x; y} -> x + y
end
```

ADTs are nominal and are introduced with `type`:

```fun
type Option A = Some A | None in
match Some 1 with
| Some x -> x
| None -> 0
end
```

Nominal type equality is by generated nominal id plus parameter equality, not by name.

## Pattern matching

Pattern matching is elaborated into core patterns and compiled through `core_match_compile.ml` into decision trees. Supported patterns include:

- wildcards and binders
- literal atoms (`I64`, `Bool`, `Char`, `Unit`)
- tuples/products
- nominal constructors, including qualified paths
- records, including renamed and partial field patterns
- or-patterns
- type-head patterns used by type-specialized primitives

The match compiler performs exhaustiveness checks for finite domains where possible and treats open domains, such as `Type`, conservatively.

## Imports

`import "path"` resolves to `<cwd>/path.fun`, using the process working directory as the import base. `Core_loader` parses imported files through the module parser entrypoint, caches parsed modules, and reports circular imports.

A `.fun` module file is parsed as a struct-like module body. Public members are exposed to import users through ordinary struct/module field access and `open`.

## Library dependency graph

```
core_tt
  ├─ menhirLib
  └─ sedlex

fun executable
  ├─ linenoise
  └─ core_tt
```

Tests depend on `core_tt` and `alcotest`.

## Language syntax notes

- Built-in types: `I64`, `Bool`, `Unit`, `Char`, `Type`.
- Type parameters are ordinary binders in dependent function types; implicit arguments use braces.
- Explicit function type: `(x : A) -> B`; implicit function type: `{x : A} -> B`.
- Explicit application: `f x`; explicit implicit application: `f {A}`.
- ADTs: `type Option A = Some A | None in ...`.
- Records: `let Point = struct x: I64; y: I64; end in Point {x = 1; y = 2}`.
- Field access: `expr.field`.
- Record patterns: `Point {x; y}`, `Point {x = n; _}`, and qualified forms such as `M.Point {x}`.
- Structs: `struct ... end` with `let`, `pub let`, `type`, `pub type`, `open`, and `export` members.
- `self` names the receiver inside methods; `Self` names the enclosing record type after field declarations.
- `open M in expr` brings public members into expression scope.
- Match: `match expr with | pat -> body | pat -> body end`.
- Recursive values use `let rec`.
- Comments: `(* ... *)` with nesting.
