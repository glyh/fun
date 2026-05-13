# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is this

`fun` is a programming language compiler/interpreter written in OCaml 5.3. It features Hindley-Milner type inference, a struct-based module system (inspired by Zig), Maranget-style pattern match compilation with exhaustiveness checking, and file imports.

## Build commands

```sh
dune build              # build
dune test               # run all tests
dune exec fun           # launch REPL
```

Run a single test suite:
```sh
dune exec lib/backend/interp/test/test_interp.exe
dune exec lib/syntax/test/test_syntax.exe
dune exec lib/semantic/type/test/test_type.exe
dune exec lib/semantic/type/typecheck/test/test_typecheck.exe
```

Run a single test case (Alcotest syntax):
```sh
dune exec lib/backend/interp/test/test_interp.exe -- test 'matches' -e 'literal switch'
```

Core type theory tests (separate prototype):
```sh
dune exec lib/semantic/core/test/test_elaborate.exe
dune exec lib/semantic/core/test/test_core.exe
dune exec lib/semantic/core/test/test_debug.exe
```

## Compilation pipeline

```
Source string
  → Lexer (sedlex)
  → Parser (menhir)
  → Syntax.Ast.Expr.t              (untyped AST, may contain TypeDecl)
  → Syntax.Desugar.expr
  → Syntax.Desugared_ast.Expr.t    (TypeDecl eliminated — only Value/Open/Export bindings)
  → Typecheck.Inference.on_expr    (constraint-based HM inference)
  → Typed_ir.Expr.t                (every node annotated with Type.T.t)
  → Interp.Eval.eval               (tree-walking interpreter)
```

The desugar pass rewrites `type T = ...` into `let T = struct ... end` + `open T`. Callers must call `Desugar.expr` before passing to the typechecker. Match expressions are compiled lazily during evaluation: `Eval.eval` calls `Match_compile.compile` to build a decision tree, then interprets it against the scrutinee.

## Type system architecture

`Type.Generic` is parameterized over variable representation:
- `Type.Human.t` — source-level types with string variables, used in parser output and AST
- `Type.T.t` — internal types with unique integer-tagged `Var.t`, used after typechecking

`Type.T.of_human` converts between the two. `Type.T.equal` handles alpha-equivalence for `Forall` types.

Type inference in `typecheck.ml` generates constraints from `Desugared_ast.Expr.t`, solves via unification, and generalizes let-bindings. Record types are tracked in `RecordDefs` with lookup by field name or type name (used for field access inference).

`self` is encoded as a sentinel `Con({path=[]; name="self"}, [])` in `Human.t`. The typechecker's `resolve_self ~nominal` substitutes it with the actual nominal type before processing.

## Module system

`struct ... end` is the universal construct for records, ADTs, modules, and namespaces. A struct body is one of three forms:
- `Fields` — record type (`x: I64; y: I64;`)
- `Variants` — ADT (`| Some 'a | None`)
- `Namespace` — pure namespace (only `let`/`type`/`open` members)

`type T = ...` desugars into `let T = struct ... end` + `open T` (or `export T` for pub types inside structs). The `Desugared_ast` has no `TypeDecl` — only `Value | Open | Export` bindings.

`open Name` brings struct members into scope privately. `export Name` does the same but re-exports them publicly. Inside structs, `open`/`export` control whether opened names leak to the parent's public interface.

## Key IR types

- `Typed_ir.Binding.t` = `Value | Open | Export` (no TypeDecl)
- `Typed_ir.Expr.StructDef` carries `body : Ast.struct_body`, `members : Binding.t list`, `pub_names : string list`
- `Value.t` uses `Struct of (string * t) list` for both records and structs (unified)

## Pattern match compilation

`lib/semantic/match/` implements Maranget-style pattern matrix decomposition:
- `matrix.ml` — pattern matrix with occurrence-indexed columns
- `match_compile.ml` — finds refutable columns, specializes by constructor (`Destruct`) or literal (`Switch`)
- `decision_tree.ml` — hash-consed DAG output (deduplicates identical subtrees)
- `missing_pat.ml` — structured missing pattern diagnostics for exhaustiveness errors
- Union patterns (`|`) are expanded into multiple matrix rows

## Dependent type theory prototype (`core_tt`)

`lib/semantic/core/` is a standalone prototype for the planned DT migration. It has its own surface syntax (`surface.ml`), core terms (`core.ml`), NbE evaluator (`nbe.ml`), elaborator with implicit argument insertion (`elaborate.ml`), and higher-order unification with metavariables (`unify.ml`). It depends on `std` and `syntax` (for `Ast.Atom.t`) but is not wired into the main compilation pipeline yet.

Key differences from the HM pipeline:
- Uses de Bruijn indices/levels instead of named `Var.t`
- Type : Type (no universe hierarchy yet)
- Implicit arguments (`{x : A} -> B`) with insertion via `InsertedMeta`
- Nominal types with constructors (`VNominal`, `VCon`) and pattern matching
- Structs with `open` scoping and partial application

The test suite has its own parser (`core_parser.mly` / `core_lexer.ml`) for writing test expressions in a concise syntax.

## Library dependency graph

```
std
  ← type
     ← syntax, typed_ir
        ← typecheck, match
           ← interp, loader

std, syntax
  ← core_tt (standalone prototype, not connected to main pipeline)
```

## Language syntax notes

- Type parameters: `'a` syntax. Parameterized types use square brackets: `option['a]`, `list['a]`
- ADTs: `type option['a] = Some 'a | None` (desugars to struct internally)
- Records: `type point = {x: I64; y: I64}`, constructed as `point {x = 1; y = 2}`, accessed as `expr.field`
- Record patterns: strict `point {x; y}` or partial `point {x; _}` (type name required)
- Structs: `let M = struct ... end`, parameterized `struct['a] ... end`
- `self` keyword in type positions refers to the enclosing type (recursive types)
- `open M in expr` / `open M` inside structs; `export M` inside structs for re-export
- Nullary constructors in patterns require parens: `Red()` is a constructor, `Red` is a variable binding
- Match: `match expr | pat -> body | pat -> body end`
- `let rec` desugars to `Fix(Lam(name, body))`
- Comments: `(* ... *)` with nesting
- Built-in types: `I64`, `Bool`, `Unit`, `Char`
- File imports: `import "path"` resolves to `./path.fun`, cached and circular-import-safe
