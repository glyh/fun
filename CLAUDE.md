# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is this

`fun` is a programming language compiler/interpreter written in OCaml 5.3. It features Hindley-Milner type inference, algebraic data types, records, and Maranget-style pattern match compilation with exhaustiveness checking.

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

## Compilation pipeline

```
Source string
  → Lexer (sedlex)
  → Parser (menhir)
  → Syntax.Ast.Expr.t           (untyped AST)
  → Typecheck.Inference.on_expr (constraint-based HM inference)
  → Typed_ir.Expr.t             (every node annotated with Type.T.t)
  → Interp.Eval.eval            (tree-walking interpreter)
```

Match expressions are compiled lazily during evaluation: `Eval.eval` calls `Match_compile.compile` to build a decision tree, then interprets it against the scrutinee.

## Type system architecture

`Type.Generic` is parameterized over variable representation:
- `Type.Human.t` — source-level types with string variables, used in parser output
- `Type.T.t` — internal types with unique integer-tagged `Var.t`, used everywhere after typechecking

`Type.T.of_human` converts between the two. `Type.T.equal` handles alpha-equivalence for `Forall` types.

Type inference in `typecheck.ml` generates constraints from expressions, solves via unification, and generalizes let-bindings. Record types are tracked in `RecordDefs` with lookup by field name or type name.

## Pattern match compilation

`lib/semantic/match/` implements Maranget-style pattern matrix decomposition:
- `matrix.ml` — pattern matrix with occurrence-indexed columns
- `match_compile.ml` — finds refutable columns, specializes by constructor (`Destruct`) or literal (`Switch`)
- `decision_tree.ml` — hash-consed DAG output (deduplicates identical subtrees)
- `missing_pat.ml` — structured missing pattern diagnostics for exhaustiveness errors
- Union patterns (`|`) are expanded into multiple matrix rows

## Library dependency graph

```
std
  ← type
     ← syntax, typed_ir
        ← typecheck, match
           ← interp
```

## Language syntax notes

- Type parameters: `'a` syntax. Parameterized types use square brackets: `option['a]`, `list['a]`
- ADTs: `type option['a] = Some 'a | None`
- Records: `type point = {x: I64; y: I64}`, constructed as `{x = 1; y = 2}`, accessed as `expr.field`
- Record patterns: strict `{x; y}` or partial `{x; _}`
- Match: `match expr | pat -> body | pat -> body end`
- `let rec` desugars to `Fix(Lam(name, body))`
- Comments: `(* ... *)` with nesting
- Built-in types: `I64`, `Bool`, `Unit`, `Char`
