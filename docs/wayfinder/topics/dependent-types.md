# Dependent core: bidirectional checking and NbE

`core_tt` is the implementation path for the language. It combines a surface AST, bidirectional elaboration, higher-order metavariable unification, normalization by evaluation, nominal ADTs, structural records/modules, and decision-tree pattern matching.

## Source files

| File | Purpose |
|---|---|
| `lib/core_kernel/atom.ml` | primitive atom values and pretty-printing |
| `lib/syntax/surface.ml` | surface AST consumed by elaboration |
| `lib/core_kernel/core.ml` | core terms, semantic values, neutrals, closures, metas, nominal ids |
| `lib/backend/interp/nbe.ml` | evaluation, application, forcing, quoting, conversion, primitive reduction |
| `lib/semantic/typecheck/unify.ml` | pattern unification and metavariable solving |
| `lib/semantic/typecheck/elaborate.ml` | bidirectional checking, implicit insertion, generalization, pattern elaboration |
| `lib/semantic/match/core_decision_tree.ml` | decision-tree representation for matches |
| `lib/semantic/match/core_match_compile.ml` | pattern matrix compilation and exhaustiveness checking |
| `lib/loader/core_loader.ml` | module file import resolution and caching |
| `lib/expand/enforest.ml` | expression and module parser/enforester |

Tests live in `test/backend/test_core.ml`, `test/semantic/test_elaborate.ml`, and `test/syntax/`.

## Core representation

Terms use de Bruijn indices. Semantic values use de Bruijn levels. Names are surface/elaboration conveniences for lookup and user-facing output.

The current universe model is intentionally simple:

```fun
Type : Type
```

This is useful for experimentation and can be replaced with a proper universe hierarchy later.

## Bidirectional elaboration

Elaboration has two main modes:

- `infer` synthesizes a core term and semantic type.
- `check` verifies a surface term against an expected semantic type.

Application inference inserts implicit arguments when it sees leading implicit Pi domains. Check mode also inserts implicits before unifying with an expected type. Let-bound unsolved metas are generalized into implicit parameters, recovering HM-like let-polymorphism through ordinary dependent function types.

## Metavariables and unification

Metavariables are stored in `MetaContext`. A flex value carries a meta id and spine. Solving follows the Miller pattern fragment: the solver builds a renaming from spine variables, performs an occurs check, abstracts over the spine, and installs the solution.

Conversion and unification force solved metas before comparing values.

## NbE

`nbe.ml` evaluates core terms into semantic values and quotes semantic values back to core terms. It handles:

- function application and eta-like comparison of functions
- primitive operations
- structs, records, field access, and tuple projection
- nominal constructors
- stuck neutrals and elimination frames
- match evaluation and quoted match frames

## Nominal ADTs

`type` introduces nominal ids. Name equality is not enough: two separate declarations with the same name are distinct. Parameterized ADTs compare by nominal id plus parameter equality.

Constructors are ordinary values with Pi-typed constructor functions. Type arguments are implicit for constructors, so code can write `Some(1)` and let elaboration infer the element type.

## Structural records and modules

Structs are structural values. Field declarations define record fields; public value bindings define public computed members; private bindings are internal. Record instances are constructed with `StructName{field = value}` and checked against the struct's field declarations.

## Pattern matching

Patterns are elaborated against the scrutinee type and then compiled into decision trees. Supported cases include constructor patterns, literals, tuples, records, or-patterns, binders, wildcards, qualified paths, and type-head patterns.

Open domains such as `Type` remain conservative for exhaustiveness; finite nominal domains can be checked precisely.

## Imports

`import "path"` resolves `<cwd>/path.fun`, parses it as a module file, caches the parsed module, and detects circular imports. Imported module files expose public members through ordinary struct semantics.

## Verification

```sh
dune build
dune test
dune exec test/backend/test_core.exe
dune exec test/semantic/test_elaborate.exe
dune exec test/syntax/test_syntax.exe
```
