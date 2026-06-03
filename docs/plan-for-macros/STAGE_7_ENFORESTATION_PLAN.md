# Stage 7 Enforestation Plan

## Goal

Stage 7 introduces enforestation as the parsing boundary for the redesigned `fun` surface syntax described in `SYNTAX_SPEC.md`.

This stage should not try to implement every redesigned syntax form at once. The primary goal is to establish the architecture that future syntax extension can build on:

- raw token/tree reading before committed AST parsing;
- syntax classes for expression, type, pattern, declaration/module item, and block/body forms;
- operator tables for prefix, infix, postfix, and delimiter-driven forms;
- a deterministic enforest function that consumes one form at a time;
- a parser pipeline where new syntax enters through enforestation instead of Menhir grammar growth.

## Design References

- `SYNTAX_SPEC.md` is the target surface syntax reference.
- `IMPLEMENTATION_PLAN.md` lines for Stage 7 define the stage-level exit criteria.
- `papers/regular-syntax/honu-enforestation.pdf` is the model for consuming one form from a term stream with operator precedence and syntax extension.
- `SUMMARY.md` describes why enforestation is needed for regular syntax rather than s-expression-only macros.

## Non-Goals For Stage 7

- Do not rewrite elaboration around a new AST.
- Do not implement type-aware, problem-aware, stuck, or type-providing macros.
- Do not add arbitrary user-defined grammar productions yet.
- Do not commit to a full parser redesign in a single patch.

Stage 7 should be implemented as a sequence of small compatibility-preserving slices.

## Pipeline Target

The initial Stage 7 pipeline should be:

```text
source text
  -> raw reader/token trees
  -> enforest one form at a time using built-in syntax/operator tables
  -> Syntax.t
  -> existing hygienic Expand.expand
  -> Lower_surface.lower_expr
  -> Surface.t
  -> existing elaborator/backend
```

The old Menhir compatibility path has been removed. `Parse_expand.parse_expr` and `Parse_expand.parse_module` now route through enforestation directly.

## Phase 7A: Raw Reader And Minimal Expression Enforesting

Status: Implemented.

### Purpose

Create the minimum useful enforestation path without changing existing language behavior.

### Scope

Implement a raw reader that recognizes syntax-spec reader terms:

- integers, strings, chars, identifiers, and operator identifiers;
- delimiters `()`, `[]`, `{}`;
- punctuation `,`, `;`, `.`, `:`, `=`, `|`, `->`, `<-`, `@`;
- newlines as separators;
- line comments `# ...`;
- nested block comments `#| ... |#`;
- datum comments `#_ term`.

Implement an expression enforester that supports only:

- literals;
- identifiers as variables;
- parenthesized expressions;
- tuple expressions `(a, b, c)`;
- curried explicit call syntax `f(a, b, c)` as `f(a)(b)(c)`;
- zero-argument call syntax `f()` as `f(())`;
- curried implicit call syntax `f[A, B]` as `f {A} {B}` internally;
- field access `expr.name`;
- record construction as a postfix brace form, e.g. `Point{x = 1}`;
- record construction after type-function instantiation, e.g. `Pair[I64, Bool]{fst = 1; snd = true}`;
- prefix `not expr`;
- binary arithmetic/comparison operators from the current prelude;
- assignment `<-` lowering to `RefSet` where supported;
- `do ... end` blocks with simple bindings and a final expression.

The first `do`-block slice should accept:

```fun
do
  x = 1
  y = x + 1
  y
end
```

and lower it to nested `Let` nodes ending in the final expression.

### Files

- `lib/syntax/raw_syntax.ml`
- `lib/expand/syntax_class.ml`
- `lib/expand/operator_env.ml`
- `lib/expand/enforest.ml`
- `lib/expand/parse_expand.ml`
- `lib/expand/dune`
- `test/syntax/test_enforest.ml`
- `test/syntax/test_syntax.ml`

### Tests

Add syntax tests for:

- raw grouping for `()`, `[]`, and `{}`;
- `#` and `#| ... |#` comments;
- operator precedence: `1 + 2 * 3`;
- left associativity: `1 - 2 - 3`;
- field access: `m.x.y`;
- curried call: `f(1, 2, 3)`;
- zero-argument call: `f()`;
- implicit call: `id[I64](1)`;
- simple `do` block with two bindings and final expression;
- module bodies where newlines separate declarations;
- unsupported new syntax falls back or fails deterministically.

### Exit Criteria

- Existing syntax tests still pass.
- Existing parser-entrypoint users still parse through `Parse_expand`.
- The supported redesigned expression slice parses through `lib/expand/enforest.ml`.
- `dune exec test/syntax/test_syntax.exe` passes.
- `dune build` passes.

## Phase 7B: Built-In Prefix Forms

Status: Implemented.

### Purpose

Move redesigned built-in expression forms out of Menhir grammar growth and into enforestation entries.

### Scope

Add built-in first-token dispatch for:

- [x] `fn` anonymous functions with `()` explicit params and `[]` implicit params;
- [x] `if cond do ... else ... end`;
- [x] `match scrut do ... end` for the simplest existing value-branch patterns;
- [x] `ref(expr)` and `deref(expr)` syntax;
- [x] `resume expr` and `resume ()`;
- [x] `import "path"`;
- [x] `open M` inside `do` blocks.

`fn` initially supports:

```fun
fn(x : I64) -> x
fn(x : I64) do x end
fn[A : Type](x : A) -> x
fn[A : Type] -> struct value: A; end
fn() -> 1
```

Anonymous functions with only implicit parameters do not require an empty explicit `()` parameter list. `fn()` lowers to a single explicit `Unit` parameter and `f()` lowers to application to unit. References use `deref(r)` for reads; postfix `.deref` is ordinary field access and is not ref syntax.

### Files

- `lib/expand/enforest.ml`
- `lib/expand/operator_env.ml`
- `test/syntax/test_enforest.ml`
- targeted semantic/backend tests only if the new forms should elaborate immediately.

### Tests

- [x] `fn` arrow body and block body;
- [x] implicit params must precede explicit params;
- [x] `if ... do ... else ... end` lowers to `If`;
- [x] simple `match ... do ... end` value branches lower to `Match`;
- [x] `deref(ref(1))` lowers to `RefGet (RefNew ...)` shape;
- [x] `resume ()` lowers to `Resume Unit`;
- [x] `import "x"` shape remains `Import "x"`;
- [x] `open M` works as a statement inside `do` blocks.

### Exit Criteria

- [x] No new final syntax is added to Menhir for these forms.
- [x] Existing old syntax remains compatible.
- [x] The redesigned examples parse to expected `Surface.t` shapes.

## Phase 7C: Declarations And Modules

Status: Implemented for the initial declaration/module parsing slice. Full Honu-style two-pass binding discovery is still deferred until user-visible syntax-extension bindings need it.

### Purpose

Introduce declaration/module-item syntax classes so top-level and block parsing can discover bindings before parsing all nested bodies.

### Scope

Support redesigned declaration forms:

- [x] `x = expr`;
- [x] `x : Type = expr`;
- [x] `rec fn f(params) ...` in expression `do` blocks;
- [x] `fn f(params) ...` sugar;
- [x] `fn f(params) -> expr` named arrow-body sugar without a trailing `end`;
- [x] `fn f(params) do ... end` named block-body sugar;
- [x] `fn f[implicit-params] do ... end` named implicit-only block-body sugar;
- [x] `pub method name(params) -> expr` and `pub method name(params) do ... end` inside structs lower to method bindings with `self`/`Self` in scope;
- [x] `pub` modifiers at top-level/module/struct positions;
- [x] `macro name = expr` as the current macro-binding syntax in redesigned modules;
- [x] `module name? do ... end`;
- [x] `struct do ... end` with field declarations and bindings.

The important architectural point is two-pass scope handling:

- pass 1 detects binding names and registers lexical scopes;
- pass 2 enforests nested bodies using the completed scope for that block.

This mirrors the Honu parse1/parse2 split and prevents order-sensitive macro behavior inside blocks.

The implemented 7C slice parses declarations into `Syntax.t` and still leaves authoritative binding-scope introduction to `Expand.expand`, matching the Stage 7 hygiene boundary. A full pre-enforestation parse1/parse2-style binding discovery pass remains a follow-up before expanding beyond the initial Stage 7E syntax-extension slice.

### Files

- `lib/expand/syntax_class.ml`
- `lib/expand/enforest.ml`
- `lib/expand/expand.ml` if scope introduction needs small adjustments;
- `test/syntax/test_enforest.ml`;
- `test/backend/test_core.ml` for simple evaluation examples.

### Tests

- [x] top-level `pub x = 1` in `parse_module`;
- [x] `do x = 1; x end`;
- [x] `rec fn fact(n : I64) ...` after the expression subset can express the body;
- [x] struct methods using `method`, including zero-extra-argument methods such as `pub method get() do self.value end`;
- [x] module member visibility matches existing `Module`/`Struct` lowering;
- [x] macro declarations are not exposed as runtime fields.

### Exit Criteria

- [x] `parse_module` can parse a small redesigned module body through enforestation.
- [x] Binding scopes are still introduced by `Expand.expand`; enforestation should not replace hygiene.
- [x] Module files parse through enforestation.

## Phase 7D: Type And Pattern Syntax Classes

Status: Implemented.

### Purpose

Separate expression parsing from type and pattern parsing so macros can eventually be problem-aware.

### Scope

Add syntax classes for:

- [x] type expressions: arrows, implicit Pi `[A : Type] -> ...`, applied nominal types `Option(I64)`, qualified types;
- [x] patterns: wildcard, binders, literals, constructors, qualified constructors, tuples, records, qualified records, or-patterns;
- [x] match clauses.

The type parser covers the syntax-spec examples that have direct current `Surface.t` equivalents.

`Enforest` now exports three per-class entrypoints: `parse_expr`, `parse_type`, and `parse_pat`.
Type parsing handles `->` as a right-associative arrow with per-class precedence in `Enforest`
(rather than reusing the expression operator table). Record-pattern brace groups are recognized
in `parse_pat_postfix`, dotted pattern postfixes support qualified constructor and record heads such as `C.Red`, `N.M.X(n)`, and `Alias.Point {x}`, and `parse_type_terms` is used by parameter annotations in `parse_fn`.

### Tests

- [x] `fn(x : Option(I64)) -> x` lowers to type application `Ap(Option, Explicit, I64)`;
- [x] `fn(f : I64 -> Bool) -> f(1)` parses arrow type in param annotation;
- [x] `fn(f : I64 * Bool -> Bool) -> f(1, true)` parses product type `ProdTy`;
- [x] `match x do Some(y) -> y | None -> 0 end` parses constructor payload in match;
- [x] `match x do M.Some(y) -> y | M.None -> 0 end` parses qualified constructor patterns;
- [x] `match p do Point{x; y} -> x end` parses record pattern shorthand;
- [x] `match p do M.Point{x; y} -> x end` parses qualified record patterns;
- [x] `match p do Point{x = n; _} -> n end` parses record pattern renamed field with partial;
- [x] `fn(m : sig x : I64 end) -> m.x` parses signature sugar in parameter annotations.

### Exit Criteria

- [x] Enforestation can request expression/type/pattern parsing explicitly.
- [x] The code has a single place that defines operator precedence per syntax class.

## Phase 7E: User-Visible Syntax Extension

Status: Implemented for the first constrained prefix/infix slice. General Honu-style syntax extension remains future work.

### Purpose

Validate that enforestation is not merely a hard-coded parser replacement.

### Scope

Implement the first constrained syntax-extension bindings:

- [x] one macro-defined prefix form;
- [x] one macro-defined infix operator with precedence and associativity;
- [x] imported syntax extensions use Stage 6 phase-aware macro visits.

This phase is deliberately small. The current API is not the final macro syntax:

```fun
syntax prefix name = fn(stx) -> ...
syntax infix op precedence left = fn(stx) -> ...
syntax infix op precedence right = fn(stx) -> ...
```

The declaration registers a prefix or infix operator during enforestation and expands uses as ordinary macro calls. Prefix syntax passes the parsed operand syntax to the macro. Infix syntax passes a two-element product syntax containing the left and right operands.

Current implementation notes:

- Dynamic operators live in `Operator_env` and are scoped with snapshot/restore around `do` blocks and module parsing.
- `pub syntax ...` exports operator declarations through `Core_loader.load_syntax_exports` without exposing them as runtime fields.
- The RHS macro value is parsed and later expanded through the existing `MacroBinding` path; syntax-extension declarations do not introduce arbitrary grammar productions yet.

### Tests

- [x] local prefix syntax extension works;
- [x] local infix operator groups by declared precedence;
- [x] imported public syntax extension works at compile time;
- [x] runtime imports do not expose compile-time syntax-extension bindings as values;
- [x] syntax-extension shadowing is lexical and deterministic.

### Exit Criteria

- [x] At least one prefix form and one infix operator are defined outside hard-coded parser branches.
- [x] The implementation composes with the existing phase-aware loader.
- [x] Ambiguous or unsupported syntax-extension cases produce deterministic errors for the implemented slice.

## Remaining Stage 7 Work Tracker

These items are the known Stage 7 debt before treating enforestation as a stable foundation for Stage 8 problem-aware macros.

### Architecture

- [ ] Generalize the current statement pre-scan into a real Honu-style two-pass declaration pass:
  - pass 1 discovers value/type/module names and syntax-extension bindings for a block/module;
  - pass 2 enforests nested bodies with the completed binding and syntax-extension environment.
- [ ] Replace the process-global dynamic operator refs in `Operator_env` with an explicit lexical operator environment threaded through enforestation.
- [ ] Decide whether `Syntax_class.t` should become part of the parser API/context or be removed until Stage 8 needs it. It currently exists as a marker type, while the real separation is through `parse_expr`, `parse_type`, and `parse_pat` entrypoints.
- [ ] Represent operator declarations as first-class data with fixity, precedence, associativity, syntax class, and expansion behavior instead of only prefix/infix helper registration.
- [ ] Move more built-in operator handling into the same operator-table machinery used by syntax extensions where practical, especially arithmetic/comparison operators and assignment.

### Syntax Extension Semantics

- [ ] Define the final user-facing syntax-extension declaration syntax, or explicitly mark the current `syntax prefix` / `syntax infix` forms as provisional in user docs.
- [ ] Specify shadowing and import precedence rules for multiple syntax extensions with the same symbol.
- [ ] Add deterministic ambiguity errors for incomparable syntax-extension candidates once operator environments become lexical rather than global snapshots.
- [ ] Support syntax-extension declarations that affect later forms in the same block/module without relying on order-sensitive ad hoc scans.
- [ ] Decide whether syntax extensions can be defined for type, pattern, declaration, module-item, and block syntax classes before Stage 8 exposes problem-aware macros.

### Macro Integration

- [ ] Make syntax-extension expansion report useful source spans for the operator declaration and the use site.
- [ ] Check that syntax-extension macro RHS values are expanded/evaluated through the same phase-aware path as `macro` / `pub macro` declarations in every supported module/block position.
- [ ] Add negative tests for imported syntax-extension cycles that are distinct from runtime circular imports and ordinary macro visits.
- [ ] Add tests for unsupported syntax-extension shapes so failures stay deterministic as the parser grows.

### Cleanup And Documentation

- [x] Update `SYNTAX_SPEC.md`, `IMPLEMENTATION_PLAN.md`, and `CLAUDE.md` so they agree on current comment syntax, Stage 7 status, and provisional syntax-extension forms.
- [ ] Remove stale Menhir-related test dependencies if they are no longer needed by the semantic/backend test stanzas.
- [ ] Document the current parser boundary: raw reader -> enforestation -> hygienic expansion -> lowering to `Surface.t`.
- [ ] Keep `Surface.t` as the elaborator-facing boundary until Stage 8+ proves an interleaved expansion/elaboration design.

## Compatibility Rules

- `Parse_expand.parse_expr` and `Parse_expand.parse_module` remain the only downstream entrypoints.
- Existing tests should not use a parser entrypoint below `Parse_expand` unless they are specifically testing the reader/enforester.
- New final syntax features should be added through enforestation.
- Lowering to `Surface.t` remains the elaborator-facing boundary for Stage 7.

## Error Handling Rules

Stage 7 should produce simple but deterministic errors. Good enough for this phase:

- unmatched delimiters report the delimiter and source span when available;
- unsupported forms report that they are outside the current enforestation slice;
- operator parse failures report the operator and expected operand position;
- unsupported redesigned forms should fail deterministically rather than silently choosing another parser.

## Implementation Notes

### Enforest Contract

The core function should consume exactly one form from a stream:

```text
enforest_expr : env -> min_prec -> raw_term list -> Syntax.t * raw_term list
```

It should follow the Honu/Pratt shape:

1. read a prefix/primary form;
2. repeatedly consume postfix and infix operators whose precedence is high enough;
3. return the grouped form and the unconsumed terms.

### Operator Environment

Represent built-ins as data where possible:

- name/token;
- fixity: prefix, infix, postfix, delimiter/postfix-call;
- precedence;
- associativity;
- builder function from syntax operands to `Syntax.t`.

Hard-coded branches are acceptable for complex first-token forms such as `fn`, `if`, `match`, and `do`, but arithmetic and comparison should use the table from the beginning.

### Syntax Classes

Use explicit syntax classes even before user-defined classes exist:

- `Expr`;
- `TypeExpr`;
- `Pattern`;
- `Decl`;
- `ModuleItem`;
- `Block`.

This keeps Stage 8 problem-aware macros from needing to reverse-engineer parser context later.

### Hygiene Boundary

Enforestation may create `Syntax.id` values with source spans, but it should not become the authoritative name-resolution pass. Scope introduction and resolution should remain in `Expand.expand` and `Expand_ctx`.

## Completed Initial Work Order

1. [x] Add `test/syntax/test_enforest.ml` with tests for Phase 7A shapes.
2. [x] Add raw reader support and make the reader tests pass.
3. [x] Add `Operator_env` and expression enforest support for literals, identifiers, parentheses, calls, fields, and operators.
4. [x] Route `Parse_expand.parse_expr` through enforestation.
5. [x] Add `do ... end` binding/final-expression support.
6. [x] Run `dune exec test/syntax/test_syntax.exe` and `dune build`.
7. [x] Update `IMPLEMENTATION_PLAN.md` Stage 7 status after Phase 7A and the later Stage 7 slices were implemented.

## Phase 7A Acceptance Checklist

- [x] `test/syntax/test_enforest.ml` exists and is included in `test/syntax/test_syntax.ml`.
- [x] `lib/expand/enforest.ml` exists.
- [x] `lib/expand/operator_env.ml` exists.
- [x] `lib/expand/syntax_class.ml` exists.
- [x] `Parse_expand.parse_expr` can parse the Phase 7A redesigned expression slice.
- [x] Parser-entrypoint users route through `Parse_expand`.
- [x] The old `core_parser.mly` parser is removed rather than extended for Phase 7A syntax.
- [x] `dune exec test/syntax/test_syntax.exe` passes.
- [x] `dune build` passes.
