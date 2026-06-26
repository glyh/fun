# Macro Implementation Plan

## Goal

Build a hygienic, regular-syntax, type-integrated macro system for `fun` without committing the prototype to a dead-end parser or a surface-only rewrite pass.

The final system should support:

- syntax objects with source spans, scope sets, and phase information;
- hygienic expansion by default, with explicit controlled-capture tools later;
- regular syntax extension through enforestation rather than more Menhir grammar growth;
- phase-aware modules/imports so compile-time dependencies stay separate from runtime dependencies;
- first-class compile-time macro values;
- problem-aware, type-aware, and type-providing macros that cooperate with bidirectional elaboration.

The safe path is incremental:

1. [x] Build the syntax/hygiene substrate.
2. [x] Route all existing programs through the expansion boundary without behavior changes.
3. [x] Add minimal user macros only after the boundary is stable.
4. [x] Add phase-aware imports before imported macros.
5. [x] Add enforestation before general syntax extension.
6. [ ] Add type-aware and stuck macros after basic expansion is proven deterministic. (Stage 7 complete — Stage 8+ pending.)

## Current Status

Completed:

- [x] `Source_span`, `Raw_syntax`, `Scope_set`, and `Syntax` modules exist in `lib/core_kernel/` (moved from `lib/syntax/` to break dependency cycle for `VStx`).
- [x] Raw syntax reader terms and enforestation exist.
- [x] `core_tt_expand` exists with binding resolution, expander context, built-in expander, enforestation, lowering to `Surface.t`, and `Surface.t -> Syntax.t` compatibility conversion.
- [x] `Parse_expand.parse_expr` and `Parse_expand.parse_module` are the downstream parser entrypoints.
- [x] REPL, loader, stdlib prelude parsing, semantic tests, backend tests, and syntax parser-shape tests use `Parse_expand`.
- [x] The old direct `Core_lexer.parse_expr` / `Core_lexer.parse_module` pipeline and Menhir parser files were removed.
- [x] Syntax tests are a single Alcotest executable: `dune exec test/syntax/test_syntax.exe`.
- [x] Stage 4 is complete: expander allocates lexical scopes for all binding forms, resolves shadowed identifiers to stable internal names, and lowers to `Surface.t` without losing identity.
- [x] Stage 5 is complete: local macro definitions, macro API primitives, hygiene tests, and panic propagation work through NBE integration.
- [x] Stage 6 is complete for imported expression macros: runtime imports and compile-time macro visits use separate loader states/caches, imported public macros expand, and macro-generated imports go through phase checks.

Current focus:

- [x] Stage 7 enforestation infrastructure is complete through the constrained prefix/infix syntax-extension slice.
- [x] Stage 7F practical macro authoring: `syntax <head-token> do | pattern -> replacement end`, infix-only precedence-aware `operator` forms, and at least one non-dummy enforestation-backed macro.
- [x] Stage 7H module/struct declaration templates: `decl` holes, `multi ... end` declaration-template output, and declaration-template contexts for value and typed value bindings with optional `pub`.
- [x] Stage 7G computed hygienic macro authoring: `Syntax.Expr` as a nominal matchable ADT with pattern synonyms (`Var`, `Ap`, `Lam`, `Let`, `Atom`). `Syntax.*` builders construct ADT values directly. `wrap_stx`/`unwrap_stx` use real nominals from stdlib (no dummies). Source spans preserved through round-trip. Nested pattern matching with named binders works. ADT matching preserves hygiene. All 28 `stx_*` primitives eliminated. `rec fn` in module bindings supported (`LetBinding.recursive`). Class-specific ADTs (`Pattern`, `TypeExpr`, `Decl`, `Decls`) deferred to Stage 8.
- [x] Stage 7I macros generating macros: `multi ... end` generates `macro`, `syntax`, and `operator` declarations with correct scoping, public export, later-wins shadowing, and cycle detection in module/module-file contexts. Generated public operators/macros rejected in runtime structs.
- [x] Multi-arg macros: `MacroCall(f, args_list)`, `m @ (a, b, c)` parsed via `parse_args`, expander applies fold_left.
- [x] Operator redesign: `infix (sym) prec Assoc (params) -> body`. Template form: `infix (sym) prec Assoc ($a, $b) -> $a - $b` uses `substitute_template_captures`.
- [x] Syntax expanding to macro: template replacement can contain `macro @ (args)` calls.
- [x] Stage 8: kind-tagged macros with Expr/Decl annotation, context validation, decl-position macro calls, and multi-binding returns.

## Validation Commands

Run these after every stage:

```sh
dune build
dune test
```

Useful focused commands:

```sh
dune exec test/syntax/test_syntax.exe
dune exec test/semantic/test_elaborate.exe
dune exec test/backend/test_core.exe
```

## Architecture Target

Current implemented pipeline:

```text
source text
  -> Raw_syntax reader terms
  -> Enforest Syntax.t
  -> Expand.expand_expr / expand_module
  -> Lower_surface.lower_expr
  -> Surface.t
  -> Elaborate.on_expr
  -> Core.term + semantic type
  -> NbE/backend
```

Long-term pipeline:

```text
source text
  -> tokens with spans
  -> raw syntax / token trees
  -> syntax objects with scope sets and phase
  -> enforestation using current syntax/operator environment
  -> hygienic expansion (fixed-kind macros: Expr or Decl)
       - expression macro evaluation
       - declaration macro evaluation
       - type-aware reflection (Stage 10)
  -> post-expansion Surface.t
  -> Core.term + semantic type
  -> NbE/backend
```

Keep `Surface.t` as the elaborator-facing boundary. Since macro kinds are fixed per definition, no task graph scheduler is needed.

## Design Invariants

These are non-negotiable unless this document is updated with a replacement invariant.

1. Hygiene is scope-set based, not textual renaming.
2. Alpha-renamed strings may be used only as a lowering encoding for `Surface.Var` compatibility.
3. User macros must not run through the runtime import path as ordinary values.
4. Expansion and typechecking interleavings must be deterministic.
5. New final syntax features should go through enforestation.
6. No user-defined macros should be exposed before Stage 4 is solid.

TURNSTILE/Klister notes:

- Use TURNSTILE as the guide for type rule shape: synthesize/check modes, expected-type propagation, type-directed rewriting, and reusable type-operation hooks.
- Fixed-kind macros (Expr/Decl per definition) eliminate the need for a Klister-style scheduler. Macros always produce their declared kind; no stuck expansion or expansion variables are needed.
- Stage 10 type-aware macros stay read-only: expose expected type, fail clearly when unavailable.

Identifier resolution rule:

1. Each identifier occurrence has a written name and a scope set.
2. Each binding has a written name and a binding scope set.
3. Candidates are bindings with the same written name whose binding scope set is a subset of the occurrence scope set.
4. Choose the candidate with the largest compatible scope set.
5. Reject no candidate where required, or ambiguous incomparable best candidates.

## Stage 0: Baseline Regression Lock

Status: Done.

Purpose:

- Preserve current language behavior before macro infrastructure changes.

Implemented:

- [x] Existing syntax, semantic, and backend test suites still pass.
- [x] Syntax tests were reorganized by topic under `test/syntax/`.
- [x] Downstream parser entrypoint is now `Parse_expand`.

Exit criteria:

- [x] `dune build` passes.
- [x] `dune test` passes.
- [x] REPL and loader parse through `Parse_expand`.

## Stage 1: Source Spans And Raw Syntax

Status: Done for the compatibility-parser milestone.

Purpose:

- Introduce syntax representations that can carry macro information while preserving existing parser behavior.

Implemented files:

- [x] `lib/core_kernel/source_span.ml`
- [x] `lib/syntax/raw_syntax.ml`
- [x] `lib/core_kernel/syntax.ml`
- [x] `lib/expand/enforest.ml`

Implemented tasks:

- [x] Source span type with file, byte offsets, line/column positions, and synthetic marker.
- [x] Raw reader produces span-preserving terms.
- [x] Raw syntax token/group data structures.
- [x] Parser entrypoints route through reader/enforestation and `Parse_expand`.

Remaining optional follow-ups:

- [ ] Add line/column span tests for multi-line inputs.
- [ ] Add raw delimiter-group construction tests once raw token-tree parsing begins.

Exit criteria:

- [x] Span/token APIs exist.
- [x] Old direct parse entrypoints are not used downstream.
- [x] Existing language behavior is unchanged.

## Stage 2: Syntax Objects And Scope Sets

Status: Done for resolver substrate.

Purpose:

- Build hygiene primitives before user macro authoring exists.

Implemented files:

- [x] `lib/core_kernel/scope_set.ml`
- [x] `lib/core_kernel/syntax.ml`
- [x] `lib/expand/binding.ml`
- [x] `test/syntax/test_scope_sets.ml`

Implemented tasks:

- [x] Scope-set representation and operations.
- [x] Syntax identifiers with written name, source span, and scope set.
- [x] Binding table with scope-set-aware resolution.
- [x] Tests for lexical shadowing, non-capture, and ambiguous best binding rejection.

Remaining optional follow-ups:

- [ ] Add property-style tests for scope-set subset/union laws if the representation changes.
- [ ] Add tests for nested generated scopes once user macros can introduce identifiers.

Exit criteria:

- [x] Hygiene behavior is testable without elaboration.
- [x] Binding resolution does not use gensym/textual renaming as the authoritative model.

## Stage 3: Built-In Hygienic Expander To Surface.t

Status: Done for the compatibility-parser milestone.

Purpose:

- Establish the expansion boundary while preserving existing semantics.

Implemented files:

- [x] `lib/expand/dune`
- [x] `lib/expand/expand_ctx.ml`
- [x] `lib/expand/expand.ml`
- [x] `lib/expand/lower_surface.ml`
- [x] `lib/expand/surface_to_syntax.ml`
- [x] `lib/expand/parse_expand.ml`
- [x] `test/syntax/test_expand_compat.ml`

Implemented tasks:

- [x] Expander context with binding table, phase, scope allocator, and future loader slot.
- [x] Built-in expansion walk over current `Syntax.t` forms.
- [x] Lowering from expanded `Syntax.t` to `Surface.t`.
- [x] `Parse_expand.parse_expr` and `Parse_expand.parse_module` as public downstream parse entrypoints.
- [x] REPL, loader, stdlib prelude, semantic tests, backend tests, and syntax parser-shape tests use `Parse_expand`.
- [x] Direct `Core_lexer.parse_expr` and `Core_lexer.parse_module` removed.
- [x] Menhir parser files removed after enforestation became the parser entrypoint.

Current limitations:

- [ ] User-defined syntax extensions are still constrained to the implemented prefix/infix operator-extension slice.
- [ ] Type-aware and stuck/problem-aware macros are not implemented yet.

Exit criteria:

- [x] All current programs parse through the expansion boundary.
- [x] `dune build` and `dune test` pass.

## Stage 4: Move Name Introduction Into Expansion

Status: Done.

Purpose:

- Make lexical identity authoritative before user macros can introduce identifiers.

Prerequisites:

- [x] Stages 1 through 3 complete.
- [x] All downstream consumers use `Parse_expand`.

Concrete tasks:

- [x] Audit every binding form currently represented in `Syntax.t`:
   - lambda parameters;
   - let and let-rec names;
   - type names;
   - constructor names;
   - effect family names;
   - trait names;
   - impl-local method names;
   - module/struct public and private members;
   - method parameters;
   - pattern binders.
- [x] For each binding form, define exactly where the introduced scope applies:
   - binder itself;
   - type annotation;
   - value RHS;
   - body;
   - sibling declarations in modules/structs;
   - patterns and branch bodies.
- [x] Make `Expand.expand` allocate scopes according to that definition.
- [x] Stop relying on final string names for correctness.
- [x] Add lowering-time stable internal names if `Surface.t` cannot preserve resolved identity.
- [x] Add regression tests for lexical hygiene through elaboration/evaluation.

Implementation details:

- `Binding.has_name` and `Expand_ctx.fresh_resolved_name` generate stable internal
  names (e.g. `x__0`) when a binding name already exists in scope, so shadowed
  binders lower to distinct strings that the elaborator can tell apart.
- `expand_struct_bindings` processes struct/module members sequentially,
  propagating earlier member scopes into later members so sibling declarations
  can refer to each other without leaking into previous members' RHS.
- `expand_pat_binders` collects pattern-binder scopes and adds them to both the
  pattern and branch body before expansion, fixing resolution of bound
  identifiers inside match branches.
- Type-definition names and constructor binders were converted from plain strings
  to `Syntax.id` so their lexical scopes are managed uniformly.
- Arrow binder names (for dependent Pi) are also converted to `Syntax.id` and
  scoped, and the parameter lists of RecordTypeDef/TypeDef/EffectDef/TraitDef
  are `Syntax.id list` rather than plain strings.
- Public struct members (field names, method names, type/ctor names) keep their
  written resolved names for the elaborator API; only local binders that are
  private receive numbered suffixes when shadowed.

Suggested files:

- `lib/expand/expand.ml`
- `lib/expand/expand_ctx.ml`
- `lib/expand/binding.ml`
- `lib/expand/lower_surface.ml`
- `lib/expand/surface_to_syntax.ml`
- `lib/syntax/syntax.ml`
- `test/syntax/test_scope_sets.ml`
- `test/syntax/test_expand_compat.ml`
- `test/semantic/test_elaborate.ml`
- `test/backend/test_core.ml`

Tests added:

- [x] Nested same-name lets resolve to the innermost binding (syntax lowering + backend eval).
- [x] Lambda parameter shadows an outer let with the same name (syntax lowering + semantic type-check + backend eval).
- [x] Let-rec RHS sees the recursive binding but non-rec let RHS does not (syntax lowering + semantic type-check + backend eval).
- [x] Pattern binders shadow outer names only inside their branch body (syntax lowering + semantic + backend).
- [x] Or-pattern alternatives bind the same names and reject incompatible binder sets at the existing layer (existing semantic test continues to pass).
- [x] Struct/module private members do not capture fields outside their intended scope (all existing struct/module tests pass).
- [x] Generated fresh scopes in tests do not accidentally capture user identifiers (expand_hygiene suite in test_expand_compat.ml).

Exit criteria:

- [x] Scope-set resolution is the authoritative lexical model before elaboration.
- [x] Lowering preserves resolved identity well enough for the existing string-based elaborator.
- [x] All existing tests pass (syntax 60/60, semantic 310/310, backend 189/189).


## Stage 5: Minimal Local Macro Definitions

Status: Done for local expression macros.

Purpose:

- Expose first user-defined macros only after Stage 4 proves hygiene.

First supported shape:

```fun
do macro name(stx) -> body; ... end
```

Invocation: `name @ ( arg )` using the `@` token.

Keep this syntax provisional.

Concrete tasks:

- [x] Decide whether macro declarations live only in raw/expanded syntax or also appear in `Surface.t`.
    (Live in both Surface.t and Syntax.t; expanded away before elaboration.)
- [x] Add compile-time macro value representation:
   - macro closure implemented via NBE-evaluated `Core.term` → `Core.value` (specifically `VLam`).
   - syntax value: `VStx of Syntax.t` in Core.value, `Stx of Syntax.t` in Core.term.
   - macro errors propagate as exceptions in the expander.
- [x] Add a minimal macro evaluator that cannot run arbitrary runtime refs/effects.
    (Macro bodies are elaborated to `Core.term` and evaluated by NBE, which does not support
    refs or effects in a compile-time context.)
- [x] Add macro API primitives (7 of 8 implemented):
   - [x] construct identifiers (`stx_make_var : String -> Stx`);
   - [x] construct literals (`stx_make_i64`, `stx_make_bool`, `stx_make_string`);
   - [x] construct groups — application (`stx_make_ap`) and lambda (`stx_make_lam`);
   - [x] return syntax (the final expression in the macro body is the return value);
   - [x] raise syntax error (use existing `panic` built-in);
   - [x] inspect syntax kind (`stx_kind : Stx -> String`);
   - [x] compare identifiers by hygiene-aware operations (`stx_id_eq : Stx -> Stx -> Bool` — same name + compatible scope sets).
- [x] Support one invocation shape only, `name @ ( expr )`.
- [x] Expand simple local macro calls in expression position.

Implementation details:

- `macro name(stx) -> body` is parsed as `Surface.MacroDef` → `Syntax.MacroDef`.
  The expander lowers the body to `Surface.t`, elaborates via the `elaborate` callback
  (provided by `Expand_ctx`; wired to `Elaborate.on_expr` + `Nbe.eval` in the REPL),
  and registers the resulting `Core.value` (a `VLam`) in `Expand_ctx.macro_table`.
- `name @ ( arg )` is parsed as `Surface.MacroCall` → `Syntax.MacroCall`.
  The expander looks up the macro, wraps the argument `Syntax.t` as `VStx`, applies
  the closure via the `eval_and_apply` callback (`Nbe.apply`), unwraps `VStx` from
  the result, and splices the expanded syntax back into the expansion tree.
- `VStx` and `Stx` are opaque to NBE: `Stx` evaluates to `VStx`, `VStx` quotes back
  to `Stx`, and two `VStx` values are always considered convertible.
- `cont`, `result`, `effect_request` moved from NBE to `core.ml` so `VCont` can
  hold `cont` directly (zero `Obj.t` anywhere in the codebase).
- All shared type definitions (`explicitness`, `trait_bound`, `atom_ty`) extracted
  to dedicated kernel modules; no duplicated aliases.

Suggested files:

- `lib/expand/macro_value.ml`
- `lib/expand/macro_eval.ml`
- `lib/expand/expand.ml`
- `lib/expand/expand_ctx.ml`
- `lib/expand/parse_expand.ml`
- `lib/core_kernel/core.ml`
- `lib/core_kernel/syntax.ml`
- `lib/core_kernel/explicitness.ml`
- `lib/core_kernel/trait_bound.ml`
- `lib/core_kernel/atom_ty.ml`
- `bin/main.ml`
- `test/syntax/test_macros.ml`

Tests added:

- [x] Local macro expands to an atom (identity macro: `do macro id(stx) -> stx; id @ (42) end`).
- [x] Local macro expands to an application (identity over `1 + 2`).
- [x] stx_make_i64 literal construction.
- [x] stx_make_ap expression construction (1 + 2 via primitives).
- [x] stx_make_lam + stx_make_var identity lambda construction.
- [x] Macro-generated identifier does not capture user binding.
- [x] User identifier does not capture macro-generated binding.
- [x] Macro error includes a useful span (panic message propagates).

Exit criteria:

- [x] Local macros expand simple syntax.
- [x] Generated identifiers are hygienic by default.
- [x] No imported macros yet.
- [x] No type-aware macros yet.

## Stage 6: Phase-Aware Module Loading

Status: Done for imported expression macros.

Purpose:

- Support imported macros without conflating compile-time visits and runtime imports.

Prerequisites:

- [x] Stage 5 local macros work.

Concrete tasks:

- [x] Add `macro` / `pub macro` as module and struct bindings in `Surface.t`, `Syntax.t`, parser conversion, expansion, and lowering.
- [x] Register macro bindings during expansion when an elaboration callback is available.
- [x] Drop macro bindings before elaboration so compile-time-only macros are not exposed as runtime module/struct members.
- [x] Add `Core_loader.visit_macros` as a provisional compile-time visit path for imported public macro bindings.
- [x] Wire REPL imported macro loading through `Core_loader.visit_macros` instead of an ad-hoc local loader.
- [x] Add backend regressions for imported macro expansion and compile-time-only non-exposure as runtime fields.
- [x] Split `Core_loader`'s old single parsed-module cache into raw parsed-module and expanded runtime-module caches.
- [x] Add a compile-time visited macro cache for public imported macro values.
- [x] Add an elaborated runtime module cache for imported module core/type/value triples.
- [x] Replace the provisional compile-time visit path with phase-indexed import modes:
   - runtime import for values/types;
   - compile-time visit for macros;
   - future phase shifts for macros defining macros are deferred until nested macro phases are implemented.
- [x] Annotate exported members by phase:
   - runtime value/type/effect/trait/impl;
   - compile-time macro;
   - both only if explicitly supported by adding both a runtime binding and a macro binding.
- [x] Make circular import detection phase-indexed for runtime imports versus compile-time macro visits.
- [x] Ensure macro-generated imports still go through phase checks.

Suggested files:

- `lib/loader/core_loader.ml`
- `lib/expand/module_expand.ml`
- `test/semantic/test_elaborate.ml`
- `test/syntax/test_macros.ml`

Tests to add:

- [x] Imported macro expands in another file.
- [x] Compile-time-only macro is not exposed as runtime value.
- [x] Runtime circular imports and compile-time circular visits report separately.
- [x] Macro-generated imports cannot bypass phase checks.

Exit criteria:

- [x] Imported macros work for simple public macro bindings.
- [x] Runtime imports behave as before.
- [x] Phase errors are deterministic for current runtime import and compile-time macro visit phases.

## Stage 7: Enforestation And Regular Syntax Extension

Status: Implemented through expression syntax-template macro authoring.

Purpose:

- Stop growing Menhir as the final surface grammar and enable regular syntax extension.
- Use the enforestation boundary as the point to implement the planned surface-syntax redesign instead of doing another committed Menhir redesign first.

Prerequisites:

- [x] Stage 6 if imported syntax extensions are desired.
- [x] Stage 5 is enough for local-only syntax experiments.

Concrete tasks:

- [x] Add an enforestation pass over token/syntax streams.
- [x] Represent syntax classes:
   - expression;
   - type;
   - pattern;
   - declaration/module item;
   - block/body.
- [x] Add redesigned built-in prefix forms through enforestation entries:
   - `fn` anonymous functions with explicit `()` and implicit `[]` params;
   - `if cond do ... else ... end`;
   - simple `match scrut do pat -> body | ... end` value branches;
   - `ref(expr)`, `deref(expr)`, `resume expr`, and `resume ()`;
   - `import "path"`;
   - `open M` statements inside `do` blocks.
- [x] Add redesigned declaration/module-item parsing through enforestation:
   - top-level `parse_module` support for `pub x = expr`, `x : Type = expr`, `fn f(params) ...`, `macro name(params) -> expr`, and named `M = module ... end`;
   - expression `do` support for typed declarations and recursive function declarations;
   - expression `module ... end` and `struct ... end` support.
- [x] Decide the Stage 7 declaration-order model for syntax extensions:
   - syntax extensions are sequential and affect only later forms;
   - duplicate operators use deterministic later-wins shadowing;
   - full Honu-style two-pass declaration discovery is deferred unless future syntax extensions need whole-block scope.
- [x] Separate type/pattern/expression parsing through per-class entrypoints:
   - `Enforest.parse_type` for type expressions with arrows, implicit Pi, type application, products;
   - `Enforest.parse_pat` for patterns with record fields, constructor payloads;
   - `Enforest.parse_expr` for expression forms.
- [x] Represent operator declarations:
   - fixity;
   - precedence;
   - associativity;
   - expansion function.
- [x] Move current operator parsing into enforestation tables where practical for Stage 7:
   - application;
   - field access;
   - prefix forms such as `ref`, `deref`, `perform`, `resume`;
   - infix arithmetic/comparison/assignment;
   - arrows/effect rows where appropriate.
- [x] Reintroduce the initial planned surface-syntax redesign through enforestation tables and built-in syntax entries.
- [x] Decide which legacy syntactic forms should remain accepted, which should stay rejected, and document the redesigned syntax as the tested direction.
- [x] Prefer implementing redesigned constructs as built-in macro/enforestation entries so user macros exercise the same mechanism.
- [x] Use this stage for larger block/form experiments, including Ruby/Elixir-style `do ... end` shapes if still desired; decision: current `do ... end` syntax is the Stage 7 block experiment and broader block-form experiments are deferred.
- [x] Removed the Menhir parser files after enforestation covered the tested language surface.
- [x] Implemented at least one macro-defined prefix form and one macro-defined infix operator.

Suggested files:

- `lib/syntax/raw_syntax.ml`
- `lib/expand/enforest.ml`
- `lib/expand/syntax_class.ml`
- `lib/expand/operator_env.ml`
- `test/syntax/test_enforest.ml`

Tests to add:

- [x] Existing operator precedence remains compatible or intentionally changes with migration tests.
- [x] Redesigned built-in syntax has parser/enforestation tests.
- [x] Redesigned syntax is the documented/tested direction.
- [x] Ruby/Elixir-style block syntax examples either work or are explicitly deferred with rationale.
- [x] Built-in prefix forms parse through enforestation without Menhir grammar growth.
- [x] Initial redesigned declaration/module syntax has parser/enforestation tests.
- [x] Type/pattern/expression syntax classes have per-class parser/enforestation tests.
- [x] Macro-defined prefix form works.
- [x] Macro-defined infix operator works.
- [x] Syntax extension composes with module/import scoping.

Exit criteria:

- [x] Initial redesigned syntax slice works through enforestation.
- [x] Menhir is no longer the place new final syntax features are added.

## Stage 8: Kind-Tagged Macros

Status: Complete.

Purpose:

- Let macros declare their syntactic return kind via explicit annotation (defaults to Expr).
- Each macro has a fixed return kind — Expr or Decl — simplifying later stages.

Design:

- `macro name(params) -> ...` — default: Expr
- `macro name(params) : Decl -> ...` — annotated: Decl
- `macro name(params) : Expr -> ...` — explicit: Expr
- Kind is static per macro; no runtime dispatch
- Name shadowing: later bindings win regardless of kind

Implemented:

- [x] Parse optional `: Decl` / `: Expr` annotation on macro definitions
- [x] Default kind is Expr
- [x] Store kind in MacroBinding/MacroDef (Syntax.t and Surface.t)
- [x] Expand_ctx tracks current context kind and per-macro kind
- [x] Validate call site kind matches macro kind
- [x] parse_expr sets Expr context, parse_module sets Decl context
- [x] MacroCallBinding in declaration position (module/struct bodies)
- [x] Decl macros can return multiple bindings
- [x] Imported macro kinds preserved via Core_loader
- [x] Tests: annotation parsing, kind rejection, name shadowing, decl-position calls

## Stage 9: Decl/Pattern ADT Completion

Status: Not started.

Purpose:

- Define ADT constructors for `Syntax.Decl`/`Syntax.Decls` (and `Syntax.Pattern`) so macros can construct/splice declaration and pattern syntax, not just expressions.

Concrete tasks:

- [ ] Add DeclBinding ADT to stdlib: `Syntax.DeclBinding(binding)` nominal
- [ ] Add wrap_stx support for Decl/Decls ADT constructors
- [ ] Add builder functions: `Syntax.decl(binding)`, `Syntax.decls(bindings)`
- [ ] Add Syntax.Pattern ADT and pattern-synonym constructors (Stage 7G deferred)
- [ ] Tests: Decl macro returning `Syntax.decl(...)`, multi-decl macros

## Stage 10: Type-Aware Macros

Status: Not started.

Purpose:

- Expose expected-type information to macro bodies without a scheduler (kinds are fixed, no stuck expansion needed).
- This is a read-only reflection API: a macro in checking mode can inspect the expected type.

Concrete tasks:

- [ ] Let expression macros in checking mode inspect expected semantic type
- [ ] Add stable reflected type heads: primitives, nominals, structural record fields
- [ ] Add macro-facing type operations: type equality, type-head reflection
- [ ] Graceful failure when expected type is unavailable

Tests to add:

- [ ] Type-directed defaulting
- [ ] Type-directed wrapper generation
- [ ] Graceful failure when expected type is unavailable

Exit criteria:

- [ ] Useful type-aware macros exist
- [ ] No scheduler required (fixed kind per macro)

## Stage 11: Macro-Powered Language Features

Status: Not started.

Purpose:

- Validate the macro system by moving suitable compiler features into macros or macro-like libraries.

Candidate validations:

- [ ] Derived helpers using type-case/reflection.
- [ ] Record/ADT deriving scaffolding.
- [ ] DSL block syntax.
- [ ] Custom pattern forms.
- [ ] Protocol-style boilerplate over traits.
- [ ] Future syntax experiments such as Ruby/Elixir-style `do ... end` blocks.

Exit criteria:

- [ ] At least one feature that would otherwise require parser/compiler changes is implemented as a macro.
- [ ] The macro implementation is simpler than bespoke compiler machinery.
- [ ] Shortcomings feed back into the macro API before any rewrite.

## Stage 12: Macro Diagnostics And Expansion UX

Status: Not started.

Purpose:

- Add the user-facing reliability and debugging tools that become necessary once untyped, problem-aware, type-aware, stuck, and type-providing macro phases exist.

Concrete tasks:

- [ ] Add macro-authored diagnostic APIs, such as syntax errors attached to explicit syntax spans.
- [ ] Add syntax-object pretty-printing and macro expansion debugging/tracing tools.
- [ ] Define recursive expansion limits and deterministic cycle explanations.
- [ ] Improve no-match, ambiguous syntax, and failed expansion messages using the complete macro context.
- [ ] Document the debugging workflow for downstream macro authors.

Tests to add:

- [ ] Macro-authored syntax errors report intended spans.
- [ ] Recursive macro expansion failures are deterministic and understandable.
- [ ] Expansion traces are stable enough for debugging without becoming golden-output noise.

Exit criteria:

- [ ] Macro authors can debug practical failed expansions without reading compiler internals.
- [ ] Diagnostics preserve hygiene and phase context in their explanations.

## Dead-End Avoidance Checklist

Do not:

- implement macros as string substitution;
- implement hygiene only as gensym renaming;
- add user macros directly on `Surface.t` as the only representation;
- reintroduce parser-generator grammar growth for final syntax experiments instead of enforestation;
- treat compile-time imports as runtime imports;
- let macro evaluation freely run runtime effects/refs;
- force type-aware macros into strict expansion-before-elaboration ordering;
- expose unstable OCaml internals as the long-term macro API.

Acceptable temporary shortcuts:

- lower resolved hygienic identifiers to alpha-renamed `Surface.Var` strings while elaboration still expects strings;
- keep compatibility shims only when they are exercised through `Parse_expand`;
- start with prefix-only macro invocation before general regular-syntax macros;
- expose only read-only expected-type reflection before stuck macros exist;
- keep typed/core macro fragments out of scope until surface-expanding macros are stable.
