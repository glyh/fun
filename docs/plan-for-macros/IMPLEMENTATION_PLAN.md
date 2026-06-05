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
6. [ ] Add type-aware and stuck macros after basic expansion is proven deterministic.

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
- [x] Add Stage 7F practical macro authoring: `syntax <head-token> do | pattern -> replacement end`, infix-only precedence-aware `operator` forms, and at least one non-dummy enforestation-backed macro.
- [ ] Add Stage 7H module/struct declaration templates: `decl` holes, `multi ... end` declaration-template output, and declaration-template contexts for value and typed value bindings with optional `pub`.
- [ ] Add Stage 7G computed hygienic macro authoring: started with a public stdlib `Syntax` module over the current primitive builders/inspectors; still needs the broader documented inspection/deconstruction/provenance API without requiring quote/unquote or user-visible symgen.
- [ ] Add Stage 7I macros generating macros: macros that produce `macro`, `syntax`, and `operator` declarations, with correct scoping and export.
- [ ] Start Stage 8: problem-aware macros.

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
  -> hygienic expansion task graph
       - ordinary expansion tasks
       - macro evaluation tasks
       - type elaboration/unification tasks
       - blocked/stuck macro continuations
  -> post-expansion Surface.t or typed fragments
  -> Core.term + semantic type
  -> NbE/backend
```

Keep `Surface.t` as the elaborator-facing boundary until surface macros are stable. Only bypass `Surface.t` for typed/core fragments in Stage 11.

## Design Invariants

These are non-negotiable unless this document is updated with a replacement invariant.

1. Hygiene is scope-set based, not textual renaming.
2. Alpha-renamed strings may be used only as a lowering encoding for `Surface.Var` compatibility.
3. User macros must not run through the runtime import path as ordinary values.
4. Expansion and typechecking interleavings must be deterministic.
5. New final syntax features should go through enforestation.
6. No user-defined macros should be exposed before Stage 4 is solid.

TURNSTILE/Klister split:

- Use TURNSTILE as the guide for type rule shape: synthesize/check modes, expected-type propagation, type-directed rewriting, and reusable type-operation hooks.
- Use Klister as the guide for scheduling: problem-aware expansion requests, expansion variables, type metavariables, blocked continuations, and deterministic stuck-macro execution.
- Do not add a scheduler merely to expose expected types. Stage 9 should stay read-only and fail clearly when the requested information is unavailable; Stage 10 is only for macros that require macro expansion and elaboration to wait on each other.
- Treat type-providing macro output as a distinct result form, not as syntax plus an implicit property. Later elaboration must verify that the eventual expansion agrees with the provided type.

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

## Stage 8: Problem-Aware Macros

Status: Not started.

Purpose:

- Let macros know which syntactic judgment they are expanding for.

Research note:

- This stage is the Klister-style `which-problem` slice. It should expose the requested judgment without yet exposing type structure or adding scheduler semantics.
- TURNSTILE's synthesize/check distinction confirms that expression inference and expression checking should be separate problem variants, not a boolean flag on expression expansion.

Problem kinds:

- expression inference;
- expression checking;
- type expression;
- pattern;
- declaration/module item;
- effect row if useful;
- trait/impl item if useful.

Concrete tasks:

- [ ] Thread problem information through expansion.
- [ ] Add macro API to inspect the current problem.
- [ ] Allow a macro binding to expand differently by problem kind.
- [ ] Integrate with bidirectional elaboration:
   - `infer` requests expression expansion with no expected type;
   - `check` requests expression expansion with expected type;
   - pattern elaboration requests pattern expansion;
   - module elaboration requests declaration expansion.

Tests to add:

- [ ] A macro distinguishes expression/type/pattern/declaration contexts.
- [ ] A checked-expression macro can observe that it is in checking mode.
- [ ] Expansion remains deterministic.

Exit criteria:

- [ ] Problem kind is visible to macros.
- [ ] No type reflection yet.

## Stage 9: Type-Aware Macros Without Stuck Expansion

Status: Not started.

Purpose:

- Expose useful expected-type information without building the full scheduler.

Research note:

- This is the TURNSTILE/MLISH expected-type slice: a checking context can make the expected type available to a macro before expansion completes.
- Keep this intentionally weaker than Klister stuck macros. A macro may inspect already-known expected type structure, but it cannot suspend waiting for an inferred subexpression type.
- Type operations exposed here should be stable hooks, such as equality/subtyping/type-head reflection, rather than raw semantic values from the OCaml implementation.

Concrete tasks:

- [ ] Let expression macros in checking mode inspect expected semantic type.
- [ ] Add stable reflected type heads:
   - primitive types;
   - nominal type constructors and arguments;
   - structural record fields where current type-case supports reflection.
- [ ] Add macro-facing type operations before broad reflection:
   - type equality for reflected types;
   - optional subtype/check relation hook if subtyping exists by then;
   - normalization/evaluation hook only if a concrete macro example needs it.
- [ ] Keep this read-only and non-stuck:
   - unknown expected type returns `unknown` or a clear error;
   - no suspension/resumption yet.

Tests to add:

- [ ] Type-directed defaulting.
- [ ] Type-directed wrapper generation.
- [ ] Record-field-based code generation.
- [ ] Graceful failure when expected type is unavailable.

Exit criteria:

- [ ] Useful type-aware macros exist.
- [ ] No scheduler required.

## Stage 10: Expansion Variables And Stuck Macro Scheduler

Status: Not started.

Purpose:

- Support expansion/typechecking interleavings where either side can wait on the other.

Research note:

- This is the Klister operational slice, not merely a more powerful TURNSTILE `local-expand`.
- Tasks should block on solved-enough problems, such as an expansion variable being filled or a type metavariable's head being known, rather than directly depending on task identities that may not exist yet.
- Scheduler order must be semantically invisible: independent unblocked tasks may run in any deterministic implementation order, but results must not depend on that order.

Concrete tasks:

- [ ] Add expansion variables analogous to metavariables.
- [ ] Add task records:
   - task id;
   - problem kind;
   - inputs/dependencies;
   - output variable;
   - continuation/action.
- [ ] Define solved-enough predicates for blocked macros:
   - expansion variable known to a constructor/head form;
   - type metavariable known to a reflected type head;
   - fully solved syntax/type when a macro requests exact structure.
- [ ] Add deterministic scheduler:
   - run unblocked tasks;
   - mark tasks blocked on type metas or expansion variables;
   - detect no-progress states;
   - report dependency cycles.
- [ ] Expose elaboration operations as tasks where needed:
   - infer type of expanded expression;
   - check expression against expected type;
   - solve unification constraints;
   - resume blocked macro once reflected type is known.

Suggested files:

- `lib/expand/expansion_var.ml`
- `lib/expand/task_graph.ml`
- `lib/semantic/typecheck/elaborate.ml`
- `lib/semantic/typecheck/unify.ml`

Tests to add:

- [ ] Macro waits for expected type.
- [ ] Typechecker waits for macro expansion.
- [ ] Independent tasks produce same result regardless of order.
- [ ] Deadlock/cycle reports waiting problems.

Exit criteria:

- [ ] Stuck macros work for small examples.
- [ ] Dependency failures are understandable enough for prototype work.
- [ ] Non-macro elaboration behavior remains unchanged.

## Stage 11: Type-Providing Macros And Typed Fragments

Status: Not started.

Purpose:

- Allow macros to provide type information before full expansion is known.

Research note:

- TURNSTILE communicates type information through expanded syntax properties, but `fun` should model this explicitly because type-providing macros can participate before full expansion is available.
- Klister's type-providing framing suggests the macro result itself should carry a provided type and delayed expansion obligation.

Concrete tasks:

- [ ] Extend macro result forms:
   - expanded syntax only;
   - provided type plus delayed expansion;
   - checked typed fragment;
   - core term plus semantic type, if needed.
- [ ] Add consistency checks for provided types:
   - surrounding elaboration may use the provided type immediately;
   - the delayed expansion must later elaborate to a compatible type;
   - type/provider mismatch reports both the macro use span and the delayed expansion span.
- [ ] Define which fragments may bypass `Surface.t`.
- [ ] Preserve hygiene for fragments introducing binders or lexical references.
- [ ] Integrate with metavariable solving:
   - provided types may contain metas;
   - delayed expansion may depend on solved metas;
   - unification failures point back to macro spans.

Tests to add:

- [ ] Macro provides a type so surrounding application/checking proceeds.
- [ ] Later expansion agrees with provided type.
- [ ] Disagreement is rejected.
- [ ] Macro-generated binders remain hygienic.

Exit criteria:

- [ ] Type-providing macros work in at least one realistic example.
- [ ] Typed/core fragments have a clear hygiene story.

## Stage 12: Macro-Powered Language Features

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

## Stage 13: Macro Diagnostics And Expansion UX

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
