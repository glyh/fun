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
3. [ ] Add minimal user macros only after the boundary is stable.
4. [ ] Add phase-aware imports before imported macros.
5. [ ] Add enforestation before general syntax extension.
6. [ ] Add type-aware and stuck macros after basic expansion is proven deterministic.

## Current Status

Completed:

- [x] `Source_span`, `Raw_syntax`, `Scope_set`, and `Syntax` modules exist in `lib/syntax/`.
- [x] `Core_lexer.tokens_with_spans` and `Core_lexer.spanned_token` exist.
- [x] `core_tt_expand` exists with binding resolution, expander context, built-in expander, lowering to `Surface.t`, and `Surface.t -> Syntax.t` compatibility conversion.
- [x] `Parse_expand.parse_expr` and `Parse_expand.parse_module` are the downstream parser entrypoints.
- [x] REPL, loader, stdlib prelude parsing, semantic tests, backend tests, and syntax parser-shape tests use `Parse_expand`.
- [x] The old direct `Core_lexer.parse_expr` / `Core_lexer.parse_module` pipeline was removed.
- [x] Syntax tests are a single Alcotest executable: `dune exec test/syntax/test_syntax.exe`.

Next phase:

- [ ] Stage 4: move all name introduction into expansion robustly and prove lowered identifiers preserve lexical hygiene before user macros exist.

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
  -> Core_lexer tokens
  -> Core_parser Menhir Surface.t
  -> Surface_to_syntax.expr
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
5. Menhir can remain as a compatibility parser, but new final syntax features should go through enforestation.
6. No user-defined macros should be exposed before Stage 4 is solid.

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

- [x] `lib/syntax/source_span.ml`
- [x] `lib/syntax/raw_syntax.ml`
- [x] `lib/syntax/syntax.ml`
- [x] `lib/syntax/core_lexer.ml`

Implemented tasks:

- [x] Source span type with file, byte offsets, line/column positions, and synthetic marker.
- [x] Token-with-span API behind `Core_lexer.tokens_with_spans`.
- [x] Raw syntax token/group data structures.
- [x] Compatibility parser remains Menhir-backed through `Parse_expand`.

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

- [x] `lib/syntax/scope_set.ml`
- [x] `lib/syntax/syntax.ml`
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

Current limitations:

- [ ] Menhir still produces `Surface.t` first, then `Surface_to_syntax` converts into macro-facing syntax.
- [ ] No raw token-tree parser yet.
- [ ] No user-defined macros yet.

Exit criteria:

- [x] All current programs parse through the expansion boundary.
- [x] `dune build` and `dune test` pass.

## Stage 4: Move Name Introduction Into Expansion

Status: Next.

Purpose:

- Make lexical identity authoritative before user macros can introduce identifiers.

Prerequisites:

- [x] Stages 1 through 3 complete.
- [x] All downstream consumers use `Parse_expand`.

Concrete tasks:

- [ ] Audit every binding form currently represented in `Syntax.t`:
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
- [ ] For each binding form, define exactly where the introduced scope applies:
   - binder itself;
   - type annotation;
   - value RHS;
   - body;
   - sibling declarations in modules/structs;
   - patterns and branch bodies.
- [ ] Make `Expand.expand` allocate scopes according to that definition.
- [ ] Stop relying on final string names for correctness.
- [ ] Add lowering-time stable internal names if `Surface.t` cannot preserve resolved identity.
- [ ] Add regression tests for lexical hygiene through elaboration/evaluation.

Suggested files:

- `lib/expand/expand.ml`
- `lib/expand/lower_surface.ml`
- `test/syntax/test_scope_sets.ml`
- `test/semantic/test_elaborate.ml`
- `test/backend/test_core.ml`

Tests to add:

- [ ] Nested same-name lets resolve to the innermost binding.
- [ ] Lambda parameter shadows an outer let with the same name.
- [ ] Let-rec RHS sees the recursive binding but non-rec let RHS does not.
- [ ] Pattern binders shadow outer names only inside their branch body.
- [ ] Or-pattern alternatives bind the same names and reject incompatible binder sets at the existing layer.
- [ ] Struct/module private members do not capture fields outside their intended scope.
- [ ] Generated fresh scopes in tests do not accidentally capture user identifiers.

Exit criteria:

- [ ] Scope-set resolution is the authoritative lexical model before elaboration.
- [ ] Lowering preserves resolved identity well enough for the existing string-based elaborator.
- [ ] All existing tests pass.

## Stage 5: Minimal Local Macro Definitions

Status: Not started.

Purpose:

- Expose first user-defined macros only after Stage 4 proves hygiene.

First supported shape:

```fun
macro name = fun stx -> ...
```

or, if simpler:

```fun
pub macro name = fun stx -> ...
```

Keep this syntax provisional.

Concrete tasks:

- [ ] Decide whether macro declarations live only in raw/expanded syntax or also appear in `Surface.t`.
- [ ] Add compile-time macro value representation:
   - macro closure;
   - primitive macro operations;
   - syntax value;
   - macro result;
   - macro error.
- [ ] Add a minimal macro evaluator that cannot run arbitrary runtime refs/effects.
- [ ] Add macro API primitives:
   - return syntax;
   - raise syntax error;
   - inspect syntax kind;
   - construct identifiers/literals/groups;
   - compare identifiers by hygiene-aware operations.
- [ ] Support one invocation shape only, for example `name!(...)`.
- [ ] Expand simple local macro calls in expression position.

Suggested files:

- `lib/expand/macro_value.ml`
- `lib/expand/macro_eval.ml`
- `lib/expand/expand.ml`
- `test/syntax/test_macros.ml`

Tests to add:

- [ ] Local macro expands to a literal.
- [ ] Local macro expands to an application.
- [ ] Macro-generated identifier does not capture user binding.
- [ ] User identifier does not capture macro-generated binding.
- [ ] Macro error includes a useful span.

Exit criteria:

- [ ] Local macros expand simple syntax.
- [ ] Generated identifiers are hygienic by default.
- [ ] No imported macros yet.
- [ ] No type-aware macros yet.

## Stage 6: Phase-Aware Module Loading

Status: Not started.

Purpose:

- Support imported macros without conflating compile-time visits and runtime imports.

Prerequisites:

- [ ] Stage 5 local macros work.

Concrete tasks:

- [ ] Replace `Core_loader`'s single parsed-module cache with phase-aware states:
   - parsed module cache;
   - expanded runtime module cache;
   - elaborated runtime module cache;
   - compile-time visited module cache.
- [ ] Add import modes:
   - runtime import for values/types;
   - compile-time visit for macros;
   - future phase shifts for macros defining macros.
- [ ] Annotate exported members by phase:
   - runtime value/type/effect/trait/impl;
   - compile-time macro;
   - both only if explicitly supported.
- [ ] Make circular import detection phase-indexed.
- [ ] Ensure macro-generated imports still go through phase checks.

Suggested files:

- `lib/loader/core_loader.ml`
- `lib/expand/module_expand.ml`
- `test/semantic/test_elaborate.ml`
- `test/syntax/test_macros.ml`

Tests to add:

- [ ] Imported macro expands in another file.
- [ ] Compile-time-only dependency is not exposed as runtime value.
- [ ] Runtime circular imports and compile-time circular visits report separately.
- [ ] Macro-generated imports cannot bypass phase checks.

Exit criteria:

- [ ] Imported macros work.
- [ ] Runtime imports behave as before.
- [ ] Phase errors are deterministic.

## Stage 7: Enforestation And Regular Syntax Extension

Status: Not started.

Purpose:

- Stop growing Menhir as the final surface grammar and enable regular syntax extension.
- Use the enforestation boundary as the point to implement the planned surface-syntax redesign instead of doing another committed Menhir redesign first.

Prerequisites:

- [ ] Stage 6 if imported syntax extensions are desired.
- [ ] Stage 5 is enough for local-only syntax experiments.

Concrete tasks:

- [ ] Add an enforestation pass over token/syntax streams.
- [ ] Represent syntax classes:
   - expression;
   - type;
   - pattern;
   - declaration/module item;
   - block/body.
- [ ] Represent operator declarations:
   - fixity;
   - precedence;
   - associativity;
   - expansion function.
- [ ] Move current operator parsing into enforestation tables:
   - application;
   - field access;
   - prefix forms such as `ref`, `!`, `perform`, `resume`;
   - infix arithmetic/comparison/assignment;
   - arrows/effect rows where appropriate.
- [ ] Reintroduce the planned surface-syntax redesign through enforestation tables and built-in syntax entries.
- [ ] Keep old syntax accepted during transition where practical, but make redesigned syntax the documented/tested direction.
- [ ] Prefer implementing redesigned constructs as built-in macro/enforestation entries so user macros exercise the same mechanism.
- [ ] Use this stage for larger block/form experiments, including Ruby/Elixir-style `do ... end` shapes if still desired.
- [ ] Keep Menhir only for compatibility parsing, delimiter handling, or recovery if useful; do not grow it as the final grammar.
- [ ] Implement at least one macro-defined prefix form and one macro-defined infix operator.

Suggested files:

- `lib/syntax/raw_syntax.ml`
- `lib/expand/enforest.ml`
- `lib/expand/syntax_class.ml`
- `lib/expand/operator_env.ml`
- `test/syntax/test_enforest.ml`

Tests to add:

- [ ] Existing operator precedence remains compatible or intentionally changes with migration tests.
- [ ] Redesigned built-in syntax has parser/enforestation tests.
- [ ] Old and redesigned syntax coexist where transition compatibility is intentional.
- [ ] Ruby/Elixir-style block syntax examples either work or are explicitly deferred with rationale.
- [ ] Macro-defined prefix form works.
- [ ] Macro-defined infix operator works.
- [ ] Syntax extension composes with module/import scoping.

Exit criteria:

- [ ] Initial redesigned syntax slice works through enforestation.
- [ ] Menhir is no longer the place new final syntax features are added.

## Stage 8: Problem-Aware Macros

Status: Not started.

Purpose:

- Let macros know which syntactic judgment they are expanding for.

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

Concrete tasks:

- [ ] Let expression macros in checking mode inspect expected semantic type.
- [ ] Add stable reflected type heads:
   - primitive types;
   - nominal type constructors and arguments;
   - structural record fields where current type-case supports reflection.
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

Concrete tasks:

- [ ] Add expansion variables analogous to metavariables.
- [ ] Add task records:
   - task id;
   - problem kind;
   - inputs/dependencies;
   - output variable;
   - continuation/action.
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

Concrete tasks:

- [ ] Extend macro result forms:
   - expanded syntax only;
   - provided type plus delayed expansion;
   - checked typed fragment;
   - core term plus semantic type, if needed.
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

## Dead-End Avoidance Checklist

Do not:

- implement macros as string substitution;
- implement hygiene only as gensym renaming;
- add user macros directly on `Surface.t` as the only representation;
- grow Menhir for final syntax experiments before enforestation;
- treat compile-time imports as runtime imports;
- let macro evaluation freely run runtime effects/refs;
- force type-aware macros into strict expansion-before-elaboration ordering;
- expose unstable OCaml internals as the long-term macro API.

Acceptable temporary shortcuts:

- lower resolved hygienic identifiers to alpha-renamed `Surface.Var` strings while elaboration still expects strings;
- keep Menhir as the compatibility parser while raw syntax/enforestation is introduced;
- start with prefix-only macro invocation before general regular-syntax macros;
- expose only read-only expected-type reflection before stuck macros exist;
- keep typed/core macro fragments out of scope until surface-expanding macros are stable.
