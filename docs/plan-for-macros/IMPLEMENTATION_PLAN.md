# Macro implementation plan

## Goal

Build a hygienic, regular-syntax, type-integrated macro system for `fun` without committing the prototype to a dead-end parser or a surface-only rewrite pass.

The macro system should eventually support:

- syntax objects with source spans, scope sets, and phase information;
- hygienic expansion by default, with explicit controlled capture tools;
- regular syntax extension through enforestation rather than Lisp-only syntax;
- phase-aware modules/imports so compile-time dependencies stay separate from runtime dependencies;
- first-class compile-time macro values;
- problem-aware, type-aware, and type-providing macros that can cooperate with bidirectional elaboration.

The safe path is to first build the frontend and hygiene substrate with no user-defined macros, then expose macro authoring only after the compiler has a stable expansion boundary.

## Non-goals for the OCaml prototype

These are important, but should not block the first working macro implementation:

- polished macro diagnostics beyond what is needed to debug tests;
- final user-facing syntax redesign;
- IDE integration;
- macro optimizer/specializer;
- full Turnstile-style user-defined type systems;
- compile-time package management;
- broad stdlib macro library.

The OCaml compiler is the design prototype. Favor clear invariants and regression coverage over production polish.

## Architectural target

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

For the prototype, preserve the existing post-expansion boundary:

```text
expanded syntax -> Surface.t -> Elaborate.on_expr -> Core.term
```

Only bypass `Surface.t` for typed/core fragments after the surface macro path is working and tested.

## Core design decisions

### 1. Add a pre-surface syntax layer

`Surface.t` is too committed for macros. It has already lost token shape, source spans, concrete delimiters, and lexical binding context. Keep it as the elaborator-facing AST, but introduce a richer macro-facing syntax representation.

Initial modules should live under `lib/syntax/` or a new staged library such as `lib/expand/`:

- `Syntax` — syntax objects, identifiers, spans, syntax datum/tree shape;
- `Scope_set` — compact scope-set representation and operations;
- `Binding` or `Resolve` — scope-set-aware identifier binding and lookup;
- `Raw_syntax` — parser output before macro expansion;
- `Expand` — hygienic expansion from raw/syntax objects to `Surface.t`.

### 2. Use scope-set hygiene from the start

Do not implement temporary textual renaming. It will produce false confidence and make later type-aware macros harder.

Identifier resolution rule:

1. every identifier occurrence has a written name and a scope set;
2. every binding has a written name and a binding scope set;
3. candidates are bindings with the same written name whose scope set is a subset of the identifier's scope set;
4. choose the candidate with the largest scope set;
5. reject no candidate or ambiguous best candidates.

This should be used first for a small desugaring/built-in expander before user macros exist.

### 3. Keep expansion deterministic

Interleaving expansion and typechecking must not make results depend on scheduler order. Any future task graph should satisfy:

- a task writes exactly one expansion/type variable;
- competing writes are an error;
- blocked tasks depend on explicit variables/problems;
- the final result is independent of which unblocked task ran first.

This should be tested once stuck macros exist.

### 4. Phase-aware modules are required before real imported macros

Current imports load `<cwd>/path.fun`, parse the module, cache by path, and detect circular imports by path. Macros require a richer loader state:

- runtime import cache;
- compile-time visit cache;
- phase-indexed active stack for circularity;
- exported binding tables annotated with phase;
- compile-time evaluation of macro definitions before expanding dependent modules.

Do not bolt macro imports onto the current `Core_loader.load` path as ordinary runtime imports.

### 5. Regular syntax comes after hygiene basics

Enforestation is necessary for the final language shape, but it should not be first. First prove that syntax objects, scopes, and built-in expansion can reproduce today's language. Then replace the committed Menhir grammar piece by piece with raw syntax plus enforestation.

## Implementation stages

### Stage 0: Baseline regression lock

Purpose: make sure the macro substrate does not accidentally change the language.

Work:

1. Add focused parser/elaboration/backend tests for current syntax that macros will touch:
   - nested `let`, `type`, `trait`, `impl`, `effect`, `module`, `struct`, `open`, `import`;
   - qualified paths and field access;
   - match branches and patterns;
   - record construction/patterns;
   - implicit binders and trait bounds;
   - effect rows and reference syntax.
2. Keep existing parser entrypoints working:
   - `Core_lexer.parse_expr`;
   - `Core_lexer.parse_module`;
   - `dune exec fun`.
3. Capture pretty-printer or debug snapshots only where existing tests already depend on output shape.

Exit criteria:

- `dune test` passes;
- representative current programs parse/elaborate/evaluate before any macro refactor begins.

### Stage 1: Source spans and raw syntax

Purpose: introduce a syntax representation that can carry macro information while still lowering to existing `Surface.t`.

Work:

1. Add source span types:
   - file/module path;
   - start/end byte offsets;
   - start/end line/column if cheap to track;
   - synthetic span marker for generated syntax.
2. Extend lexing to optionally produce tokens with spans.
3. Add raw syntax data structures:
   - identifiers;
   - literals;
   - delimiter groups;
   - punctuators/operators;
   - keyword tokens;
   - raw forms for module bodies and expression bodies when Menhir still handles the final parse.
4. Keep the old Menhir parser as the compatibility parser.
5. Add tests that tokenization preserves spans and delimiters.

Suggested files:

- `lib/syntax/source_span.ml`
- `lib/syntax/raw_syntax.ml`
- `lib/syntax/syntax.ml`
- `lib/syntax/core_lexer.ml`
- `test/syntax/`

Exit criteria:

- new span/token APIs exist;
- old parser behavior is unchanged;
- no user-visible macro syntax yet.

### Stage 2: Syntax objects and scope sets

Purpose: build the hygiene substrate before any macro authoring API.

Work:

1. Define syntax objects:
   - datum/tree;
   - source span;
   - scope set;
   - phase;
   - optional properties for later expansion metadata.
2. Define identifier operations:
   - written name;
   - scope-set access;
   - add/remove/flip scope;
   - same written name;
   - bound-identifier equality;
   - free-identifier equality once binding tables exist.
3. Implement binding tables and scope-set resolution.
4. Add a small resolver test suite independent of the parser:
   - lexical shadowing chooses largest scope set;
   - macro-introduced names do not capture user names;
   - user names do not capture macro-introduced names;
   - ambiguous best binding is rejected.

Suggested files:

- `lib/syntax/scope_set.ml`
- `lib/expand/binding.ml` or `lib/syntax/binding.ml`
- `test/syntax/test_scope_sets.ml`

Exit criteria:

- hygiene behavior is testable without changing elaboration;
- no textual gensym-renaming is used as the binding model.

### Stage 3: Built-in hygienic expander to existing `Surface.t`

Purpose: create the expansion boundary while preserving current language semantics.

Work:

1. Add an expander context:
   - lexical binding table;
   - current phase;
   - scope allocator;
   - expansion environment for built-in forms;
   - loader handle for future module expansion.
2. Implement built-in expansion for a small expression subset first:
   - variables;
   - atoms;
   - application;
   - lambda;
   - let;
   - annotation;
   - field access.
3. Lower expanded syntax to `Surface.t`.
4. Add parser entrypoints in parallel with old ones:
   - parse using old path;
   - parse raw/syntax path then expand/lower;
   - assert both produce equivalent `Surface.t` for covered forms.
5. Gradually add the rest of current surface forms:
   - records/modules/structs;
   - ADTs/effects/traits/impls;
   - match and patterns;
   - refs;
   - imports/open/export behavior as currently defined.

Suggested files:

- `lib/expand/expand.ml`
- `lib/expand/lower_surface.ml`
- `lib/expand/expand_ctx.ml`
- `test/syntax/test_expand_compat.ml`

Exit criteria:

- all existing syntax can be parsed through the new expander and lowered to `Surface.t`;
- old parser can be kept temporarily as a fallback, but tests should exercise the new path;
- `dune test` passes.

### Stage 4: Move name introduction into expansion

Purpose: make lexical scope explicit before user macros introduce identifiers.

Work:

1. When expanding binding forms, allocate fresh scopes for introduced binders:
   - lambda parameters;
   - let names;
   - recursive let names;
   - type/constructor names;
   - trait/impl names where applicable;
   - module/struct public fields;
   - pattern binders.
2. Attach scopes to identifier occurrences in bodies.
3. Lower resolved identifiers to the existing `Surface.Var` names only as a compatibility layer.
4. Add a generated stable internal name or binding id if `Surface.t` cannot represent resolved identity enough to preserve hygiene.
5. Decide whether `Surface.Var of string` needs to become `Surface.Var of ident_ref` or whether the lowering layer can safely alpha-rename after scope-set resolution.

Recommendation:

- For the OCaml prototype, alpha-rename during lowering if that is substantially faster.
- Keep the authoritative model as scope sets, not alpha-renamed strings.
- Treat alpha-renaming as a backend encoding of resolved identifiers.

Exit criteria:

- hygienic lexical binding is represented before elaboration;
- tests prove expansion/lowering preserves shadowing and avoids accidental capture.

### Stage 5: Minimal compile-time values and macro definitions

Purpose: expose the first user-defined macros, but only after the built-in expander is stable.

First scope:

```fun
macro name = fun stx -> ...
```

or module-level only:

```fun
pub macro name = fun stx -> ...
```

The exact syntax can remain provisional. The important semantic shape is:

```text
Syntax -> Macro Syntax
```

Work:

1. Add surface/module AST support for macro declarations or keep macro declarations in the raw expansion layer if they should not reach `Surface.t`.
2. Add compile-time value representation for macros:
   - macro closure;
   - macro primitive operations;
   - syntax value;
   - macro result/error.
3. Add a small macro evaluation engine.
   - Prefer reusing the existing evaluator only if phase separation is explicit.
   - Do not let runtime refs/effects leak directly into macro evaluation.
4. Add macro monad primitives:
   - return syntax;
   - raise syntax error;
   - inspect syntax kind;
   - construct identifiers/literals/lists/groups;
   - compare identifiers using hygiene-aware operations.
5. Restrict the first macro invocation shape to a simple prefix form so this stage does not require full enforestation.

Possible first invocation syntax:

```fun
name!(...)
```

or a reserved built-in form:

```fun
macroexpand name (...)
```

Do not treat this syntax choice as final language design.

Exit criteria:

- local macro definitions expand simple syntax;
- generated identifiers are hygienic by default;
- macro errors report a useful source span;
- no type-aware macro behavior yet.

### Stage 6: Phase-aware module loading

Purpose: make imported macros correct without conflating runtime imports and compile-time visits.

Work:

1. Replace `Core_loader`'s single parsed-module cache with phase-aware states:
   - parsed raw module cache;
   - expanded module cache per phase/context as needed;
   - elaborated runtime module cache;
   - visited compile-time module cache.
2. Distinguish import modes:
   - runtime import for values/types available at runtime/elaboration;
   - compile-time import/visit for macro bindings;
   - future phase shifts for macros that define macros.
3. Annotate exported module members with phase:
   - runtime value/type/trait/effect/impl;
   - compile-time macro;
   - both only if explicitly supported.
4. Update circular import detection to include phase.
5. Add tests:
   - imported macro expands in another file;
   - compile-time-only dependency is not exposed as runtime value;
   - runtime circular imports and compile-time circular visits are reported separately;
   - macro-generated imports do not bypass phase checks.

Suggested files:

- `lib/loader/core_loader.ml`
- `lib/expand/module_expand.ml`
- `test/semantic/` or new `test/expand/`

Exit criteria:

- imported macros work;
- phase errors are deterministic;
- existing runtime imports still behave as before.

### Stage 7: Enforestation and regular syntax extension

Purpose: support non-Lisp syntax extension and use the enforestation boundary as the right time to apply the planned surface-syntax redesign, instead of redesigning the parser before the macro substrate exists.

This is the first stage where final user-facing syntax should be seriously changed. Earlier stages should preserve today's syntax as a compatibility target so hygiene, phases, and macro expansion can be validated independently. Once enforestation owns grouping, precedence, and syntax classes, the surface redesign can be implemented as a new set of built-in syntax forms/operators rather than another round of Menhir grammar growth.

Work:

1. Add an enforestation pass over token/syntax streams.
2. Represent syntax classes:
   - expression;
   - type;
   - pattern;
   - declaration/module item;
   - block/body;
   - custom syntax classes later.
3. Represent operator declarations:
   - fixity;
   - precedence;
   - associativity;
   - expansion function.
4. Move existing operator parsing into enforestation tables:
   - application;
   - field access;
   - prefix forms such as `ref`, `!`, `perform`, `resume`;
   - infix arithmetic/comparison/assignment;
   - arrows/effect rows where appropriate.
5. Introduce the planned surface-syntax redesign through enforestation tables and built-in syntax classes.
   - Keep the old syntax accepted during the transition when practical, but treat the redesigned syntax as the direction to test and document.
   - Prefer implementing redesign features as ordinary built-in macro/enforestation entries so user macros exercise the same path.
   - Use this stage for larger block/form experiments such as Ruby/Elixir-style `do ... end` shapes if they are still desired.
6. Keep Menhir for delimiter/token recovery if useful, but stop growing it as the final surface grammar.
7. Add tests for macro-defined operators, redesigned built-in syntax, and precedence interactions.

Exit criteria:

- the redesigned surface syntax has an initial implemented slice through enforestation;
- at least one user-defined prefix form and one user-defined infix operator work;
- existing operator precedence either remains compatible or has an intentional migration test;
- syntax extension composes with module/import scoping.

### Stage 8: Problem-aware macros

Purpose: let a macro know what syntactic judgment it is expanding for.

Problem kinds:

- expression inference;
- expression checking against an expected type;
- type expression;
- pattern;
- declaration/module item;
- effect row if it becomes useful;
- trait/impl item if needed.

Work:

1. Thread expansion problem information through the expander.
2. Add macro API to inspect the current problem.
3. Allow the same macro binding to expand differently by problem kind.
4. Integrate with current bidirectional elaboration boundaries:
   - `infer` can request expression expansion with no expected type;
   - `check` can request expression expansion with an expected type;
   - pattern elaboration can request pattern expansion;
   - module elaboration can request declaration expansion.
5. Add tests modeled after Klister's `which-problem` examples.

Exit criteria:

- a macro can distinguish type/expression/pattern/declaration contexts;
- a checked-expression macro can see the expected type;
- expansion remains deterministic.

### Stage 9: Type-aware macros without stuck expansion

Purpose: expose useful type information before building the full blocked-task scheduler.

Work:

1. Let expression macros in checking mode inspect the expected semantic type.
2. Let macros inspect stable reflected type heads:
   - primitive types;
   - nominal type constructors and arguments;
   - structural record fields where current type-case already supports reflection.
3. Keep this read-only and non-stuck:
   - if the expected type is unknown/unsolved, the macro gets `unknown` or fails with a clear error;
   - do not suspend/resume yet.
4. Add tests:
   - type-directed defaulting;
   - type-directed wrapper generation;
   - record-field-based code generation;
   - graceful failure when expected type is unavailable.

Exit criteria:

- useful type-aware macros exist;
- no scheduler/interleaving machinery is required yet;
- reflection API aligns with existing type-case design.

### Stage 10: Expansion variables and stuck macro scheduler

Purpose: support Klister-style interleaving where expansion and typechecking can wait on each other.

Work:

1. Add expansion variables analogous to metavariables.
2. Add task records:
   - task id;
   - problem kind;
   - inputs/dependencies;
   - output variable;
   - continuation/action.
3. Add scheduler:
   - run unblocked tasks;
   - mark tasks blocked on type metas or expansion variables;
   - detect no-progress states;
   - produce dependency-cycle diagnostics.
4. Expose elaboration operations as tasks where needed:
   - infer type of expanded expression;
   - check expression against expected type;
   - solve unification constraints;
   - resume blocked macro once reflected type is known.
5. Keep scheduler deterministic by construction.
6. Add tests:
   - macro waits for expected type;
   - typechecker waits for macro expansion;
   - independent tasks produce same result regardless of order;
   - deadlock/cycle reports waiting problems.

Suggested files:

- `lib/expand/expansion_var.ml`
- `lib/expand/task_graph.ml`
- `lib/semantic/typecheck/elaborate.ml`
- `lib/semantic/typecheck/unify.ml`

Exit criteria:

- stuck macros work for small examples;
- dependency failures are understandable enough for prototype work;
- existing non-macro elaboration remains unchanged.

### Stage 11: Type-providing macros and typed expansion fragments

Purpose: allow macros to provide enough type information for surrounding elaboration before their full expansion is known.

Work:

1. Extend macro result forms:
   - expanded syntax only;
   - provided type plus delayed expansion;
   - checked typed fragment;
   - core term plus semantic type, if needed.
2. Define which fragments may bypass `Surface.t`.
3. Preserve hygiene for fragments that introduce binders or references to lexical names.
4. Integrate with metavariable solving:
   - provided types may contain metas;
   - delayed expansion may depend on solved metas;
   - unification failures should point back to macro source spans.
5. Add tests:
   - macro provides a type so surrounding application/checking proceeds;
   - macro expansion later agrees with provided type;
   - disagreement is rejected;
   - macro-generated binders remain hygienic.

Exit criteria:

- type-providing macros work in at least one realistic example;
- typed/core fragments have a clear hygiene story;
- this stage does not require replacing the whole elaborator.

### Stage 12: Macro-powered language features

Purpose: validate the system by moving suitable compiler features into macros or macro-like libraries.

Candidate validations:

1. derived helpers using type-case/reflection;
2. record/ADT deriving scaffolding;
3. DSL block syntax;
4. custom pattern forms;
5. protocol-style boilerplate over traits;
6. future parser syntax experiments such as Ruby/Elixir-style `do ... end` blocks.

Exit criteria:

- at least one feature that would otherwise require parser/compiler changes can be implemented as a macro;
- the macro implementation is simpler than adding bespoke compiler machinery;
- shortcomings feed back into the macro API before the CLR/C# rewrite.

## Validation strategy

Run the normal validation loop throughout:

```sh
dune build
dune test
```

Add targeted suites as the system grows:

- syntax object and scope-set unit tests;
- expander compatibility tests against current parser output;
- hygiene tests;
- macro expansion tests;
- phase/import tests;
- type-aware macro tests;
- stuck scheduler tests.

Representative single-suite commands should follow existing project conventions, for example:

```sh
dune exec test/syntax/test_syntax.exe
dune exec test/semantic/test_elaborate.exe
```

If a new test executable is added for expansion, document the command in `CLAUDE.md` or this directory once it exists.

## Dead-end avoidance checklist

Avoid these shortcuts:

- implementing macros as string substitution;
- implementing hygiene only as gensym renaming;
- adding user macros directly on `Surface.t` as the only macro representation;
- expanding Menhir with final-syntax experiments before enforestation exists;
- treating compile-time imports as ordinary runtime imports;
- letting macro evaluation freely run runtime effects/refs;
- making type-aware macros require a strict expansion-before-elaboration phase order;
- exposing unstable OCaml internals as the long-term macro API;
- adding broad diagnostic polish before the architecture is settled.

Acceptable temporary shortcuts:

- lower resolved hygienic identifiers to alpha-renamed `Surface.Var` strings while the elaborator still expects strings;
- keep Menhir as a compatibility parser while raw syntax/enforestation is introduced;
- start with prefix-only macro invocation before general regular-syntax macros;
- expose only read-only expected-type reflection before stuck macros exist;
- keep typed/core macro fragments out of scope until surface-expanding macros are stable.

## Recommended near-term milestone

The first implementation milestone should be:

> A syntax-object and scope-set layer plus a built-in hygienic expander that reproduces today's language and lowers to `Surface.t`, with no user-defined macros yet.

This milestone is large enough to validate the architecture but small enough to avoid entangling macro evaluation, phases, and dependent elaboration all at once.

Concrete first PR/commit sequence:

1. [x] Add source spans and token-with-span support behind new APIs.
2. [x] Add `Syntax` and `Scope_set` modules with unit tests.
3. [x] Add scope-set binding resolution tests.
4. [x] Add an expander context and lower a tiny expression subset to `Surface.t`.
5. [x] Add compatibility tests comparing old parser output with new expansion output for that subset.
6. [x] Extend the built-in expander form-by-form until it covers current `Surface.t`.
7. [ ] Switch tests or parser entrypoints to exercise the new path once coverage is equivalent.

Implemented this round:

- `Source_span`, `Raw_syntax`, `Scope_set`, and `Syntax` modules in `lib/syntax/`.
- `Core_lexer.tokens_with_spans` and `Core_lexer.spanned_token`, with existing parser entrypoints preserved.
- `core_tt_expand` library with binding resolution, expander context, built-in expander, lowering to `Surface.t`, and a `Surface.t -> Syntax.t` compatibility adapter.
- Single `test/syntax/test_syntax.exe` Alcotest executable with parser suites split by topic, plus span, scope-set, binding-resolution, and expand/lower compatibility tests.
- Updated syntax test command references from `test_match_parse.exe` to `test_syntax.exe`.

Only after that should user macro syntax be added.
