# Macro system summary

## Goal

The planned macro system should be a first-class, hygienic, type-integrated extension mechanism for `fun`. It should not make the language Lisp-shaped. Instead, it should preserve regular surface syntax while giving users the same kind of extensibility that Racket gets from syntax objects, phases, hygienic expansion, and compile-time computation.

The important design direction is not merely "macros rewrite syntax before typechecking." The interesting goal is a macro/typechecker system where macro expansion and type elaboration can communicate, suspend, resume, and contribute information to each other in a predictable way.

## Source influences

The material under this directory points at four complementary ideas:

- **Klister** (`extracted/klister/`) — type-aware, type-providing, problem-aware, hygienic macros with stuck expansion and phase-aware modules.
- **Binding as Sets of Scopes** (`papers/hygiene/binding-as-sets-of-scopes.md`, `extracted/klister/commentary/scope-sets.md`) — Racket-style hygiene via syntax objects annotated with scope sets rather than name rewriting.
- **Honu** (`papers/regular-syntax/honu-enforestation.pdf`) — macro extensibility for regular/algebraic notation using enforestation, operators, syntax classes, and pattern-directed parsing, not just parenthesized s-expressions.
- **Macros for DSLs / Type Systems as Macros** (`papers/dsl-extensibility/macros-for-domain-specific-languages.md`, `papers/type-integrated-macros/type-systems-as-macros.md`) — extensible DSLs should inherit the host macro system, and type systems can be implemented as cooperating macros over a shared core.

After a closer read, the TURNSTILE paper and Klister should be used for different parts of the design:

- TURNSTILE is the better source for the rule-level interface: expansion computes both an erased term and type information, checking rules consume expected types, type operations such as equality/subtyping are overridable hooks, and type-directed rewrites can implement features such as local inference, ADTs, pattern matching, and typeclass-style dictionary insertion.
- Klister is the better source for the operational substrate: problem-aware macro APIs, expansion variables, type metavariables, blocked macro continuations, task scheduling, and deterministic stuck-macro behavior.
- The practical `fun` path should keep Stage 8 problem awareness separate from Stage 9 read-only expected-type reflection, then use the Stage 10 scheduler only for examples that truly require mutually recursive expansion and elaboration.
- Type-providing macros should not be treated as ordinary `Syntax -> Syntax` rewrites with a side channel; they need an explicit result form that records the provided type, the delayed expansion or typed fragment, and the consistency obligation checked later.

## Core principles

### 1. Syntax objects, not plain AST nodes

The frontend needs a representation of syntax that preserves:

- source locations,
- original token/tree shape,
- lexical scope information,
- phase information,
- enough concrete structure for macro pattern matching and error reporting.

This should be the macro-facing representation. `Surface.t` can remain the post-expansion language consumed by elaboration, but macros need an earlier and richer `Syntax` layer.

### 2. Hygiene by scope sets

Use Racket-style scope sets rather than textual renaming.

Each identifier occurrence carries a set of scopes. Bindings are recorded with a name and a scope set. Resolving an identifier means:

1. find bindings with the same written name whose required scope set is a subset of the identifier's scope set;
2. choose the binding with the largest scope set;
3. reject if there is no unique best binding.

This supports ordinary lexical shadowing, macro-introduced bindings, nested macro expansion, quoted syntax, and intentional hygiene-breaking operations in a uniform model.

Generated identifiers should not accidentally capture or be captured. Intentional capture should require explicit operations analogous to Racket/Klister facilities such as comparing identifiers, closing syntax, or constructing syntax in a chosen lexical context.

### 3. Regular syntax via enforestation

The macro system should respect `fun`'s normal syntax rather than forcing all extension points into Lisp syntax.

Honu's model is the most relevant reference: parse raw tokens into a less-committed syntax stream/tree, then run an **enforestation** pass that uses the current macro/operator environment to group tokens into syntax objects. This allows macros to define:

- new prefix/infix forms,
- operators with precedence and associativity,
- syntax classes for macro patterns,
- block forms and domain-specific notation,
- DSL sublanguages that still compose with the host language.

For `fun`, this suggests the parser should eventually split into at least two layers:

```text
source text
  -> tokens / raw concrete syntax
  -> enforestation using syntax/operator environment
  -> hygienic macro expansion
  -> Surface.t
  -> elaboration
  -> Core.term
```

### 4. Macros are first-class compile-time values

Macros should fit the language model instead of being an external compiler plugin API.

A baseline shape from Klister is:

```fun
Syntax -> Macro Syntax
```

But for `fun`, the long-term form should probably be richer because macros should know what kind of expansion is being requested and may produce typed information. Useful first-class compile-time concepts include:

- `Syntax` — hygienic syntax objects,
- `Macro A` — compile-time computation with controlled effects,
- `Problem` — the judgment being solved: expression, type, pattern, declaration, module item, etc.,
- reflected type/core values for type-directed expansion,
- operations for identifier comparison and syntax construction.

The macro monad should be separate from runtime effects. Compile-time effects include expansion, syntax errors, identifier comparison, asking for type information, waiting on unresolved information, and module/phase operations.

### 5. Problem-aware expansion

A macro should be able to ask what problem it is solving. Klister's `which-problem` examples are directly relevant.

The same macro name could expand differently when used as:

- a declaration,
- a type,
- an expression with an expected type,
- a pattern,
- a module item.

This matches `fun` well because the existing compiler is already bidirectional: checking an expression against an expected type is different from inferring a type. A macro in expression position should be able to see the expected type when one exists.

Example behavior from Klister:

```text
macro m:
  declaration  -> generate a declaration
  type         -> generate Bool
  expression T -> generate a term of expected type T
  pattern      -> generate a pattern
```

### 6. Type-aware and type-providing macros

The major feature is that macros should participate in the type system.

Two directions matter:

- **Type-aware macros**: a macro can inspect the expected type or already-inferred types around it and generate code accordingly.
- **Type-providing macros**: a macro can tell the typechecker what type its eventual expansion will have, allowing surrounding elaboration to proceed before expansion is fully known.

This enables macros that communicate via types, generate code from type structure, implement implicit conversions, derive operations, or encode custom type systems/languages on top of the host core.

This is much more invasive than ordinary macro expansion because expansion and elaboration can no longer be strictly sequential.

## Stuck macros and interleaving

The key Klister idea is that macro expansion and typechecking should be interleaved through a task system.

A type-driven macro may need the typechecker to solve some type before it can expand. But the typechecker may need some expansion result before it can continue. A fixed phase order cannot handle this.

Instead, model compilation as a set of partially solved problems:

- expansion variables: unknown syntax/core expansions,
- type/metavariables: unknown types or terms,
- tasks that can fill those variables,
- dependencies saying a task is blocked until some variable is solved enough.

Then run all unblocked tasks until either:

- all expansion/type problems are solved, or
- every remaining task is blocked, which is an error or ambiguity.

The existing `fun` elaborator already has metavariables and bidirectional checking, so this can potentially build on the current `MetaContext` idea. The new part would be expansion variables and a scheduler shared by expansion and elaboration.

## Module and phase implications

A serious macro system needs a phase-aware module system.

Compile-time dependencies should not become runtime dependencies. Imports need to say whether they are used at runtime, compile time, or shifted phases. Klister uses phase-shifted imports such as importing a module at phase 1 so its macros are available while expanding the current module.

For `fun`, this affects `Core_loader` and module elaboration:

- module files need separate expansion, elaboration, and evaluation caches;
- macro definitions must be evaluated at compile time;
- exported bindings need phase information;
- expansion of one module may require visiting another module at a shifted phase;
- cyclic imports need to account for phase, not just file path.

## Compiler structure impact

This feature is structurally large. A plausible future compiler pipeline is:

```text
source
  -> tokens / raw syntax
  -> Syntax objects with source spans and scope sets
  -> enforestation using current syntax/operator bindings
  -> expansion/elaboration task graph
       - hygienic macro expansion tasks
       - type elaboration tasks
       - unification tasks
       - blocked/stuck macro continuations
  -> Surface.t or directly Core.term
  -> Core.term + semantic type
  -> NbE / backend
```

The current staged source layout maps well to this, but new layers would likely be needed:

- `lib/syntax/` grows raw syntax, syntax objects, syntax classes, and enforestation.
- A new expansion stage owns scope sets, binding resolution, phases, macro environments, and expansion tasks.
- `lib/semantic/typecheck/` must expose enough incremental elaboration/unification machinery for macros to wait on or provide type information.
- `lib/loader/` must become phase-aware.
- `Surface.t` may become either the post-expansion AST or be bypassed for some typed macro expansions that directly produce core.

## Design fit with `fun`

This macro direction fits the language's current philosophy:

- `struct ... end` already unifies records, modules, and namespaces; macros can eventually use the same module system rather than a separate plugin registry.
- Types are values, so type-aware macros are conceptually natural.
- Bidirectional checking already gives contextual type information, which is exactly what expression macros want.
- Metavariable solving already exists, making stuck macros more plausible than in a simple batch typechecker.
- The language accepts pragmatic power over strict theoretical purity, which aligns with type-case and type-directed macros.

## Major open questions

1. **What is the macro-facing syntax representation?**
   It must preserve regular syntax and source spans, not just encode everything as s-expressions.

2. **Do macros expand to `Surface.t`, `Core.term`, or typed elaboration fragments?**
   Expanding to surface syntax is simpler. Expanding to typed/core fragments is more powerful and better for type-providing macros, but harder to keep hygienic and stable.

3. **How first-class are macros?**
   If macros are ordinary values at compile time, the compiler needs phase-separated evaluation and probably a compile-time standard library.

4. **How much type reflection is exposed?**
   Existing type-case over primitive type heads is a start. Full nominal/record reflection requires stable representations of type structure.

5. **What are the controlled hygiene-breaking operations?**
   Hygiene should be default, but a practical macro system needs explicit tools for intentional capture, identifier comparison, and generated bindings.

6. **How are stuck macro failures reported?**
   If expansion/typechecking deadlocks because tasks are mutually blocked, diagnostics need to explain which syntax and type problems are waiting on each other.

7. **Can expansion remain deterministic?**
   Klister's design goal is that expansion results do not depend on task scheduling order. `fun` should preserve that property if expansion and elaboration interleave.

## Suggested implementation path

A reasonable staged path is:

1. Introduce `Syntax` objects with source spans and scope sets, without user macros yet.
2. Move name resolution for parsed identifiers toward scope-set-aware binding resolution.
3. Add a small hygienic expansion pass for built-in forms only.
4. Add compile-time macro values with `Syntax -> Macro Syntax` and a minimal macro monad.
5. Add phase-aware imports for compile-time macro definitions.
6. Add regular-syntax extension/enforestation for operators and syntax classes.
7. Integrate expansion with bidirectional elaboration using expansion variables and blocked tasks.
8. Add problem-aware macros.
9. Add type-aware and type-providing macros.
10. Extend type reflection to nominal and record type structure.

The safest first milestone is not user macros. It is a hygienic syntax-object layer plus a built-in expander that still produces the same `Surface.t` as today. That creates the foundation without immediately entangling macro evaluation with dependent elaboration.
