# Roadmap

## Design philosophy

Consistency > Flexibility > Correctness.

- **Consistency**: one construct for many roles (`struct` = record/module/namespace), types are values, and the core type system should carry the language model directly.
- **Flexibility**: willing to trade theoretical properties such as parametricity for practical power. Type-case on open `Type` is acceptable.
- **Correctness**: still valued through bidirectional checking, normalization, unification, and regression tests, but not at the expense of the language's practical shape.

## Current foundation

The old HM/typecheck/interpreter pipeline has been replaced by `core_tt`:

```text
Surface.t → elaboration → Core.term + semantic type → NbE evaluation
```

This gives the language a single implementation path for dependent typing, implicit arguments, nominal ADTs, structural records/modules, pattern matching, imports, traits, algebraic effects, and mutable references.

## Ordered work

### 1. Regression coverage for current behavior

Add more current-behavior regression tests around features that already exist and should not regress while larger prototype work continues.

Priority areas:

- imports and module files;
- records and record patterns;
- methods, `self`, and `Self`;
- qualified patterns and qualified constructors;
- algebraic effect declarations, `perform`, handlers, and `resume`.

This is the lowest-risk near-term work because it preserves known behavior without forcing new surface syntax or architecture decisions.

### 2. Type-case / generic programming

Direct matching on `Type` values enables generic programming, type-driven dispatch, and comptime-style reflection. The current prototype supports primitive and nominal type-head matching, structural record type reflection by field type, predictable open `Type` fallback behavior, and generic helper examples for equality dispatch, fallback defaulting, and classification.

Completed milestones:

- primitive type-head matching with branch-local dependent refinement;
- predictable behavior for open `Type` matching via required fallback branches;
- nominal type-head matching, including applied ADTs and parameter subpatterns;
- record type reflection through `struct ... end` type patterns over constructor fields;
- tests for generic functions that branch on primitive, nominal, and structural record type shape.

This is a real prototype goal because it exercises the dependent core and the language's intended flexibility without requiring the macro system first.

### 3. Traits / ad-hoc polymorphism

Nominal `trait` declarations are the chosen mechanism for ad-hoc polymorphism. The current prototype adds local and module-level trait declarations, `impl` declarations, structural dictionary evidence, trait-bound implicit parameters such as `{A : Eq}` and `{A : Eq + Jsonable}`, qualified method calls such as `Eq.eq x y`, and tests for direct and generic dispatch.

Completed milestones:

- chose the feature name and surface shape: `trait`, `impl`, and qualified trait method calls;
- added parser and AST support for trait/impl declarations in expressions, modules, and structs;
- added hidden dictionary binders for trait-bound implicit function types;
- added local implementation lookup, dictionary passing, and ambiguity detection;
- made trait declarations, dictionary types, and module impl entries structural rather than generated-symbol based;
- added public module impl/trait evidence through `open`/imports while preserving module binding order;
- kept nominal ADT equality explicit: ADTs do not derive `Eq` automatically;
- kept record/struct `Eq` explicit by default: records reject `==` unless suitable evidence exists;
- added ordered struct impl evidence, including `pub impl Eq Self` inside a struct body;
- added imported struct-local impl coverage, so an exported struct can carry public `Self` evidence across module imports;
- added semantic and backend tests for trait declaration, implementation checking, method dispatch, bound dispatch, missing impls, duplicate fields, missing fields, explicit record equality rejection, struct-local `Self` impls, and imported struct-local `Self` impls.

Remaining milestones:

- implement explicit deriving/fallback behavior as library-level type-case code where possible, rather than compiler magic;
- add more protocol-style operations over primitive, nominal, and record types.

The near-term design details live in `docs/11.trait_plan.md`.

### 4. Algebraic effects hardening

Algebraic effects now have nominal effect families, closed latent rows, `perform`, match-based handlers, and `resume`. Handler semantics are hardened for closed rows: handlers are deep, value branches and branch bodies run through the active handler loop, parameterized effect instances are distinguished during branch coverage and dispatch, lexical `resume` is allowed inside nested branch-local lambdas, and continuations remain one-shot.

The remaining near-term effect work in `docs/7.algebraic_effects_plan.md` is open effect rows / effect polymorphism.

Optimization, VM, and CPS lowering are not part of the current effects plan.

### 5. References

Mutable references are implemented in the OCaml prototype. The language now supports `Ref A`, `ref e`, `!r`, and `r := e` without exposing reference operations in user-visible effect rows.

Completed milestones:

- added parser, AST, core, elaboration, NbE, unification, and debug-printer support for reference types and operations;
- represented runtime references as opaque mutable cells;
- preserved aliasing and closure-capture semantics so closures capture the cell, not a snapshot of the current value;
- prevented reference allocation/read/write from being pre-evaluated during elaboration through the internal compile-time-safety check;
- added syntax, semantic, and backend regression coverage for parsing, typing, rejection cases, aliasing, mutation, and closure-observed writes.

### 6. UFCS

Uniform function call syntax remains desirable once method and field access semantics settle. This is smaller than macros, but it should wait until existing method semantics are well covered and the surface syntax direction is clearer.

### 7. Native bindings

FFI support should allow `fun` code to call native functionality without compromising the core evaluator's invariants. This is useful, but it is not a prototype priority until the language core stabilizes further.

### 8. Macro system

User-defined syntax extensions are still open design territory and should wait until the core language behavior is better understood. The macro plan is much larger than the effects plan because it requires syntax objects, hygiene, enforestation, phase-aware modules, and eventually type-integrated expansion.

Parser syntax redesign is blocked on this work: the parser should not be expanded aggressively toward final user-facing syntax before the macro system direction is clear.

### 9. CLR/C# rewrite target

A future rewrite should target the CLR and be implemented in C#. This is now preferred over a Zig rewrite because the CLR provides major runtime pieces for free: GC, JIT, mature tooling, packaging, debugging/profiling support, and a practical native interop story.

The current OCaml implementation remains the design prototype. The rewrite should wait until the dependent core, implicit resolution story, type-case direction, effect semantics, and macro direction are better understood.

Likely rewrite goals:

- keep the core language model from the OCaml prototype;
- use C# for the compiler/runtime implementation;
- target CLR execution rather than building a custom VM first;
- preserve room for algebraic effects through CPS/trampolining/runtime encoding if needed;
- defer larger surface syntax redesign until the rewrite, including possible Ruby/Elixir-style `do ... end` blocks and less ML-flavored syntax.

### 10. Post-rewrite diagnostics polish

Diagnostics for parse, elaboration, unification, exhaustiveness, macro expansion, and runtime errors should be polished after the CLR/C# rewrite, when the implementation shape is less likely to be thrown away.

Before the rewrite, improve diagnostics only when needed to unblock feature work or tests. Avoid broad diagnostics cleanup in the OCaml prototype because that code is expected to be replaced.
