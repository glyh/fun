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

This gives the language a single implementation path for dependent typing, implicit arguments, nominal ADTs, structural records/modules, pattern matching, imports, and algebraic effects.

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

Direct matching on `Type` values enables generic programming, type-driven dispatch, and comptime-style reflection. Primitive type-head patterns already provide the starting point; the next useful work is expanding this toward nominal and record type analysis.

Likely milestones:

- nominal type-head matching;
- record type reflection;
- predictable behavior for open `Type` matching;
- tests for generic functions that branch on primitive, nominal, and record type structure.

This is a real prototype goal because it exercises the dependent core and the language's intended flexibility without requiring the macro system first.

### 3. Typeclasses / protocols / ad-hoc polymorphism

Resolve long-term equality and ad-hoc polymorphism for nominal and record values. The first design target should be an explicit protocol/typeclass-like mechanism that fits the existing dependent core, implicit arguments, and type-case direction rather than copying Haskell-style typeclasses wholesale.

Likely milestones:

- decide whether this is called typeclasses, protocols, instances, or another language-specific term;
- define equality for nominal values without defaulting to accidental structural equality;
- define equality or derivation behavior for record values;
- decide how instance/protocol lookup interacts with implicit arguments;
- decide whether type-case can implement part of the mechanism directly;
- add tests for equality and other small protocol-style operations over primitive, nominal, and record types.

This should come after initial type-case work because type-directed dispatch is the foundation for the simplest version of the feature.

### 4. Algebraic effects hardening

Algebraic effects now have nominal effect families, closed latent rows, `perform`, match-based handlers, and `resume`. The remaining work should focus on semantic hardening before larger type-system extensions.

Near-term effect work lives in `docs/7.algebraic_effects_plan.md` and is ordered as:

- handler semantics hardening;
- open effect rows / effect polymorphism.

Optimization, VM, and CPS lowering are not part of the current effects plan.

### 5. References

Mutable ref cells should capture the cell, not the current value, inside closures. They will affect closure conversion and runtime value representation, so they should wait until the current evaluator and core behavior are better covered by tests.

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
