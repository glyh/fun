# Roadmap

## Design philosophy

Consistency > Flexibility > Correctness.

- **Consistency**: one construct for many roles (`struct` = record/module/namespace), types are values, and the core type system should carry the language model directly.
- **Flexibility**: willing to trade theoretical properties such as parametricity for practical power. Type-case on open `Type` is acceptable.
- **Correctness**: still valued through bidirectional checking, normalization, unification, and regression tests, but not at the expense of the language's practical shape.

## Current foundation

The old HM/typecheck/interpreter pipeline has been replaced by `core_tt`:

```
Surface.t → elaboration → Core.term + semantic type → NbE evaluation
```

This gives the language a single implementation path for dependent typing, implicit arguments, nominal ADTs, structural records/modules, pattern matching, and imports.

## Near-term work

- Improve diagnostics for parse, elaboration, unification, and exhaustiveness errors.
- Expand parser syntax from the test-oriented core grammar toward the final user-facing syntax.
- Add more current-behavior regression tests around imports, module files, records, methods, and qualified patterns.
- Decide how much type-level computation and type-case should be exposed in ordinary user code before introducing larger features.

## Type-case / generic programming

Direct matching on `Type` values enables generic programming, type-driven dispatch, and comptime-style reflection. Primitive type-head patterns already provide the starting point; future work can expand this toward nominal and record type analysis.

## Algebraic effects

Effects need function-type effect annotations and a runtime model for continuations. Implementing them on a custom VM remains the preferred path because the goal is to learn and control the continuation machinery rather than delegating to OCaml effects.

## VM backend

A later backend can lower the core language through a continuation-aware intermediate representation into bytecode for a register-based VM.

```
Core.term → CPS/closure conversion → Bytecode → VM
```

- **Value representation**: tagged pointers initially; NaN-boxing can wait.
- **Closures**: flat closures are sufficient until mutable captures require more.
- **GC**: start with a simple bump allocator plus stop-the-world collection, or leak memory while prototyping the bytecode pipeline.

CPS remains attractive because it naturally exposes continuations, which the effect system will need.

## References

Mutable ref cells should capture the cell, not the current value, inside closures. They will affect closure conversion and runtime value representation.

## UFCS

Uniform function call syntax remains desirable once method and field access semantics settle.

## Native bindings

FFI support should allow `fun` code to call native functionality without compromising the core evaluator's invariants.

## Macros

User-defined syntax extensions are still open design territory and should wait until the surface syntax stabilizes.

## Zig rewrite + syntax redesign

A future rewrite in Zig may make sense once the dependent core, implicit resolution story, and VM prototype are stable. That would also be the right time for a larger surface syntax redesign toward Ruby/Elixir-style `do ... end` blocks and less ML-flavored syntax.
