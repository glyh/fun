# Roadmap

## Design Philosophy

Consistency > Flexibility > Correctness.

- **Consistency**: one construct for many roles (struct = record/ADT/module/namespace), types are values (no separate type language), DT to unify modules into dependent records.
- **Flexibility**: willing to sacrifice theoretical properties (parametricity, free theorems) for practical power. Type-case on open `Type` is acceptable. Engineering lang, not proof lang.
- **Correctness**: still valued (real type system, bidirectional checking, unification) but serves the other two rather than constraining them. Runtime asserts over heavy type-level encoding when the gain is marginal.

## Dependent Types

Most disruptive change — collapses the type/value boundary. Requires bidirectional
type checking, normalization-by-evaluation, and a universe hierarchy. Should be done
before other type system features since it reshapes the entire typechecker.

Type classes are not needed as a separate feature. Implicit argument resolution on
top of DT covers the same use case (implicit dispatch via scoped instance search).

## Algebraic Effects

Needs row-polymorphic effect annotations on function types (`Arrow(a, eff, b)`).
Implement via own VM rather than OCaml's effect system — the point is to learn the
continuation machinery.

## Type-Case / Pattern Matching on Types

Direct matching on `Type` values — enables generic programming, type-driven dispatch,
comptime-style reflection (like Zig). Parametricity is explicitly traded for flexibility.

## VM Backend

Register-based VM with CPS intermediate representation.

```
Typed_ir → CPS → Closure conversion → Bytecode → VM
```

- **Value representation**: tagged pointers to start, NaN-boxing as a later optimization.
- **Closures**: flat (copy free variables at creation). No shared environments needed
  without mutable captures.
- **GC**: simple bump allocator + stop-the-world, or just leak memory initially to
  focus on the compilation pipeline.

CPS is chosen over ANF because it naturally exposes continuations, which the effect
system will need later.

## References

Mutable ref cells. Affects closure representation — closures capture the ref, not
the value inside it.

## UFCS

Uniform function call syntax.

## Native Bindings

FFI for calling into native code from `fun`.

## Macros

User-defined syntax extensions. Design TBD.

## Zig Rewrite + Syntax Redesign

Rewrite the compiler in Zig once the type system design (DT + implicit resolution)
and VM prototype are stable. Pair with a complete surface syntax redesign toward
Ruby/Elixir style — `do...end` blocks, less ML-flavored. Good time to do both
together since the new parser only gets built once.
