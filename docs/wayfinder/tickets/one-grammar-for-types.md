---
title: Annotation types use a separate grammar that ignores user operators
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Annotation types use a separate grammar that ignores user operators

Found by the bare-arrow-is-pure run (2026-09-15).

Types are values, but a type written in a binding or parameter annotation
(`x : T = …`, `fn(g : T)`) is read by a separate type grammar that hardcodes
`->`, `+` and `*` and reads no user roles. So the prelude's `~>`
(`pub infix (~>) arrow ($a, $b) { $a -> $b can _ }`) works where the expression
grammar reads the type and fails in annotations:

```fun
Callback = Unit ~> I64;                   // works (expression grammar)
wrap = fn(g : Unit ~> I64) { g(()) }      // parse error (type grammar)
```

Direction: read annotation types with the expression grammar (one grammar,
Consistency > Flexibility); `->` and `can` become expression-level forms with
their order groups. Check the "after `->`" and `Tight` built-in positions the
order-groups run kept.

## Grilled (2026-09-15): tuple types are `Tuple(n, …)`, `*` is only multiplication

`A * B * C` cannot mean the flat `(x, y, z)` type as a binary operator (it reads
`((x, y), z)`), and `(I64, Bool)` as a type would give one syntax two meanings (a
pair of types vs the pair type — the same problem `sig` vs `module` had). So:

- **`*` is only multiplication.** No product-type reading anywhere.
- **Tuple types are written `Tuple(n, T1, …, Tn)`**, a built-in whose type is an
  ordinary dependent function computing its own arity (the printf trick):
  ```fun
  Arrows = fn(k : I64) : Type { if (k == 0) { Type } else { Type -> Arrows(k - 1) } };
  Tuple : (n : I64) -> Arrows(n)
  p : Tuple(3, I64, Bool, String) = (1, True, "a")   // p.2 : String
  ```
- **`n` is explicit** (the checker needs `Tuple`'s type before its arguments).
  A negative `n` is an error (checked, not a separate natural-number type).
- `(I64, Bool)` is only ever a pair value.
- Migrate every `A * B` type to `Tuple(2, A, B)`; then annotations read with the
  expression grammar and the separate type grammar is deleted.
- The `Eq + Show` bound sugar (recognised by the spelling `+`) is resolved the same
  way: give bounds a non-overloaded form or a trait-resolved `+` — not decided;
  keep it out of this change unless it blocks deleting the type grammar (then stop
  and ask).
