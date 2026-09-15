---
title: Annotation types use a separate grammar that ignores user operators
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented. Tuple(n, T1, …, Tn) is a built-in reducing to the flat product type, its arity computed by tuple_arity(n); A * B types migrated (6 sources). parse_type_entry reads with the expression grammar; parse_type_arrow … parse_type_postfix and the parser type-keyword list are deleted. ~> works in every annotation position.
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

## Implemented (2026-09-15)

- `Tuple` is a primitive typed `(n : I64) -> tuple_arity(n)`; `tuple_arity(n)`
  reduces to `Type` at 0 and `Type -> tuple_arity(n - 1)` above, and fails on a
  negative `n`. `Tuple(n, T1..Tn)` reduces to `VProdTy` once all `n` components
  are applied; under-applied it is a type function (`Tuple(2, I64) : Type -> Type`).
- A negative count is an evaluation error that escapes the checker as
  `EvalError` (a `panic` in a type does too) — not wrapped into an elaboration
  error yet.
- `I64 * Bool` as a type fails as an ordinary unification error
  (`CannotUnify(atom type I64 vs Type)`); a message suggesting `Tuple(2, …)`
  would need to recognise `*` by spelling, so it is not given.
- `Syntax.ProdTy` is no longer produced by the parser (still reflected).
- The `Eq + Show` bound sugar did not block: `+` parses to the same application
  in both grammars.
- Not in scope, still unsupported: a function return annotation
  `fn(n : I64) : T { … }` (fails on main too).
