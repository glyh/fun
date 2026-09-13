---
title: A bare arrow is pure
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A bare arrow is pure

## Decision

- `A -> B` means `A -> B can {}` — pure.
- `A -> B can _` infers the row (Koka's `_e` wildcard); generalised at `let`
  like any other inferred type, so it gives effect polymorphism.
- `A ~> B` is shorthand for `A -> B can _`, defined in the prelude as a
  template, not a compiler rule:

  ```fun
  pub infix (~>) 0 Right ($a, $b) -> $a -> $b can _
  ```

```fun
sqr     : I64 -> I64                            -- pure
counter : Unit ~> Ref(I64)                      -- effectful, row inferred
map     : (A ~> B) -> List(A) ~> List(B)        -- polymorphic higher-order
```

Reverses [algebraic-effects](../topics/algebraic-effects.md) Phase 6's
"Regular `A -> B` no longer means pure; it infers a row tail."

## Why

Purity now decides whether the checker may evaluate a call and whether a
nominal declared in it is applicative
([nominal-identity-applicative-by-purity](nominal-identity-applicative-by-purity.md)).
The unwritten case should be the safe one.

```fun
merge_twice = fn(mk : Unit -> SetSig) ->
  do a = mk(()); b = mk(()); a.union(a.empty, b.empty) end
```

With an inferred row, `mk` is possibly generative while the body is checked,
and this is rejected; with a pure bare arrow it is accepted, and passing an
effectful `SymbolTable` is a type error at the call. A hidden inferred row would
also hide a purity flip — the footgun
[refs-in-effect-rows](refs-in-effect-rows.md) exists to prevent.

Precedent: Koka — "When the effect is total we usually leave it out in the type
annotation", with `_e` for an inferred effect. Frank's ambient-effect default
fails `merge_twice` the same way today's rule does.

## Where it changes

- `lib/semantic/typecheck/elab_type_expr.ml` — an omitted row is a fresh meta
  tail (`{ effects = []; tail = Some (Meta …) }`); becomes the closed empty row.
- `can _` parses to the fresh-meta row.
- Prelude: add `~>`. Its precedence against the `->` token needs checking —
  `->` is a reader token, not a prelude operator.
- Higher-order prelude functions and tests that relied on bare arrows threading
  effects gain `~>`.

## Cost

Higher-order signatures that pass effects through must say so. Koka's standard
library carries the same cost.
