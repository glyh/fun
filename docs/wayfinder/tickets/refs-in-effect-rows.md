---
title: Refs belong in effect rows
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Refs belong in effect rows

## Decision

Allocating, reading or writing a ref is a run-time effect that appears in the
effect row (`can Ref`). Reopens the first-pass choice in
[references](../topics/references.md) — "no user-visible effect-row tracking
for refs" — which guards elaboration with the syntactic `compile_time_safe`
check instead.

## Why

[Nominal identity](nominal-identity-applicative-by-purity.md) is generative
exactly under a run-time effect. If refs are invisible in types, purity is
invisible too, and:

- **A library adds a cache, clients break far away.** One `table = ref(empty)`
  inside a module maker flips its types to generative with no change to the
  maker's type; the error surfaces in client code as `a.Symbol ≠ b.Symbol`.
- **Higher-order code cannot be both sound and useful.**

  ```fun
  merge_twice = fn(mk : Unit -> SetSig) ->
    do a = mk(()); b = mk(()); a.union(a.empty, b.empty) end
  ```

  Without purity in `mk`'s type the checker must either reject this for every
  caller, or accept `merge_twice(SymbolTable)` and let symbols cross tables.
  With `can Ref` visible, a pure `mk` is applicative and `SymbolTable` is a
  type error at the call.

## Cost

- Allocation shows in types: `counter : Unit -> Ref(I64) can Ref`.
- Internal-only refs still leak `can Ref` to callers unless discharged by a
  local-refs handler (`runST`-style), which needs a brand so refs cannot escape.
- Generic helpers need effect polymorphism — required by effects anyway.

`compile_time_safe` may survive as the elaborator's implementation of "do not
run this during type checking", but it is no longer where purity is decided.
