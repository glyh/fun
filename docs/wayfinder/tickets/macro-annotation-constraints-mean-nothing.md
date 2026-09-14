---
title: What a macro annotation constraint means, and how many type binders a macro has
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# What a macro annotation constraint means, and how many type binders a macro has

## State after [macro-type-binders-should-be-explicit](macro-type-binders-should-be-explicit.md)

- `macro n(x) : Expr(I64)` — `I64` is resolved by scope (the enforester writes
  `_ = I64` at the head of the body), so an unbound or misspelt name is an
  error at the definition. **Nothing else uses it:** the call site does not
  unify the expected type with it (the old use-site unification was
  unreachable — a constraint never made a macro type-aware), and nothing
  checks that the constraint is a type (`: Expr(None)` is accepted).
- A macro binds **at most one** type parameter; `[A, B]` and `[A] : Decl`
  are parse errors. The ticket did not decide this; it fell out of
  `TypedExpr` carrying one expected type.
- The `_ = T` insertion is the mechanism by which the annotation resolves in
  the body's scope — a syntactic device rather than a named rule.

## Questions

1. Is a constraint a **check** (the call's expected type must convert with
   `T`, error otherwise), a **pre-solve** (unify the expected type with `T`
   before running the macro, making the macro type-aware), or **documentation
   only**? Must `T` be a type?
2. Should `[A, B]` be allowed (e.g. `B` solved from `A` by the output), or is
   one binder the model?
3. Should the annotation elaborate as the body's *type* (`Expr(T)` where `Expr`
   is indexed) instead of the `_ = T` reference trick?

## Also

`docs/wayfinder/macro-system/*.md` and older closed tickets still show the
retired `: Expr(A)` / `: A` binder form.

## Found by

The explicit type-binders implementation (2026-09-14).
