---
title: What a macro annotation constraint means, and how many type binders a macro has
parent: ../fun-design-map.md
labels:
  - wayfinder:task
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

## Grilled (2026-09-15)

1. **A constraint checks the output.** `macro n(x) : Expr(I64)` elaborates the
   macro's output against `I64` at the call; a mismatch is an error naming the
   macro ("macro `n` promises Expr(I64), its output has type String"). `T` must
   be a type, checked at the definition.
2. **Any number of type binders.** `macro pair[A, B](a : Expr(A), b : Expr(B)) : Expr((A, B))`,
   like a function's implicit parameters.
3. **Parameters are typed the same way.** `(x : Expr(I64))` means syntax that
   elaborates at `I64`; the argument is checked when it elaborates
   ("argument x of `twice` expects Expr(I64), got String"). A plain `Expr` is
   untyped.
4. **Binders are solved before the macro runs**, from the call's expected type
   and its typed arguments; the macro runs with them; then the output is
   checked. A binder unsolved when the macro must run is an error at the call
   ("cannot infer A for `default`").

Question 3 above (the `_ = T` device) follows from these: the annotation is the
output's type, so the device should go.
