---
title: What a macro annotation constraint means, and how many type binders a macro has
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented. A macro's signature (type binders, `(x : Expr(T))` parameters, `: Expr(T)` output) is a pi type elaborated at the definition; a typed call is deferred to the elaborator, which solves binders from the expected type and typed arguments, requires them solved before running, and checks the output. The `_ = T` device and the elaborator's copied macro table are deleted.
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

## Implemented (2026-09-15)

- **Signature.** `Syntax.macro_signature` writes the pi type
  `[A : Type] … -> T1 -> … -> T` (an untyped output is one more implicit
  binder); the expander elaborates it where the macro is defined, annotated as a
  `Type`, and stores it on `Expand_ctx.macro_entry`. `MacroBinding`/`MacroDef`
  carry the written `output`.
- **When a macro runs.** A macro whose signature promises a type (a binder, a
  typed parameter or a promised output) is deferred to the elaborator; one that
  promises nothing runs during expansion, as before.
- **The call.** `Elab_resolve.apply_typed_macro` instantiates the signature's
  binders as metas, checks each typed argument (expanded, then checked at its
  domain), unifies the result with the expected type, requires every binder
  solved, runs the macro with the reflected types, and checks its expanded output
  at the promised type. Errors: `MacroArgumentType`, `MacroBinderUnsolved`,
  `MacroOutputType`, named by the macro's written name.
- **Choices this made.** A typed argument elaborates twice (checked before the
  macro runs, and again where the output places it). A promise the expected type
  contradicts fails as an ordinary unification error at the call, before the
  macro runs. The elaborator finds compiled macros through its macro runtime
  (`lookup_macro`), not a copied table.
