---
title: Macro type binders should be explicit
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Macro type binders should be explicit

## Decision

A name inside a macro annotation is always a reference. A macro binds type
parameters the way a function does, in `[…]`:

```fun
macro m[A](x) : Expr(A) -> …     -- A bound here; Expr(A) refers to it
macro n(x) : Expr(I64) -> …      -- constraint
macro k(x) : Expr(Strng) -> …    -- error: unbound Strng
```

Supersedes the *annotation resolution* bullet of the closed
[design-type-aware-macro-interleaving](design-type-aware-macro-interleaving.md)
ticket ("`Expr(Foo)` is a constraint only if `Foo` is a type in the current
prior type namespace. Unresolved uppercase names remain binders.").

## Why

- **Consistency.** `fn[A : Type](…)` already binds implicit type parameters;
  every other type position only refers. Annotations were the one place a name
  could introduce a binder.
- **Meaning stops depending on unrelated code.** Today an earlier `type A`
  anywhere — an import, a prelude change — turns every macro's binder `A` into
  a constraint and changes its arity, breaking callers without touching the
  macro.
- **Typos become errors.** The closed ticket deferred typo detection to a lint
  only because implicit binding made a typo legal.
- **No case rule.** Binding no longer depends on a leading uppercase letter, so
  lowercase type aliases work as constraints and `[t]` can bind.
- **Arity is syntactic.** It is the count of `[…]` and `(…)` parameters, so it
  no longer waits on resolving the annotation.

## Where the hack lives

- `lib/semantic/typecheck/macro_resolver.ml` `resolve_kind` — resolves →
  constraint, else uppercase → `synthesize_binder_param`, else silently
  unconstrained (a lowercase typo is dropped without error, too).
- `lib/core_kernel/syntax.ml` `MacroAnnotationAdapter.resolve` — the
  parse-time copy of the same rule.
- `lib/expand/enforest.ml` `LegacyExprBinder` (bare `: A`), and
  `lib/expand/expand.ml` binder detection.
- `Syntax.MacroKind.Expr of string option * string option` — the
  binder-name slot goes away.

Existing tests use `: Expr(A)` / `: A` binders and migrate to `[A]` in the same
change.
