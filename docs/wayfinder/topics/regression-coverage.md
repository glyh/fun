# Regression coverage plan

## Goal

Track regression tests for current behavior before larger prototype work continues. This plan prioritizes features that already exist and should remain stable through type-case, protocol/typeclass, effects, macro, and CLR/C# rewrite exploration.

## Phase 1: Imports + modules integration

Highest-value gap: local feature tests are strong, but imported modules need more cross-feature regression coverage.

- [x] imported record type construction and field access
- [x] imported record pattern matching
- [x] imported method using `self`
- [x] imported method using `Self`
- [x] imported qualified nested constructor pattern
- [x] imported module alias used in a pattern
- [x] imported public effect used in a handler
- [x] imported private constructor remains inaccessible
- [x] imported private record/module member remains inaccessible through aliases

## Phase 2: Effects regression hardening

Effects are changing fastest, so keep behavior locked down with focused tests.

- [x] `resume` outside an effect branch is rejected
- [x] `resume` without an argument is rejected at parse or elaboration time
- [x] continuation reuse raises a clear runtime error, if surface-expressible
- [x] full multi-operation handler removes the handled effect
- [x] partial multi-operation handler remains effectful
- [x] handler branch can perform another effect handled by the same match
- [x] direct unhandled `perform` raises a clear top-level runtime error
- [x] tuple operation payload patterns work statically and at runtime
- [x] record operation payload patterns work statically and at runtime

## Phase 3: Type-case / equality baseline

Before expanding type-case, lock current behavior around primitive type-head matching and equality.

- [x] primitive type-head matching works for `I64`
- [x] primitive type-head matching works for `Bool`
- [x] primitive type-head matching works for `Char`
- [x] primitive type-head matching works for `Unit`
- [x] open `Type` matches require a fallback where appropriate
- [x] `==` accepts same-typed primitive operands
- [x] `!=` accepts same-typed primitive operands
- [x] `==` rejects mismatched operand types
- [x] nominal equality is accepted statically under current polymorphic equality
- [x] record equality is accepted statically under current polymorphic equality

## Phase 4: Records and methods local edge cases

Existing local coverage is already strong. Add only edge cases that protect intended semantics.

- [x] method returning `Self`
- [x] method accepting parameterized `Self`
- [x] private helper used by a public method
- [x] method on parameterized record with extra explicit argument
- [x] record pattern with qualified alias through an imported module

## Phase 5: Qualified paths

Local qualified path coverage is solid. Add import-backed path cases to protect module/import interaction.

- [x] imported nested module constructor pattern
- [x] alias of imported module used in constructor pattern
- [x] alias of imported module used in record pattern
- [x] private imported constructor remains inaccessible
- [x] wrong-nominal qualified constructor pattern is rejected across imports

## Suggested implementation order

1. Add semantic import integration tests.
2. Add backend/runtime import integration tests for cases that evaluate to concrete values.
3. Add effects hardening tests while effect code is still fresh.
4. Add type-case/equality baseline tests before implementing more type-case behavior.
5. Fill records/methods and qualified-path edge cases only where coverage is still missing.
