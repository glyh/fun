---
title: Macro bodies implicitly open the prelude
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Macro bodies implicitly open the prelude

## Decision

A macro body is elaborated in the scope of its definition site, and nothing is
ambient there. A unit that writes macros using prelude names opens the prelude
(`open (import "std")`) or qualifies them, exactly as its runtime code must.

## Today

`Macro_driver` elaborates every macro body against one persistent
prelude-opened context (`elab_entry.ml`, the helper documented as the
ctx-builder counterpart of `on_macro_body`). A macro body therefore sees the
prelude bare even in a unit that never opened it — the one place the
[explicit prelude open](explicit-prelude-open-operator-demotion.md) rule does
not hold.

## Why

- **One scope per macro.** Quoted syntax in a macro resolves at the definition
  site ([macros-have-no-quoted-syntax](macros-have-no-quoted-syntax.md)); an
  implicitly opened body would give the body and its quoted ids two different
  scopes.
- **Sets of scopes / Racket.** A transformer is compiled in its definition
  site's scope; Racket is stricter still (`for-syntax` requires). `fun` has no
  phases, so the plain rule applies.
- **The strict-open rule has no exceptions** once this is removed.

## Cost

Every unit — test fixtures included — that defines macros without opening the
prelude gains one `open (import "std")` line, or qualifies via `stdlib.`.
