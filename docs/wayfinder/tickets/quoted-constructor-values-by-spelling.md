---
title: A quoted constructor value is re-found by spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A quoted constructor value is re-found by spelling

## Defect

[nested-qualified-constructor-pattern](nested-qualified-constructor-pattern.md)
made `NomRef` carry the nominal id, so a quoted *type* evaluates back to the
nominal it came from. A quoted *constructor value* (`VCon`) still quotes to
`Con name` in `Nbe_quote.quote` and in `Unify`'s solution-term builder, and
`Nbe.eval_con` finds it again by name — innermost binding first. The same
shadowing that broke chain payloads applies: a solved meta or a normal form
mentioning a constructor, re-evaluated under an environment where another
nominal has a constructor of that spelling, denotes the wrong constructor.

## Direction

`Con` carries the nominal id (and the constructor name as the member label
inside that nominal), mirroring `NomRef`. Write the shadowing case first.

## Found by

The nested-qualified-constructor fix (2026-09-14).
