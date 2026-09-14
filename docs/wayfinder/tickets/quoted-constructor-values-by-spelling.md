---
title: A quoted constructor value is re-found by spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee:
resolution: Fixed — `Con` is deleted. A constructor value quotes (in `Nbe_quote` and in `Unify`'s solution builder, through one `Nbe_quote.con_term`) to the `Ctor` term that builds it, carrying its nominal value and the nominal's params, so evaluation never looks a constructor up — by name or otherwise. `Nbe.eval_con` is gone. The live symptom was worse than shadowing: a quoted constructor with a payload, or of a parametric type, bound outside a module was not found at all (`unbound constructor/type`), because only nullary constructors sit in the environment as `VCon`.
closed_date: 2026-09-14
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
