---
title: Template and operator heads resolve by scope set
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Template and operator heads resolve by scope set

## Decision (macro model M7, pass one I4c)

Fixity, precedence and "this name is a syntax form" are a binder's
**syntactic role**, resolved by scope set like any other meaning a name
carries, so a later binder of the same name takes the role away (`do not = 5;
not end` should work).

## Today

`Binding.find_operator` is string-keyed and newest-wins, because enforestation
runs before expansion and no scope sets exist yet. Its comment defers
scope-keyed resolution to "the interleaving driver". The prefix-template and
operator paths in `enforest.ml` consult it by written name.

## Notes

This needs scopes at parse time, which is the fixed-passes defect the surface
pass named (enforestation entirely before expansion). It likely needs Honu's
arrangement: body forms are enforested lazily as expansion reaches them, after
the enclosing binders have added their scopes. Grill the arrangement before
building it.
