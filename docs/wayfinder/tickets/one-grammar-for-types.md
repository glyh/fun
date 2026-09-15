---
title: Annotation types use a separate grammar that ignores user operators
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Annotation types use a separate grammar that ignores user operators

Found by the bare-arrow-is-pure run (2026-09-15).

Types are values, but a type written in a binding or parameter annotation
(`x : T = …`, `fn(g : T)`) is read by a separate type grammar that hardcodes
`->`, `+` and `*` and reads no user roles. So the prelude's `~>`
(`pub infix (~>) arrow ($a, $b) { $a -> $b can _ }`) works where the expression
grammar reads the type and fails in annotations:

```fun
Callback = Unit ~> I64;                   // works (expression grammar)
wrap = fn(g : Unit ~> I64) { g(()) }      // parse error (type grammar)
```

Direction: read annotation types with the expression grammar (one grammar,
Consistency > Flexibility); `->` and `can` become expression-level forms with
their order groups. Check the "after `->`" and `Tight` built-in positions the
order-groups run kept.
