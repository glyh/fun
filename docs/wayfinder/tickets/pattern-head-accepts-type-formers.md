---
title: Any function returning a nominal type is accepted as a pattern-head type
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Any function returning a nominal type is accepted as a pattern-head type

## Defect

Since path heads resolve by entry
([names-resolve-without-spelling](names-resolve-without-spelling.md)), a
pattern head's nominal is read off the entry its head resolves to. A
parametric type's entry is its type former (a function), so
`find_nominal_template_opt` applies the entry to fresh metas until a
`VNominal` appears. That also accepts *any* function whose result is a
nominal — `f = fn(A : Type) -> List(A)` becomes usable as a type name in a
pattern head, behaving like an alias.

## Question

Is that the intended semantics (types are values, so a type-level function
that reduces to a nominal is as good as its name — consistent with the design
philosophy) or should only a declaration's own former qualify (the entry must
be the nominal's former, identified by nominal id)? If the former, the entry
needs to carry the declaration identity rather than being recognised by
reduction.

## Where

`lib/semantic/typecheck/elab_resolve.ml` `find_nominal_template_opt`.

## Found by

The path-heads implementation (2026-09-14).
