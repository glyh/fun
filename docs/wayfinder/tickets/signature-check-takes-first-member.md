---
title: Checking a module against a signature takes the first member of a name
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Checking a module against a signature takes the first member of a name

Found by the C# port's structs fork and confirmed by the follow-up verification
(2026-09-16). **Fixed in the C# port only**; the OCaml prototype keeps the defect.

## Defect

```
(fn(m : sig { x : I64 }) { m.x })(module { pub x = 'a'; pub x = 1 })
```

The module's member `x` is the last one, `1` (domain model I3: a dotted path
denotes the last member of that name). The prototype checks the signature against
the *first* `x` and fails with `CannotUnify(I64 vs Char)`; the port gives 1. Same
family as the closed [dotted-paths-first-match](dotted-paths-first-match.md).

## Conformance

`values/signature-check-takes-last-member` (1), listed in
`test/conformance/prototype-divergences.txt`.
