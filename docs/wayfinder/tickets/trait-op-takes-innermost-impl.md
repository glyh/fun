---
title: Trait.op takes the innermost impl of the trait whatever its argument
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# `Trait.op` takes the innermost impl of the trait whatever its argument

Recorded as a follow-up by the C# port's traits fork (2026-09-16).

## Decided (user, 2026-09-17): resolve by the argument's type

```
{ trait Size(A) = sig { size : A -> I64 };
  impl Size(I64) = module { size = fn(n) { 1 } };
  impl Size(Char) = module { size = fn(c) { 2 } };
  Size.size(5) }
```

gives 1: `Size.size` at an `I64` argument uses `Size(I64)`, found the way a bound
`[A : Size]` finds its impl (evidence for the trait *at the argument*), so both paths
to an impl agree. Both the prototype and the port take the innermost impl
(`Size(Char)`) and fail with `CannotUnify(Char vs I64)`. **To be fixed in the C#
port only** (the `ponytail:` note in `Elaborator.Traits.cs`); the prototype keeps
the defect.

## Conformance

`values/trait-op-resolves-by-argument` (1), listed in
`test/conformance/prototype-divergences.txt`.
