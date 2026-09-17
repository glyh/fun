---
title: Impl resolution takes the innermost impl, not the most precise matching one
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Impl resolution takes the innermost impl, not the most precise matching one

Recorded as a follow-up by the C# port's traits fork (2026-09-16).

## Decided (user, 2026-09-17): the most precise matching impl

```
{ trait Size(A) = sig { size : A -> I64 };
  impl Size(I64) = module { size = fn(n) { 1 } };
  impl Size(Char) = module { size = fn(c) { 2 } };
  Size.size(5) }
```

gives 1. Resolution follows the rule written in
[traits.md, "Resolution"](../topics/traits.md): candidates are the in-scope impls
matching the trait *at the use's argument types* (for `Trait.op` and bounds alike);
the most precise one (whose arguments are an instance of every other candidate's)
is chosen; no unique most precise one is an ambiguity error; unknown argument types
make the choice wait, and still-unknown at the end is an error; lexical nearness
never breaks a tie. This replaces the earlier "multiple matching impls: ambiguity
error". Both the prototype and the port take the innermost impl here and fail with
`CannotUnify(Char vs I64)`. **To be fixed in the C# port only** (the `ponytail:`
note in `Elaborator.Traits.cs`); the prototype keeps the defect.

## Conformance

`values/trait-op-resolves-by-argument` (1), listed in
`test/conformance/prototype-divergences.txt`. The implementing fork adds cases for
precision (a generic and a specific impl), incomparable candidates (ambiguity) and
waiting on unknown argument types.
