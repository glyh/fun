---
title: An under-applied declaration macro is fed a dummy Unit
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# An under-applied declaration macro is fed a dummy Unit

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariant

**M8**: kind checking is positional, and its error is an error. A macro never runs
on syntax it was not given.

## Where the code deviates

`lib/expand/expand.ml:1117-1122`: after applying a `: Decl` macro to its
arguments, `force_val` keeps applying any remaining lambda to a synthetic
`Atom Unit` expression until the result stops being a function. The
`ArgumentCount` check (`enforest.ml:27`) runs only when some parameter has a
non-`Expr` kind, so an all-`Expr` call is not counted.

## Example

```fun
macro two(a, b) : Decl { … };
two(x);   // runs with b = (); should be an ArgumentCount error
```

## Direction

Delete `force_val`. Check arity for every macro call, not only kinded ones, and
fail with `ArgumentCount`.
