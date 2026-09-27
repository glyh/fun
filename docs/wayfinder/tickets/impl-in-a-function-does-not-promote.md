---
title: An impl declared inside a function does not promote its bound
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# An impl declared inside a function does not promote its bound

Found by the fork that implemented
[a generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md)
(2026-09-27); reproduced by the integrator. Top-level, module and unit impls — which is
what `std/` contains and what the fix was measured on — work. An impl declared **inside a
function** whose head variable comes from the enclosing binder does not.

## The reproducer

```fun
{ trait Size(A) = sig { size : A -> I64 };
  impl Size(I64) = module { fn size(x) { 1 } };
  f = fn[B : Type] { impl Size(List(B)) = module { fn size(xs) { match (xs) { Nil => 0, Cons(h, t) => Size.size(h) } } }; 1 };
  f[I64] }
```

```
ELAB missing implementation of `Size`
```

The control — the same impl with a body that never uses its element — elaborates:

```fun
  f = fn[B : Type] { impl Size(List(B)) = module { fn size(xs) { 42 } }; 1 };
#   -> VALUE 1
```

So the impl is accepted; what fails is *promoting* the body's demand for `B`'s evidence
into a dictionary argument of the impl, because the head meta carries a spine over the
enclosing function's bound entries.

The fork reported this failure with the message `cannot choose … never known` at the
definition; the reproducer above gives `ELAB missing implementation of \`Size\``. Both
were run — they are different shapes, or the message depends on where the demand is
noticed. The message is worth pinning as part of the fix.

## Why it matters

Small today: nothing in `std/` declares an impl inside a function, and the shipped
library surface does not need it. It matters because a **local impl is the natural way
to write a scoped instance** — the same thing `open` is used for — and because the
failure is a false negative: the impl is legal, its body is legal, and the evidence it
demands exists in scope (`Size(I64)` is right there).

It is also the shape a macro-generated impl could easily take, since a macro's output
lands wherever the macro is invoked.

## Reading

- `src/Fun.Compiler/Elaborator.Traits.cs` — `Contribute`'s promotion of pending evidence,
  and `ResolveVar`/`IndexOf`, which walk the impl's own variables
- `test/conformance/cases/values/trait-generic-impl-bound-in-body.fun` — the top-level
  shape that works, to be extended rather than replaced
- [A generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md)
  — the fix this is the missing half of
