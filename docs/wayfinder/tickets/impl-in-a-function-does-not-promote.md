---
title: An impl declared inside a function does not promote its bound
parent: ../quill-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-28
resolution: Closed 2026-09-28 (`44583e0`, merged as `impl-in-a-function-promotes-its-bound`). The promotable shape is fixed: `BindHeadNames` now returns the head's variable *values* and `Contribute` forces each after the head elaborates, so the impl's variable is the meta inference actually left — inside a function a constructor's parameter meta whose solution is a lambda over the enclosing bound entries, which `ResolveVar` could not follow. **`ResolveVar` is deleted**, which is what the fix bought. Probes: the lowercase shape with an unused body went from `cannot choose an implementation … never known` to `VALUE 1`; used at run time, from `missing implementation of Size` to `VALUE 7` (case `values/trait-local-impl-bound-in-body`); the top-level case unchanged. Suite 905 → **906 cases, 0 failed**; xUnit 206. **The uppercase reproducer this ticket was filed with is a different shape and stays unfixed by ruling:** under the case rule an uppercase free name in a head is a *reference* to the enclosing binder, so the head has no meta at all (`Vars = []`) and its demand forces to the rigid `VVar(B)` — evidence the *enclosing scope* must supply, not something an impl can carry, since `Vars` indexes the impl's own metas. It still reports a plain `missing implementation of Size`, which is misleading rather than wrong; that wording belongs with the other diagnostics work (Stage 12).
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

```quill
{ trait Size(A) = sig { size : A -> I64 };
  impl Size(I64) = module { fn size(x) { 1 } };
  f = fn[B : Type] { impl Size(List(B)) = module { fn size(xs) { match (xs) { Nil => 0, Cons(h, t) => Size.size(h) } } }; 1 };
  f[I64] }
```

```
ELAB missing implementation of `Size`
```

The control — the same impl with a body that never uses its element — elaborates:

```quill
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

- `src/Quill.Compiler/Elaborator.Traits.cs` — `Contribute`'s promotion of pending evidence,
  and `ResolveVar`/`IndexOf`, which walk the impl's own variables
- `test/conformance/cases/values/trait-generic-impl-bound-in-body.qll` — the top-level
  shape that works, to be extended rather than replaced
- [A generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md)
  — the fix this is the missing half of

## Findings 2026-09-28, from the fork that ran out of turns before landing (measured)

**There are two shapes here, not one, and only the first is promotable.**

- **Shape "b" — `impl Size(List(b))` with a lowercase `b` (the impl's own variable).** Fails
  because `ResolveVar` follows only bare `VMeta` solutions, while inside a function the head
  meta's solution is a lambda-wrapped constant (`987 -> VLam(…) -> VMeta 988 -> VMeta 989`) and
  the body's demand forces to meta `989`. Deriving each head variable's identity by **forcing its
  recorded value after the head is elaborated** — a one-line widening of `Contribute` — fixes it:
  the fork's `/tmp/probe_b.qll` printed `VALUE 1`. This is the shape the *case rule* asks for,
  since a lowercase head name is the impl's own.
- **This ticket's own reproducer spells `impl Size(List(B))` with an uppercase `B`**, written
  before the case rule landed. Under that rule an uppercase free name is a **reference to the
  enclosing binder**, so the head has **no meta at all** (`Vars = []`) and its demand forces to
  the rigid `VVar(B)`.

**Ruling: a rigid `VVar` cannot be promoted, and the attempt must not be repeated.**
`Vars` indexes the impl's *own* metas — `ImplBound(Trait, int Var)` is a meta index, and a rigid
variable has none — so a demand on an enclosing binding is evidence the *enclosing scope* must
supply, not something the impl can carry. The fork's attempt (a fresh meta solved to the rigid
`VVar`, appended to `Vars`) also walked into an unrelated refusal —
`not ported yet: reflecting the type arguments a pattern synonym use supplies` — which
[reflect-pattern-synonym-type-arguments](reflect-pattern-synonym-type-arguments.md) is closing
independently; a green run there would have been luck, not soundness.

**So the work is:** land shape "b" (the `Contribute` widening plus a case), then *measure* what
the uppercase reproducer does — if it still reports `missing implementation`, that message is
misleading (the evidence was never in scope) and the residue is a clearer diagnostic, recorded
rather than chased.

## Landed 2026-09-28 (`44583e0`, merged as `impl-in-a-function-promotes-its-bound`)

Shape "b" only, as scoped. `BindHeadNames` now returns the head's variable **values** and
`Contribute` forces each after the head is elaborated, so the impl's variable is the meta
inference actually left — inside a function that is a constructor's parameter meta whose solution
is a lambda over the enclosing bound entries, which `ResolveVar` could not follow. **`ResolveVar`
is deleted**, which is what the fix bought: one walker fewer rather than one more.

Probes: the lowercase shape with an unused body went from `cannot choose an implementation of
Size: its argument type is never known` to `VALUE 1`; used at run time from `missing
implementation of Size` to `VALUE 7` (new case `values/trait-local-impl-bound-in-body`); the
top-level case unchanged at `VALUE 1`. Suite `905` → **`906` cases, 0 failed**; xUnit `206`.

**The residue, re-measured by the integrator on the merged tree.** The fork measured it on a base
that predates the reflection fix, where it failed with the reflection refusal; on today's tree
the ticket's **uppercase** reproducer (`impl Size(List(B))`, `B` the enclosing binder) still
fails, now with `ELAB missing implementation of \`Size\``, while its control answers `VALUE 1`.
That is what the ruling predicts — the demand is on a rigid `B` and nothing in the enclosing scope
supplies `Size(B)`, so the impl cannot carry it — so the only thing still wrong is the **message**:
it reports a missing implementation without saying the evidence was never in scope where the impl
was written. **Recorded, not chased.**
