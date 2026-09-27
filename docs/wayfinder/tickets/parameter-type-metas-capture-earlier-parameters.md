---
title: A parameter's type meta captures the earlier binders, not only `self`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A parameter's type meta captures the earlier binders

Found by the [method-signature metas](method-signature-metas-capture-self.md) fork (2026-09-27) as
the same failure one layer out, and **measured by the integrator on both `main` and base
`a2c0044`** — the numbers below are identical on both, so this is pre-existing, not a regression
from that fix.

## The defect

A meta inserted while elaborating a parameter's type lists the binders already in scope in its
spine. At a call, every spine entry must be invertible — a *variable*. `self` is never one (the
receiver is a value), which is what the method ticket fixed by marking that entry `Defined`. An
**earlier parameter** is a variable only if the caller happens to pass one, and nothing about a
`Ref(I64)` parameter's hidden heap depends on the parameter before it — the dependency is a
by-product of where the meta is inserted, not of what the type needs.

| probe | result, `main` and base alike |
| --- | --- |
| `f = fn(a : I64, r : Ref(I64)) : I64 { a }; x = ref(40); f(0, x)` | `a meta's spine argument is not a variable` |
| same, with `n = 0` then `f(n, x)` | same failure |
| same, called from a λ with a real parameter: `g = fn(y : I64) { x = ref(40); f(y, x) }; g(0)` | **`VALUE 0`** |

So the difference is not literal-versus-variable at the source level: `n = 0` is let-bound and
unfolds to the literal at the call, while a λ parameter stays a variable and inverts. That is the
whole of the mechanism — and it is why the failure is easy to miss: the same function works or
fails depending on *how* its argument was produced.

## The same failure in three other shapes (all five probes measured)

| program | `main` | base `a2c0044` |
| --- | --- | --- |
| plain `fn` with a literal argument | fails | fails |
| plain `fn` with a let-bound argument | fails | fails |
| `Box[I64]{…}.get(x)` where `Box = fn[A : Type] { struct { … pub method get(r : Ref(I64)) … } }` | fails | fails |
| `f[I64](1, x)` where `f = fn[A : Type](a : A, r : Ref(I64)) …` | fails | fails |
| `g(b, x)` where `g = fn(o : Box[I64], r : Ref(I64)) …` and `b = Box[I64]{ v = 1 }` | fails | fails |

The last three share the story: the ambient `A` or the struct value is substituted at `Box[I64]` or
passed as a record, so the spine entry is not a variable.

## Adjacent, different mechanism, also pre-existing

A user type with an **unsupplied** implicit is refused in type position — `Ref(Pair)` where
`Pair = fn[A : Type, B : Type] { struct { … } }` gives `cannot unify VStruct with VU`, for a method
and a plain `fn` alike (measured on `main`; base not checked, because both shapes fail identically
and neither is touched by the method fix). `Ref(Pair[I64, Bool])` works. Whether an implicit should
be inserted inside `Ref(…)` is a separate question from this ticket's, and the two should not be
fixed together.

## Direction

The fix is the method ticket's, generalised: a parameter's written type should insert its metas so
they abstract over no *earlier parameter* either, and let unification discover any real dependency
instead of the insertion order dictating one. The method fix is the model —
`Context.WithoutSelfInMetas` marks an entry `Defined` for exactly this purpose — but it is keyed to
one entry, so the generalisation needs a decision: which earlier entries a signature meta should
skip (all written-type positions? only non-dependent ones?), and what that does to
`f : (a : T) -> (r : Ref(…a…)) -> …`, where the dependence is genuine and must survive.

**That decision is why this is a ticket and not a fork**: the fix has a shape to choose, and a
wrong choice would break dependent parameter types that work today. Probe a genuinely dependent
signature before choosing.

## Reading

- [a meta in a method's signature captures `self`](method-signature-metas-capture-self.md) — the fix
  this generalises, and the three findings it recorded
- `src/Fun.Compiler/Elaborator.Structs.cs` — `MethodType`/`Params`/`MethodBody` and
  `Context.WithoutSelfInMetas`
- `src/Fun.Compiler/Unify.cs:144` (`Invert`) — where a non-variable spine entry is refused
- `src/Fun.Compiler/Nbe.cs` — `InsertedMeta`, which reads the `EntryKinds` spine
