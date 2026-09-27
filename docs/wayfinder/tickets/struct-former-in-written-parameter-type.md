---
title: A struct former in a written parameter type is refused
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# A struct former in a written parameter type is refused

Found 2026-09-27 while verifying the signature-meta shapes
([parameter-type-metas-capture-earlier-parameters](parameter-type-metas-capture-earlier-parameters.md)),
by the integrator. Recorded there as an "adjacent, different mechanism" footnote; it is a
user-visible bug on its own and does not belong inside that ticket.

## The bug

A parameter whose written type applies a struct former is refused, **with the type
argument supplied**:

```fun
{ Box = fn[A : Type] { struct { v : A; pub method get(r : Ref(I64)) : I64 { 3 } } };
  g = fn(o : Box[I64]) : I64 { o.v };
  b = Box[I64]{ v = 1 }; g(b) }
```

```text
ELAB type mismatch: cannot unify VStruct with VU
```

Two facts that make it precise, both measured:

- **It is not the spine bug.** The same program fails identically with and without the
  signature-meta fix ([S1](parameter-type-metas-capture-earlier-parameters.md) applied), and
  it contains no spine-shaped expression. The failure is at the parameter's *written type*.
- **A supplied argument is enough.** The ticket it was found in described the family as "a
  user type with an **unsupplied** implicit is refused in type position — `Ref(Pair)` where
  `Pair = fn[A, B] { struct { … } }`". This case has `Box[I64]`, its argument *given*, and
  still fails — so the trigger is a struct former applied in a written parameter type, not
  the missing argument. (`Box(I64)` as the parameter type is a different error, `applying
  non-function`, which is the form-versus-application spelling of the same surface.)

## Why it matters

It is not exotic: a struct type with a parameter is exactly how this language writes a
container, so `fn(o : Box[I64])` is the ordinary way to take one. Macros and library code
will both hit it, and the diagnostic (`cannot unify VStruct with VU`) names neither the
parameter nor the missing implication — `VU` and `VStruct` are internal value
constructors.

## Where to look

- `src/Fun.Compiler/Elaborator.cs` — the written-parameter-type elaboration
  (`InferLam` and the `Lam`-against-`Pi` check), which is where `Box[I64]` is turned into a
  domain
- `src/Fun.Compiler/Unify.cs` — the `VStruct`/`VU` mismatch that is reported
- `src/Fun.Compiler/Nbe.RecTypes.cs`, `Elaborator.RecTypes.cs` — a struct former is a
  recursive binding, so the form yields a closure until applied
- [parameter type metas capture earlier parameters](parameter-type-metas-capture-earlier-parameters.md)
  — the investigation it came out of, and its `dep4`/`dep10` probes, which are the same
  family seen from the value side

## A case this needs

`fn(o : Box[I64]) : I64 { o.v }` applied to `Box[I64]{ v = 1 }` in
`test/conformance/cases/elaborate/`, which today cannot even be written. Once it passes,
the same shape with the argument left implicit is the next probe.
