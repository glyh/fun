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

## Diagnosis 2026-09-28 (from a fork that could not be resumed; its branch held only the probes)

The fork that found this located the cause by instrumentation, and its branch
(`pi-agent-1b517b3f-ca18-468`) was **instrumentation only** — never merged, now deleted. What it
established, and what reading it changed:

- **The trigger is *any non-field member*, not the lambda former.** A plain
  `S = struct { k : I64; pub method m() : I64 { self.k } }` used as `fn(o : S)` fails the same
  way, so `Box[I64]` was one instance.
- Stack: `TypeValue` → `TypeOfExpr` → `CheckTypeLike` (`Elaborator.Structs.cs:354`) →
  `ctx.Unify(type = VStruct, VU)` → `Unify.Values`' default branch.
- Its debug line: `IsTypeLike false: forced=VStruct entries=v:Field:VAtomTy,get:Method:VLam`.
- **The precise cause — and the reason the obvious fix is already there:** `IsTypeLike`'s
  `VStruct` branch already filters to `OfType<ModuleEntry.Field>()`, but a struct's **method is
  itself a `ModuleEntry.Field` with `Kind == Method`** (`Elaborator.Structs.cs:58` routes it
  through `AddMember`, and `:102` puts it in the same list as the field entries). The predicate
  skips only `Private`/`PrivateMethod` and then requires the entry's payload to be type-like —
  and a method's payload is its **definition** (`VLam`), so the test fails on a member that is
  not part of the record's *type* at all.
- **So the fix is the predicate, and it is the rule that landed yesterday**: require
  type-likeness only of entries whose kind is `Field`, and skip every other kind — `Field` is
  what a record type is made of. (An earlier framing of mine, "ignore entries that are not
  `ModuleEntry.Field`s", was wrong for exactly this reason: methods *are* `ModuleEntry.Field`s.)
- **A latent hazard to record, not to fix here:** `InferStruct` builds a struct's entries with
  member *types*, while `Nbe`'s `Term.Struct` evaluation stores the evaluated *definition* — two
  construction sites that disagree on the payload's kind. Nothing probed needs that reworked
  today; the predicate above does not depend on it.
