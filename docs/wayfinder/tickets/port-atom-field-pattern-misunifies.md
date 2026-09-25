---
title: "Port: the atom-field-pattern ticket was a mis-diagnosis — the program was ill-typed"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 as a mis-diagnosis, the second in this area in one session. The program was ill-typed - its argument 5 does not match the atom 2, so y's type is Char and passing an I64 is correctly rejected. The pattern elaborates against the declared field type and the neighbours are all right; the one real defect was the message, which named neither atom, and that is fixed (6d98c3e, merged) with three conformance cases (0d82cea).
assignee:
blocked_by:
---

# Port: an atom pattern in a record field does not unify

> ## Resolution (2026-09-25) — closed as a mis-diagnosis
>
> **The rejection was correct; the message was the bug.** A fork diagnosed this by instrumenting
> the pattern path before changing anything, and found:
>
> - the atom field pattern **is** elaborated against the declared field type, `VAtomTy(I64)`, every
>   time — so there is no wrong-expected-type bug, which was this ticket's leading hypothesis;
> - the failing unify is at the **application** `f(5, 5)`: `y`'s domain is the type-level match,
>   which reduces to the `_ => Char` arm **because the atom `2` does not equal the scrutinee's
>   `g = 5`**, so `y : Char` and passing `5 : I64` is a genuine type error;
> - and the domain model says exactly that: `core-124`/`core-125` pin literal patterns matching by
>   **value** (`match (1) { 1 => 10, _ => 20 }` → `10`), and
>   `docs/wayfinder/topics/record-field-type-reflection.md` has the record-field reading.
>
> With the argument changed to satisfy the atom — `f(2, 5)` — the answer is `VALUE 5`, verified by
> the integrator, and the binder and constructor neighbours were never broken.
>
> **So the real defect was the error message**, which read `cannot unify VAtomTy with VAtomTy` and
> named neither side. Fixed by adding `VAtomTy` and `VAtom` to `Unify.Describe` (the helper the
> nominal path already used), so it now reads:
>
> ```text
> ELAB type mismatch: cannot unify VAtomTy(Char) with VAtomTy(I64)
> ```
>
> That is the shape of message a person can act on, and it is what turned a "wrong semantics"
> report into a five-minute diagnosis.
>
> **Tests** (`0d82cea`): `values/record-field-binder-pattern`, `-constructor-pattern`,
> `-atom-pattern` — all `.expect 5`, the atom one using `f(2, 5)`. The fork checked the neighbours
> as the brief asked (an atom against a `Char` field, inside a tuple, inside a nested record — each
> `10`, each correctly failing when unequal) and found no wrong-expected-type bug in any position.
>
> **Counts**: port `773 cases, 0 failed` and `185/185` xUnit.
>
> **The lesson this ticket is the second half of.** The previous ticket in this area
> ([a nested field pattern](port-nested-field-patterns.md)) was wrong because the *program* was
> malformed; this one was wrong because the program was *ill-typed* — I varied the pattern without
> varying the argument, so `f(5, 5)` stopped satisfying the pattern the moment it became an atom.
> A probe is evidence only if the program is valid **and the pattern actually matches the value**;
> both of my errors survived review because a rejection looks like a rejection.
