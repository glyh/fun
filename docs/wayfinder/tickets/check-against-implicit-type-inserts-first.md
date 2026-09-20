---
title: Checking a value against an implicit function type inserts metas before binding
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Decided (2026-09-16) that checking a value against an implicit function type binds the implicit parameter first; the port now passes the shared case.
assignee:
blocked_by:
---

# Checking a value against an implicit function type inserts metas before binding

Found by the C# port's follow-up verification (2026-09-16). **To be fixed in the C#
port only**; the OCaml prototype keeps the defect.

## Decided (user, 2026-09-16): accept it

```
id = fn[A : Type](a : A) { a };
g : [A : Type] -> A -> A = id;
```

must check: `[A : Type] -> A -> A` is exactly `id`'s type. When the *expected* type
is an implicit function type, checking first binds its implicit parameter (as
checking a lambda does), then checks the value in that scope — where the value's
own implicit arguments are inserted against the body. Today both the prototype and
the port insert `id`'s implicit argument first and are left comparing a
non-implicit function with an implicit type (a function-type mismatch).

## Conformance

A shared case with `g`'s use succeeding, listed in
`test/conformance/prototype-divergences.txt` once the port passes it.

## Resolution (2026-09-20)

The case landed as `values/check-against-implicit-type` and the port passes it. The
"Today both the prototype and the port insert ..." paragraph above is the
pre-fix state (2026-09-16); it is historical, not the current behaviour. Closed
after re-running both runners on `main @ d58af64`: `dune test --root .
test/conformance` reports `690 cases, 0 failed, 19 known prototype divergences`,
so `values/check-against-implicit-type` fails in the prototype as listed; `cd
dotnet && dotnet run --project test/Fun.Conformance --no-build` passes it (not
among the 13 unrelated residue failures), so the port is correct.
