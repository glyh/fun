---
title: A pattern synonym cannot be declared in a block
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Decided (2026-09-17) that a block may declare a pattern synonym; the port accepts it, the prototype has no such statement.
assignee:
blocked_by:
---

# A pattern synonym cannot be declared in a block

Found while porting pattern synonyms to C# (2026-09-16).

## Decided (user, 2026-09-17): a block may declare one

Every other declaration — a value, a type, `rec`, `macro`, `syntax` — may appear in a
block and scopes over the rest of it; a pattern synonym is no different. The port
already accepts it; the OCaml prototype has no `pattern` block statement and fails
with `Enforest_util.Error("unexpected token in expression")`. **The port keeps it**;
the prototype keeps the gap.

## Conformance

```
{ P = enum { Pair(I64, Char) }; open P; pattern Swap(c, n) = Pair(n, c);
  match (Pair(1, 'x')) { Swap(a, b) => b } }
```

`values/pattern-synonym-in-block` (1), listed in
`test/conformance/prototype-divergences.txt`.

## Resolution (2026-09-20)

Closed after re-running both runners on `main @ d58af64`.
`dune test --root . test/conformance` reports `690 cases, 0 failed, 19 known
prototype divergences`, so `values/pattern-synonym-in-block` fails in the
prototype as listed (`Enforest_util.Error("unexpected token in expression")`); `cd
dotnet && dotnet run --project test/Fun.Conformance --no-build` passes it (not
among the 13 unrelated residue failures), so the port is correct.
