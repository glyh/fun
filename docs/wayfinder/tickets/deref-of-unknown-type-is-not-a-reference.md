---
title: A reference operation on a value of unknown type is "not a reference"
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Reading or writing a value of unknown type unifies it with a fresh `Ref`, so a function over it checks; the port already behaves this way.
assignee:
blocked_by:
---

# A reference operation on a value of unknown type is "not a reference"

Found by the C# port's follow-up verification (2026-09-16). **Fixed in the C# port
only** (it already behaves this way); the OCaml prototype keeps the defect.

## Decided (user, 2026-09-16): infer that it is a reference

```
f = fn(r) { deref(r) };
```

Reading or writing a value whose type is still unknown unifies that type with a
fresh `Ref(?h, ?A)`, so `f` checks. This is the same rule as applying a value of
unknown type, which unifies it with a fresh arrow. The prototype raises
`ApplyingNonFunction`.

## Conformance

A shared case where such a function elaborates and runs, listed in
`test/conformance/prototype-divergences.txt`.

## Resolution (2026-09-20)

The case landed as `values/deref-infers-reference`. Closed after re-running both
runners on `main @ d58af64`: `dune test --root . test/conformance` reports `690
cases, 0 failed, 19 known prototype divergences`, so `values/deref-infers-reference`
fails in the prototype as listed (`ApplyingNonFunction`); `cd dotnet && dotnet run
--project test/Fun.Conformance --no-build` passes it (not among the 13 unrelated
residue failures), so the port is correct.
