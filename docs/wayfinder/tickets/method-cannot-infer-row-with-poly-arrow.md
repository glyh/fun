---
title: A method cannot infer its row with ~>
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: A method's final `~> T` infers its row as a definition's does; fixed in the C# port only.
assignee:
blocked_by:
---

# A method cannot infer its row with `~>`

Raised by the C# port's effects follow-ups fork (2026-09-16).

## Decided (user, 2026-09-17): allow it

```
S = struct { v : I64; pub method run() ~> I64 { … } }
```

A method may write its result as `~> T`: its row is inferred from what its body
performs, as a definition's final `~>` arrow infers it (`fn(…) ~> T { … }`). The
prototype rejects it with `PolyArrowOutsideSignature`, and so does the port today:
**to be fixed in the C# port only**; the prototype keeps the defect.

## Conformance

A shared case where a method written `~> T` performs and is handled, listed in
`test/conformance/prototype-divergences.txt`.

## Fixed in the port (2026-09-17)

Merged from `port/method-poly-arrow` (`d731f6a`, merge `7948d44`):
`Elaborator.Structs.MethodRow` reads a method's final `~> T` as a definition's.
`values/method-poly-arrow-infers-row` (5) passes in C# (and fails with the old error
when the fix is removed); the prototype fails it with `PolyArrowOutsideSignature`,
and it is listed in `prototype-divergences.txt`. Not covered by the ruling: `~>` in a
method's *parameter* types still raises the same error.

## Resolution (2026-09-20)

Closed after re-running both runners on `main @ d58af64`.
`dune test --root . test/conformance` reports `690 cases, 0 failed, 19 known
prototype divergences`, so `values/method-poly-arrow-infers-row` fails in the
prototype as listed (`PolyArrowOutsideSignature`); `cd dotnet && dotnet run
--project test/Fun.Conformance --no-build` passes it (not among the 13 unrelated
residue failures), so the port is correct.
