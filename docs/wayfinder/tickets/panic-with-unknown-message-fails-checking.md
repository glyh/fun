---
title: panic with an unknown message fails while checking
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: `panic` stays stuck until its arguments are values, so an unknown-message `panic` at a type checks; the port already behaves this way.
assignee:
blocked_by:
---

# `panic` with an unknown message fails while checking

Found by the C# port's follow-up verification (2026-09-16). **Fixed in the C# port
only** (it already behaves this way); the OCaml prototype keeps the defect.

## Decided (user, 2026-09-16): it stays unevaluated

```
f = fn(s : String, x : panic[Type](s)) { x };
```

`panic` is a primitive, and a primitive reduces only once its arguments are known
values, otherwise its application stays stuck. With `s` a parameter, `panic[Type](s)`
is a stuck type, so `f` checks; it panics only if the type is computed with a known
message. The prototype fails while checking, making `panic` the one primitive that
does not wait for its arguments.

## Conformance

A shared case where `f` elaborates (and is not called), listed in
`test/conformance/prototype-divergences.txt`.

## Resolution (2026-09-20)

The case landed as `values/panic-unknown-message-in-type`. Closed after
re-running both runners on `main @ d58af64`: `dune test --root .
test/conformance` reports `690 cases, 0 failed, 19 known prototype divergences`,
so `values/panic-unknown-message-in-type` fails in the prototype as listed; `cd
dotnet && dotnet run --project test/Fun.Conformance --no-build` passes it (not
among the 13 unrelated residue failures), so the port is correct.
