---
title: panic with an unknown message fails while checking
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
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
