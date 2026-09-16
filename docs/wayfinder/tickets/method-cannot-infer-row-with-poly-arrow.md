---
title: A method cannot infer its row with ~>
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
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
