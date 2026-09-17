---
title: Opening a handle on a unit is not an open of that unit
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Opening a handle on a unit is not an open of that unit

Found by the C# port's interleaving fork (2026-09-17). **Fixed in the C# port only**;
the OCaml prototype keeps the defect.

## Defect

```
-- unit v
pub three = 3;
pub syntax three_of { three_of => three }

-- unit w
V = import "v";
open V;
pub x = three_of;

-- program
{ W = import "w"; W.x }
```

gives 3 in the port: `V` is a handle on unit `v`, so `open V` is an open of that unit
(labelled `unit:v`), and the id `three` that `v`'s syntax form introduces resolves to
`v`'s member through that open (glossary **Open**, **Open choice**; M7). The prototype
treats only `open (import "v")` as a unit open; through the handle, the introduced
`three` is `UnboundVariable "three"`.

## Conformance

`imports/unit-handle-open-form-member` (3), listed in
`test/conformance/prototype-divergences.txt`.
