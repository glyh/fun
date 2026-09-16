---
title: "Port: primitives"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: primitives

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- The base context's primitives: one declaration per primitive (name, type,
  evaluation), as decided in
  [unify-primitive-declaration](unify-primitive-declaration.md) — the prototype's
  table is `lib/backend/interp/nbe_prim.ml` (`declarations`) and is bound into the
  base context by `Elab_entry.stage1_ctx`.
- Each primitive is a defined entry whose value is a neutral headed by the
  primitive; applying it to atom arguments reduces (the prototype's
  `try_prim_reduce`), otherwise it stays stuck. I64 arithmetic is **checked**:
  overflow is a genuine error, as the ticket writes down.
- Everything the prelude's two stages call (`dotnet/std/stage1.fun`,
  `stage2.fun`: `eq_string`, `panic`, arithmetic and comparison, char and string
  operations) except the ones that need the macro runtime (`expand_block`,
  `expand_decls`): those raise "not ported yet".
- The Driver's prelude rule keeps holding: a name the base context now has is
  found; a prelude name is still "not ported yet".

## Target

Base-context names reachable from programs, and whatever conformance cases call
primitives directly; mostly this unblocks the prelude slice. xUnit for reduction,
stuck applications and overflow.
