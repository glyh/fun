---
title: "Port: references"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: references

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `ref e`, `deref r`, `r <- v` (the `<-` base role is merged, as a role with no
  elaboration yet), the `Ref(h, A)` type whose heap `h` is an implicit argument
  (surface `Ref(A)` takes a fresh one per use), and the base context's `Ref` and
  `Mutate` entries (see the prototype's `Elab_entry.stage1_ctx`).
- Refs in effect rows: an operation on a reference performs `Mutate(r)`; rows that
  name a parameter (`->{Mutate(r)}`, the effects fork's "rows that name their own
  parameter"); an unhandled `Mutate` names the reference; a `let` or block whose
  mutation is private discharges its local heaps (one pass over older metas).
- E6 through references: storing a closure that names a handled effect into a
  reference not local to the match is an error.
- Evaluation: a reference is a mutable cell; `ref`/`deref`/`<-` are machine
  frames, never native recursion.
- Rows on method results that mention a reference only if the effects follow-ups
  have not taken them (they are queued after this ticket): stop at method rows.

## Decided rules to read first

[refs-in-effect-rows](refs-in-effect-rows.md) (closed),
`docs/wayfinder/topics/core-tt-domain-model-effects.md`, the effects fork's merge
record in [port-effects](port-effects.md), `STATUS.md` "Effects follow-ups" (refs).

## Target

The 30 cases blocked on "the `ref` form", 3 on `deref`, 1 on `<-`, that need
nothing else unported.
