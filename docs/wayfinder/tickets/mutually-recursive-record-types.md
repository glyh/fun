---
title: Mutually-recursive record type declarations
parent: ../fun-design-map.md
status: open
assignee:
blocked_by:
---

# Mutually-recursive record type declarations

## Question

Two (or more) record declarations referencing each other — `type A = { b : B }`
together with `type B = { a : A }` — so they elaborate as a group.

## Context

- Split out of
  [mutually-recursive-nominal-types.md](mutually-recursive-nominal-types.md)
  during grilling: that ticket chose **nominal-only** `and` chains. Records join no
  `and` chain yet, and a nominal payload referencing a record declared later
  (`type A = MkA(S)` with `S` a record below) still errors — this ticket is the
  deferred remainder.
- Harder than the nominal case: a record declaration compiles to a lambda value
  (structural `VStruct` over `VSelfType`). Mutual A↔B is a **value-level knot
  between two closures** — the nominal placeholder trick does not transfer because
  it leans on placeholder and finished type sharing one `NominalId`; a record value
  has no such identity.
- Touches the open struct-elaborator questions on the map (e.g. struct open over
  `con_fields`), so it is deliberately parked until those settle.
- Real user impact once hit: AST pairs where one side is a record, mixed
  nominal↔record forward references.

## Design questions (to grill)

- Knot mechanism for mutually recursive record-type values.
- Whether mixed nominal↔record chains stay excluded or open up.
- Interaction with the record self-reference rewrite (`rewrite_record_self_refs`).

## Resolution

_Unresolved._
