---
title: "Port: export"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: export

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `export M` and `export M.{a, b}` as module items: a module's public members, and
  an enum's constructors, become members of the enclosing module, opening nothing
  locally; an unpublished `export` is nothing until `pub` publishes it; clashes are
  errors (`ExportClash`) (3 module items, and stage 1 of the prelude).
- Re-exporting roles and impls is left to the roles and traits forks: stop there.

## Decided rules to read first

[export-construct](export-construct.md) (closed); `STATUS.md` "`export`;
block-level enum groups".

## Resolution (2026-09-16)

Merged from `port/export` (`53c23dc`). `export M` / `export M.{a, b}` publish a
module's public members or an enum's constructors as public members of the
enclosing module through the slot list, under keys nothing spells (an export opens
nothing locally); a selection exports only what it names, an unknown name is an
error, `pub export` is rejected; clashes are errors in either order and between
two exports, except a constructor exported from the enum sharing its name (I3); a
private member never clashes. 11 new shared cases, all agreeing with the
prototype. C# 103/632; xUnit 74.

**Follow-ups:**
- **Unverified deviation:** the constructor-named-like-its-enum exemption also
  applies when the exported module is reached through an open; the prototype checks
  only a direct variable. No case exercises it; reproduce and decide if one does.
- Still blocked elsewhere: `core-309` (`+`), `core-311` (`rec` enums), `core-312`,
  `core-313` (impls). Re-exporting roles and impls, and unpublished exports (only a
  macro writes one), are left to the roles, traits and macro work.
