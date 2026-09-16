---
title: "Port: export"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
