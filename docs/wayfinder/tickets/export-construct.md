---
title: `export` — re-export a module's or enum's members
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-16
assignee:
blocked_by:
---

# `export` — re-export a module's or enum's members

Decided while grilling [adts-as-let-bindings](adts-as-let-bindings.md) (see its
"Revised (2026-09-16)" section): `open` never re-exports and `pub open` is
rejected; `export M` / `export M.{a, b}` adds `M`'s public members to the
enclosing module's members without opening them locally. Any module or enum.
Clashes with own members or other exports are errors. Needed by the `type`
macro (step 3). Interacts with impls (does `export M` re-export `M`'s public
impls for trait resolution after `open` of the exporter? — decide when
implementing; stop and ask if not obvious) and roles (syntax forms / order groups
exported the same way as values).

## Implemented (2026-09-16, branch `type-macro`) — impls open

- `export M` / `export M.{a, b}` (keyword `export`, module items only) adds `M`'s
  public members — a module's fields, or an enum's constructors — as public
  members of the enclosing module, bound under keys nothing spells, so nothing is
  opened locally. `Syntax.ExportBinding { m; names }`, reflected `DeclExport`.
- Clashes (with an own public member or another export, either order) are
  `ExportClash`; an unknown selected name is `ExportUnknownMember`.
- A unit's public roles (syntax forms, order groups) are re-exported with its
  values when `M` denotes a unit (`Ops = import "ops"; export Ops`).
- **Not uniform, needs a decision:** impls. An anonymous public impl has no
  projection a re-export could bind (open reaches it only positionally), so
  `export M` of a module with public impls is `ExportImpls` today. Options: re-
  export named impls only (anonymous → error); give an export an impl slot that
  projects the impl positionally; or never re-export impls.
- Also not re-exported: a unit's procedural macros; a role selected by name
  (`export Ops.{answer}` where `answer` is only a role) is `ExportUnknownMember`.
