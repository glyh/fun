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
