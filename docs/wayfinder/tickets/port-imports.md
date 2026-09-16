---
title: "Port: imports and compilation units"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: imports and compilation units

Wave 1 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `import "u"` of a `.fun` compilation unit; the conformance runner's
  `<name>.unit-<u>.fun` files (`Driver.Elaborate`'s `units` argument, today
  "imports not ported yet").
- A unit elaborates against the **base context**, not the importer's, and its
  *value* crosses the import, never its term (domain model I5, glossary
  **Base-anchored term**, **Transport**). Units are strict: no implicit prelude
  open. The base context binds `stdlib` as a name - the prelude is not ported,
  so a unit reaching it is "not ported yet".
- Import cycles are errors; a unit imported twice is elaborated once.
- Macros and syntax roles delivered by an import are **out of scope**.

## Decided rules to read first

- Domain model I5 in full, and
  [imported-module-elaboration-context](imported-module-elaboration-context.md),
  [module-level-open](module-level-open-strict-imported-modules.md).

## Target

Cases whose first blocker is "imports not ported yet" (24) or "the `import`
form" (3) and that need nothing else unported.

## Other forks

structs, match-enums, implicits, rec run concurrently.
