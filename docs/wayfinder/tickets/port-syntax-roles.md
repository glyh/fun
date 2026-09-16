---
title: "Port: syntactic roles, operators and rule forms"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: syntactic roles, operators and rule forms

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- The role machinery: a role is a binder in the binder table resolved by scope
  set (M7); base roles; `RoleConflict` / `OpenSuppliesRole`.
- Operator and fixity declarations, prefix/infix use, order groups
  (precedence is relative, brackets decide grouping) - the "infix operator …"
  (118) and enforest-error (113) walls, for the cases that declare their own
  operators; operators from the prelude stay "not ported yet".
- `syntax` rule forms (templates with typed holes `$x`, `$(t : Block)`),
  instantiation, declaration forms (`: Decl`), in blocks and modules (8 module
  items), and roles delivered by `open`/imports.
- Once roles exist, retire the Driver's blanket "enforest errors are not ported"
  rule (`Driver.cs`, `ponytail:` comment) for errors that no longer depend on
  unported roles; keep the runner honest while the *prelude's* roles are missing.
- Procedural `macro` definitions and calls are **out of scope** (wave 3, with the
  prelude's `Syntax` module).

## Decided rules to read first

`docs/wayfinder/topics/core-tt-domain-model-surface.md`,
`core-tt-domain-model-macros.md`, `docs/wayfinder/macro-system/`, domain model
I4c, glossary **Syntactic role**, **Template**, **Hole**, **Intro scope**,
**Use-site scope**; [templates-desugar-to-macros](templates-desugar-to-macros.md).
[brackets-decide-grouping](brackets-decide-grouping.md) is still **open**: port
what is decided there and stop at anything that is not.
