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

## Progress (2026-09-16)

Merged from `port/roles` (`ca6a4c5`, `f67f90d`, `547b0a3`, merge `66f8dd3`).
`syntax` forms with typed holes (`Id`, `Pattern`, `Block`, `Decl`, `List(Decl)`),
declaration forms in modules, structs and blocks (`pub` publishes what they
return); `infix`/`prefix` fixity declarations and operator templates; `order`
groups (transitive, `assoc(left|right|none)`, `weakest`, cycles rejected); base
roles `assignment`, `<-`, `~>`; roles resolve by scope set, and a role conflicts
with a value binder of its name unless it came from a form's expansion or is
fixity-only for an existing value; every use goes through one hygiene contract
(fresh use-site and intro scopes, fill, expand in place). Newly passing: macros
core-189, 203, 224, 225, 245, 263–269, 273, 278–281, 283, 286–288, 290, 295, 298,
303, 307; shared `elaborate/role-conflicts-with-value-binder`,
`order-groups-unrelated`, `order-assoc-none-does-not-chain` (errors, reported
through `RoleException`), `values/order-groups-transitive`,
`order-group-left-assoc`. C# 161/647; xUnit 88.

**Not done (ticket stays open):**
- **Roles through `open`/imports** need a unit's roles from `Loader`
  (Fun.Compiler), which Fun.Expand cannot reference: this is the injected
  `IMacroRuntime` from the port ticket's expander-callbacks decision. Build it
  with procedural macros (wave 3).
- `OpenSuppliesRole`, `Std.additive` group paths, procedural operator macros.
- The Driver's blanket "enforest errors are not ported" rule is narrowed only for
  `RoleException`; widen as prelude roles land.

**Stopgaps (`ponytail:`):** the reading environment is ambient (thread-static) to
avoid signature churn during parallel work; `Syntax.AddScope` duplicates the new
`Syntax.Map` traversal. Unify both once the wave's merges settle.
