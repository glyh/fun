---
title: "Port: type-case, record and struct-type patterns, pattern synonyms"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: type-case, record and struct-type patterns, pattern synonyms

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- Type-case: `match (T) { I64 => …, Option(A) => … }` on a type, with branch
  refinement (18 cases blocked today), including the pattern heads that name type
  formers.
- Record patterns `P{x = p, y}` (11) and struct-type patterns `struct { x : p }` (9).
- Pattern synonyms: `pattern Name(params) = rhs` / `pub pattern` in blocks and
  modules, and their use.

## Decided rules to read first

[pattern-head-accepts-type-formers](pattern-head-accepts-type-formers.md),
[bare-constructor-pattern-resolves-by-name](bare-constructor-pattern-resolves-by-name.md)
(a bare head resolves like any name), the E11 note on type-case heads in
`Core.CPatNominalHead` (`core.ml`), `CLAUDE.md` "Pattern synonyms". Type-case
refinement's cost is a known prototype hotspot
([type-case-refinement-walks-whole-context](type-case-refinement-walks-whole-context.md)):
port the rule, not the walk. Effect branches belong to the effects fork.
