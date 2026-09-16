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

## Progress (2026-09-16)

Merged from `port/patterns` (`5cf6b9c`, `f076fa5`, merges `796faa6`, `47411cd`; one
conflict in `Core.cs`: both forks had added the same environment update as
`Replace` and `With`, kept once as `Replace`). Record patterns `P {x = p, y; _}`
(missing/unknown/duplicate fields are errors); type-case on the atom types with a
required fallback, refining a matched type variable in context and expected type;
struct-type patterns `struct { x : p; _ }`; nominal type heads `Opt(I64)`/`Opt(x)`,
formers included, matched by instance (E11), arms run in order after the decision
tree checks exhaustiveness. Newly passing: values core-156; elaborate elab-188–190,
elab-044, 046, 047; shared `values/type-case-refines-variable` (7),
`values/type-case-struct-field-type` (3).

**Prototype defect** (ticketed by the integrator):
[type-case-former-head-arity-from-template](type-case-former-head-arity-from-template.md).

**Open (user):** pattern synonyms are not ported. The prototype substitutes a
synonym's arguments into its right-hand side by position, not by parameter name,
and a synonym reached through `open` is `UnknownConstructor`. Undecided: do
arguments bind by name, and does a synonym resolve through its binder or an open
like any other name?

**Follow-up:** matching a nominal head evaluates the head term with a nested
`Eval` (`ponytail:` in `Nbe.Patterns.cs`) rather than a `Kont` frame; its depth is
the head term's, not the program's call depth.
