---
title: "Port: match, patterns and (non-recursive) enums"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: match, patterns and (non-recursive) enums

Wave 1 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope, in this order

1. `match (e) { p => e, … }` value branches: wildcard, binder, atom, tuple and or
   patterns; pattern compilation to a decision tree
   (`lib/semantic/match/`); exhaustiveness as the prototype checks it.
2. Nominal ADTs from `enum { Red, Some(A) }` bound non-recursively, and
   parameterised ones `Option = fn(A : Type) { enum { Some(A), None } }`:
   constructors as members (`Color.Red`, `open Color`), constructor values, and
   constructor patterns resolved through the pattern head's binder, never by
   spelling.
3. The conformance runner's `Driver.Describe` already has the constructor-name
   rule to add (`VCon` → its name).

Effect branches, type-case, pattern synonyms, records in patterns and `rec`
enums are **out of scope**: stop at them.

## Decided rules to read first

- Nominal identity is applicative by purity (E11):
  [nominal-identity-applicative-by-purity](nominal-identity-applicative-by-purity.md).
  A module's stamp slot (see the `ponytail:` note in `Elaborator.InferModule`)
  arrives with nominals; add it through the slot list.
- [constructor lookup matches the type name](constructor-lookup-matches-type-name.md),
  [pattern heads accept type formers](pattern-head-accepts-type-formers.md),
  [dotted paths first match](dotted-paths-first-match.md) (closed rules).
- `CLAUDE.md` "Elaborator: constructor resolution phases".

## Target

Cases whose first blocker is "the `match` form" (43) or "the `enum` form" (16)
and that need nothing else unported.

## Other forks

structs, implicits, rec, imports run concurrently. `rec T = enum { … }` belongs to
a later ticket once this and the rec fork are merged.
