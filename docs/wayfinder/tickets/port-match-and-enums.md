---
title: "Port: match, patterns and (non-recursive) enums"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-16)

Merged from `port/match-enums` (`bf735cd`, `18e4fd8`, merges `51a1c01`, `f4301a2`,
`59d8230`). A `match` compiles once, at elaboration, to a decision tree over the
scrutinee's type; exhaustiveness is a result naming a missing value; the
evaluator walks the tree in a `Kont` frame. Enums are nominals whose identity is
the declaration plus its captures' values, compared by conversion (E11); their
constructors are members (`T.C`, `open T`), type formers included; a constructor
pattern's head resolves through its binder. `Elaborator.Rec.cs` now detects
recursive types with type patterns. Newly passing, each checked: values core-061,
108, 124–126, 128–133, 136, elab-153, 154, 156, 157; errors elab-168 (Char/I64),
elab-170, 171 (non-exhaustive); ok elab-158. C# 89/618; xUnit 73.

**Open question (user):** should a bare constructor pattern resolve through its
binder, or by name among the scrutinee type's constructors? The prototype does the
latter: `{ Color = enum { Red, Green }; match (Color.Green) { Red => 1, Green => 2 } }`
gives 2 with no `open Color`, and with `Red = 5` in scope `Red` is still the
constructor. The port raises "not ported yet" there until decided.

**Not ported, marked so:** stuck matches, type-case, record and struct-type
patterns, effect branches, `rec` enums, the generative module stamp, and an enum
declared in a body that also holds structs, signatures, record construction or
`rec` groups.
