---
title: A module's public members are unique
parent: review-recorded-divergences.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-21
resolution: Implemented in the C# port (label-keyed, public entries only) with its cases; the prototype keeps the defect and prototype-divergences.txt grew to 22 lines.
assignee:
blocked_by:
---

# A module's public members are unique

Decided 2026-09-21 while reviewing the recorded divergences — decision 3 of
[review the recorded divergences](review-recorded-divergences.md). **Port-only**: the C#
port implements the rule, the OCaml prototype keeps the defect, and the affected cases
are recorded in `test/conformance/prototype-divergences.txt`.

## The rule

**A container's public member entries must have distinct names.**

| program | before | after |
|---|---|---|
| `module { pub x = 1; pub x = 2 }` | `M.x` = `2` | **error** |
| `module { pub x = 1; x = 2 }` | legal; outside `x` = 1, the body's `x` = 2 | **unchanged** |
| `module { open N; pub x = 2 }` (N has `x`) | legal, = `2` | **unchanged** |
| `open A; open B; x` (both have `x`) | `2` (last open wins) | **unchanged** |
| `enum { A(I64), A }` | accepted; the first `A` is unreachable by any spelling | **error** |
| one `Decl` capture spliced twice, its decls public | `1` (last wins) | **error** |
| `pub rec T = enum { T, U }` | legal (one written `pub`; a nominal's ctors are not members of the container) | **unchanged** |
| `struct { x : I64; fn x(p) { 1 } }` | legal | **unchanged** (a struct literal has its own fold) |
| `export E; pub A = 7` (E exports a ctor `A`) | `ExportClash "A"` | **unchanged** |

Private bindings do not count, so `pub x = 1; x = 2` stays legal: the interface holds one
public `x` while the body sees the shadowing private one — measured
`(M.x, M.y)` = `(1, 2)` for `pub x = 1; x = 2; pub y = x`. That is the deliberate scope:
the rule reads the **public entries**, not every written binding.

`export` keeps its own rule unchanged: a name already exported, or an export taking a name
already public, is `ExportClash`, **including** the carve-out for a constructor sharing the
name of the enum it is exported from (`N = module { pub rec T = enum { T, U }; export T }`
elaborates, and `N.T.T` / `N.T.U` reach the two constructors).

## Why public entries, and why the label

- **Entries, not written bindings**: the export path already thinks in public entries
  (`export E` turns a nominal's constructors into `Public` module entries), and the
  exemption is keyed on syntax — "the name equals the written label of the export's module
  expression" — not on an entry's kind, since a hoisted constructor is `Public` and
  indistinguishable from a `pub` value in the table.
- **Label, not resolved name**: the alternative — keying on each binding's resolved name,
  so a declaration spliced twice would collapse to one member — was rejected. It makes the
  verdict depend on capture provenance the source does not show, and it contradicts the
  ruling that a duplicated declaration is an error rather than a silent last-wins (the same
  reason `enum { A, A }` was closed). One declaration spliced twice is therefore **two
  public bindings**, hence a duplicate.

## Implementation (C#, landed 2026-09-21)

- `dotnet/src/Fun.Compiler/Elaborator.Export.cs`, `ExportClashes.Check` — the `_seen`
  trigger now applies to every binding and reports
  `duplicate member: \`x\` is already public` when the binding is not an export; the
  `_exported` trigger and the `source` carve-out are untouched, so
  `export clash: \`x\` is already a member` keeps meaning an export collided with the
  interface. The class doc comment now states the rule.
- `dotnet/src/Fun.Compiler/Elaborator.Enum.cs`, `InferEnum` — rejects a repeated
  constructor name. `Elaborator.RejectDuplicates` gained an optional subject
  (`duplicate <subject> \`x\``), so the enum says *constructor* while its five existing
  call sites still say *field*.
- Units need nothing: a unit parses to `Syntax.Module` (`enforest.ml:1581`) and goes through
  `InferModuleBindings` (`Elaborator.cs`), the same fold. Struct literals are a separate fold
  and were deliberately left alone.
- xUnit: `dotnet/test/Fun.Tests/MemberTests.cs` — four tests (repeated public member,
  repeated constructor, private shadow allowed, an open's name shadowed by a `pub`).

## Conformance (as landed)

The divergence file went from 20 lines to **22**: one deleted, three added.

- `values/core-121` — rewritten to assert the error; **line added** (the prototype
  answers `2`).
- `values/signature-check-takes-last-member` — `.expect` → `error`; **line deleted**. The
  prototype already errors there (`CannotUnify(I64 vs Char)`, measured), so the case stops
  carrying a rule; the rule is pinned by `core-121`.
- `elaborate/duplicate-enum-constructor` (new) — **line added** (the prototype accepts
  `enum { A(I64), A }`).
- `macros/spliced-public-decl-is-a-duplicate` (new) — one `Decl` capture spliced twice with
  public decls; **line added** (the prototype answers `1`).
- `values/interface-sees-the-public-member` (`1`) and `values/private-shadow-is-not-a-duplicate`
  (`21`) — C2's boundary: the interface holds the public member, the body sees the private
  one. Both implementations agree, so neither is listed.
- `elaborate/open-name-shadowed-by-pub` (`2`) — an open's names are not members of the
  container. Both agree.
- Two existing cases changed because the port now rejects them, both user-written
  duplicates rather than a fork: `values/elab-021` named two impls `eq_I` → renamed
  `eq_bool` / `eq_i64` (its property — a named impl is a member — is preserved); and
  `macros/core-321` spliced a *public* `Decl` twice → rewritten to splice private decls and
  read them through one public member, so its double-splice property survives while both
  implementations agree at `2`.

**The predicted blast radius was wrong.** A file-level scan said "exactly two cases"; it
skipped `pub impl` and could not see macro-generated bindings. `values/elab-021` and
`macros/core-321` were found only by running both suites — the reason the cases and the code
land in one change.

Runs (2026-09-21): port `718 cases, 0 failed`; prototype
`718 cases, 0 failed, 22 known prototype divergences`; xUnit `182 passed`; `dune test` green.

## Docs

- `docs/wayfinder/topics/core-tt-domain-model.md` **I3** and **I4** updated: the
  "last member of that name" clause no longer governs containers.
- `docs/STATUS.md` and the resolution text of the closed
  [signature-check-takes-first-member](signature-check-takes-first-member.md) and
  [dotted-paths-first-match](dotted-paths-first-match.md) note that the rule supersedes
  their "last match" for containers.
