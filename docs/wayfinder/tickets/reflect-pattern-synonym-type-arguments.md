---
title: Reflecting a pattern-synonym use that supplies its type arguments
parent: ../quill-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-28
resolution: Closed 2026-09-28. The refusal is gone: `Path` carries the supplied types and the round trip is the identity. Landed as `f69ed8f` — a turn-limited fork's tree, saved by pi's cleanup and gated by the integrator, because the fork never ran the full suite. Suite `904` → **`905` cases, 0 failed**; xUnit `206`; no expectation changed. **This was the last live unported path in `src/`** — the only one a macro author could reach.
assignee:
blocked_by: []
---

# Reflecting a pattern-synonym use that supplies its type arguments

Found by the [unported-path re-sweep](port-unported-path-audit.md) on 2026-09-27, the
day the feature it reflects landed
([pattern-synonym-type-parameters](pattern-synonym-type-parameters.md), `3e60e20`), and
reproduced by the integrator. It is the **only live unported path left** in `src/`: 11
`NotImplementedException` sites remain, 8 unreachable, 1 deferred by ruling, 2 residues
with no reaching program — and this one, which a macro author can hit.

## The gap

The feature works. Only *reflection* of it does not:

```quill
# works — VALUE 1
{ M = module { pub pattern Two(a, b) = (a, b) };
  match ((1, True)) { M.Two[I64, Bool](p, q) => p } }

# refused — a macro whose output contains the same form
{ M = module { pub pattern Two(a, b) = (a, b) };
  macro use(x) { x };
  use(match ((1, True)) { M.Two[I64, Bool](p, q) => p }) }
```

```text
ELAB not ported: not ported yet: reflecting the type arguments a pattern synonym use supplies
```

## Why — and what the fix is

`src/Quill.Compiler/Reflection.cs:243-245` refuses because the reflected `Path` ADT has no
slot for the supplied types:

```quill
pub PathChoice = struct {opens: List(String); fallback: Option(String)};
pub Path = struct {head: Id; members: List(String); head_choice: Option(PathChoice)};
```

A pattern-synonym use that supplies its implicit type parameters is an `Syntax.Ap` whose
explicitness is `Implicit` wrapping a `FieldAccess` chain, and `Syntax.Path` cannot carry
the types. So this is the "**a new reflected `Syntax` field**" job that `CLAUDE.md`
describes in full, and its five steps are the checklist: the prelude declaration plus any
builders, the syntax-nominals registry the macro evaluator builds, the wrap/unwrap pair,
**every** construction site (the round trip must be the identity), and the pattern
synonyms that make the new shape resolvable by constructor name.

Two constraints worth stating up front:

- **The marker must stay a `NotImplementedException`.** It is an unported path, not a
  language error; `CLAUDE.md` makes the two deliberately distinguishable so a refusal can
  never satisfy a case expecting `error`.
- **Adding a field ripples.** Per the same file: "search for the variant name and check
  every match site preserves the new field or explicitly drops it with a reason" — the
  fields `head`/`members`/`head_choice` are read in reflection both ways, in the one
  traversal every scope, intro, rename and syntax-form fill goes through, and in a syntax
  form's rule templates.

## The case it needs

The program above, in `test/conformance/cases/macros/` — a macro whose *output* reflects a
synonym use carrying type arguments, asserting the reflected form survives (the round
trip is the identity) rather than only that it stops refusing. The suite is green today
precisely because no case reaches the site, which is the same blindness the original audit
reported at 17 gaps and is now down to this one.

## Not blocking, and adjacent

Nothing needs this: `use(match …)` is only reachable by a macro author who reflects a
parameterised synonym use, and the unparameterised form reflects fine. The deferred typed
operator macro (`Expander.Macros.cs:306`) is a different site, ruled not-a-gap in
2026-09-24 because the prototype hangs on it.

## Reading

- [the unported-path audit](port-unported-path-audit.md) — the re-sweep that found it, and
  the verdict table for the other ten sites
- [a pattern synonym's generalized types](pattern-synonym-type-parameters.md) — the
  feature, including the deviation it recorded from its ruling's route
- `src/Quill.Compiler/Reflection.cs:238-260` — the refusal and the shape it cannot build
- `std/bootstrap.qll:39-40` — `PathChoice` and `Path`
- `CLAUDE.md` — "Adding a new reflected Syntax ADT" and "Reflection and scope-addition:
  preserve ALL fields", which are this ticket's method

## Landed 2026-09-28 (`f69ed8f`)

`Path` gained `type_args : List(Expr)`, mirroring the convention `DeclImpl` already uses (its
argument is reflected as a one-element `List(Expr)`). It could not stay a `struct`: a rec group
cannot mix structs and enums (measured — `a rec … and … group holds enums, struct types or
functions, not a mix`; a forward reference fails too, `unbound variable: B`), so `Path` became a
one-constructor enum `MkPath(Id, List(String), Option(PathChoice), List(Expr))` inside the `Expr`
rec group. **Ruled acceptable by the integrator**: the enum form is forced by the language's own
rec-group rule, `Reflection.cs` peels the implicit `Ap`s into the new field and re-applies them
on read-back, and the gate shows nothing observable moved — `905` cases, 0 failed, xUnit `206`,
no expectation changed.

Files: `std/bootstrap.qll`, `src/Quill.Kernel/PreludeAbi.cs`, `src/Quill.Compiler/Reflection.cs`, and
the case `test/conformance/cases/macros/pattern-synonym-type-args-round-trip.{quill,expect}`.

The fork ran out of turns before running the suite, so its tree was **uncommitted** when it
stopped; pi's cleanup saved it (`pi-agent: Reflect synonym type arguments`) and the counts above
were taken at the merge. Its own evidence — the ticket's program now `VALUE 1`, a load-bearing
program `VALUE 7`, and a control that errors if reflection drops the arguments — is in its
report.
