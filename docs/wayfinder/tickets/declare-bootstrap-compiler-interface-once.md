---
title: Declare the bootstrap↔compiler interface once
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by: []
---

# Declare the bootstrap↔compiler interface once

## Question

The compiler reaches into the prelude's `Syntax` module (and `Bool`/`Option`/`List`)
through string literals scattered across five files. The **mechanism** is decided:
declare the interface once in C# and verify it eagerly when the prelude loads.
What is **not** settled is the *shape* of that declaration — because measuring it
showed the interface is far larger than the first estimate, and that part of it
drifts **silently** today.

## Decided (user, 2026-09-26)

**Declare once in C#, verify at load.** One declaration in the compiler, and
`Prelude.Load` resolves all of it eagerly, so a prelude rename fails immediately
with one message naming the member and its file. Consumers stop spelling names
individually.

Chosen over three alternatives, recorded so they are not re-proposed blind:

| Route | Verdict |
| --- | --- |
| Generate the C# names from `bootstrap.fun` | **Deferred.** It would only supply *spellings*, not the selection (below). Also blocked mechanically: every project is `net10.0` and a Roslyn source generator must be `netstandard2.0`, so it cannot reference `Fun.Expand`; it would have to be an MSBuild `Exec` of a console tool. |
| Mark the ABI in the prelude itself (new surface syntax) | **Rejected for now.** The only route that makes `.fun` authoritative for names *and* content — but it is a language change made for the compiler's convenience, and the map's fog cautions against exactly that. |
| Leave it, document it | **Rejected.** See the silent-drift tier below. |

**Why `.fun` cannot be the sole source.** `bootstrap.fun` supplies spellings. It
cannot supply the interface, because the interface is a fact about the compiler's
*usage*: which names matter, in which role (nominal type vs structural member vs
constructor-read-by-name), that `Bool`/`Option`/`List` live outside the `Syntax`
module, that the unit paths are ABI, and that `Type` is spelled by C# but belongs
to the elaborator, not to std (`Elaborator.cs:151`). The declaration therefore
lives on the compiler side and the prelude is checked against it. Accepted
consequence: renaming in the prelude becomes a loud *failure*, not a compile error.

## The measurement that resized this (2026-09-26)

- **~201 distinct quoted identifiers in `Reflection.cs` alone**: ~50 constructor
  names passed to `Con(...)` (65 `Con(` call sites), ~44 type and module names,
  and **15 struct field names** (`start_byte`, `end_byte`, `start_line`,
  `start_col`, `end_line`, `end_col`, `file`, `head`, `members`, `opens`,
  `fallback`, `head_choice`, `name`, `span`, `scope`).
- Sites by shape, measured: `Nominal(...)`/`Member(...)` 38 (`Reflection.cs`);
  token-spelling matches 17 (`Enforest.Roles.cs`); constructor-name matches 8
  (`QuoteHoles.cs`); `Name = "..."` comparisons 6 each in
  `Expander.Macros.cs` and `Enforest.Macros.cs`.

**Three tiers of drift, not one:**

1. **Loud today — type names.** `Reflection.Nominal`/`Member` throw
   (`Reflection.cs:100-104`), and `ReflectionTests` triggers them, so renaming a
   *type* in the prelude already fails, with a decent message.
2. **Silent today — constructor names and struct fields.** `Value.VCon` carries a
   name with no check against its nominal (`Core.Enum.cs:77`), and `Value.VRecord`
   is a string-keyed field list (`Core.Structs.cs:33`). So renaming a constructor
   or a field is caught by nothing, at load or in any test: it produces a value
   whose key no longer matches. Concrete sites: `Span` (`Reflection.cs:133-138`)
   and `Id` (`:142`).
3. **Already single-sourced — unit paths.** `Prelude.Path`, `Stage1Path`,
   `Binding` are one place each; the new declaration should reference them, not
   restate them.

This is the size that makes "declare it all as a table" a **mirror** rather than a
simplification, which is the fork below.

## The fork this ticket exists to settle

1. **Declare all of it.** `PreludeAbi.cs` holds a schema — the module name, each
   type's constructor set with arities, each struct's field list, the three
   prelude nominals — every one of the five files consumes it, and `Prelude.Load`
   verifies the prelude's *shape* against it. One spelling, loud failure, and it
   catches constructor/field/arity drift that nothing catches today. Cost: a
   ~200-entry mirror and ~200 rewired call sites.
2. **Shrink it first.** Make the compiler depend on a narrow set of
   prelude-published builders and accessors instead of the ADT's full shape. The
   prelude already exposes `pat_con`, `ap`, `i64`, `atom_val`, `pat_wild`,
   `tokens`, `expand_block` for exactly that purpose, and `Reflection.Reflect*`
   builds values by hand instead of calling them. Then the declared interface is
   dozens of names, and the ADT can be reformatted or restructured without
   touching C# at all. Cost: a redesign of the reflection boundary, needing its own
   investigation.

**Recommendation: (2), then (1) on the reduced surface.** At 200 names a table
mirrors the prelude rather than declaring an interface, and the reflection-boundary
coupling is the actual cause of the size.

## What to do, whichever branch is taken

1. Declare the interface in one file; reference `Prelude.Path`/`Stage1Path`/
   `Binding` rather than restating the paths. `Type` is **not** in it.
2. Rewire the five consumers — `Reflection.cs`, `QuoteHoles.cs`,
   `Expander.Macros.cs`, `Enforest.Macros.cs`, `Enforest.Roles.cs` — so no bare
   prelude name is spelled outside the declaration.
3. Resolve the whole declaration eagerly in `Prelude.Load` (`Prelude.cs:44-66`)
   before the stage is returned, so a rename is a load error naming the member and
   its role. Today the check is lazy, at first reflection use
   (`Reflection.cs:100-104`).
4. Keep one xUnit test asserting the declaration resolves, so `dotnet test`
   catches it without running a program; the eager check is the runtime half.
5. State in the declaration and in `std/README.md` that the declaration is the
   interface and the prelude is checked against it, with the note above about why
   codegen was deferred — so it is not re-litigated.

## Relationship

Independent of [Restructure std into a bootstrap layer and a library layer](restructure-std-into-bootstrap-and-library.md),
and **better landed first**: the restructure then moves files with the interface
already declared and guarded, and because the unit paths stay single-sourced in
`Prelude.cs`, the restructure's `stage1` → `bootstrap` rename needs no edit here.

## Answer

Unresolved — the fork above is the decision.
