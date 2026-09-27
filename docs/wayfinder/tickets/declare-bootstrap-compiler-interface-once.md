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

**Ruled by the user, 2026-09-27 — measure before shaping.** The two routes above are not
decided yet, and the reason is that both were estimated rather than measured (~200 names, ~200
call sites). So the next step is the *investigation* the recommendation (2) already needs, and
nothing else: rewrite the reflection boundary so the compiler reaches the prelude through
published builders and accessors instead of hand-building `VCon`/`VRecord` by name and field,
and report **how many of the ~201 names actually survive** — per tier (constructor / type and
module / struct field), with the rewritten call sites on disk, compiled, and the suite green.

Explicitly out of scope until that number exists: no `PreludeAbi.cs`, no eager verification in
`Prelude.Load`, no rewiring of the five consumers to a declaration. Route (1) — declare all of
it — and route (2) — declare the reduced surface — are both still live, and the choice between
them is taken against the measured count, not against the estimate that produced this ticket.

Consequence to keep in mind while measuring: the number that matters is not "how many names can
be moved into a helper" but how many must remain **spelled in C#** at all. A name the compiler
still has to write down is a name the declaration must carry, whichever route follows.

## The measurement (integrator, 2026-09-27) — the deliverable the ruling asked for

The fork this ticket asked for ran, and its branch (`bootstrap-interface-measure`) is integrated
as `71ce629`. It is exactly the step the ruling scoped: the prelude publishes its reflection
builders, and `Reflection.cs` reaches the prelude through them instead of building `VCon`/
`VRecord` by name and field. Nothing else was touched — no `PreludeAbi.cs`, no eager verification
in `Prelude.Load`, no rewiring of the five consumers — and the four other files are unchanged.

Gate: `dotnet build` 0 errors, xUnit `186/186`, `conformance: 790 cases, 0 failed`.

**How many names must remain spelled in C#, per tier** — **corrected 2026-09-27**, see the note
below the table:

| tier | before `c5f2489` | after `71ce629` |
| --- | --- | --- |
| struct field names | 15 (`file`, `start_byte` … `scope`) | **0** |
| leaf enums, `Option` / `List` / `Bool` tags | spelled at every site | **probed** — `_bool = LeafsOf(Builder("i64_to_bool"), 2)`, `_some = CtorOf(Builder("mk_option"), Probe, I64(0), Probe)` |
| constructor tags on the **build** side — `Con(nominal, "X", …)` | 47 sites, 45 names | 47 sites, 45 names — **untouched** |
| constructor tags on the **readback** side — `case ("X", …)` | not counted before | **113 distinct names over 198 sites — untouched** |
| type and module names — `Nominal(...)` / `Member(...)` | 43 call sites | 31 call sites, 24 distinct (`Syntax`'s 23 nominals plus `Decls`) |
| distinct quoted identifiers in `Reflection.cs` | 201 | **160** |

19 builders were published and called in place of the field, record and leaf-tag tiers:
`mk_option`, `mk_list`, `mk_span`, `mk_id`, `mk_path`, `mk_path_choice`, `explicitness`, `fixity`,
`delim`, `assoc`, `hole_kind`, `atom_ty`, `macro_ann`, `i64_to_bool`, `pat_wild`, `pat_var`,
`pat_atom`, `pat_prod`, `pat_or`.

### Correction — the first version of this table was wrong, and this is why

The first count came from grepping `Con(` / `Nominal(` / `Member(` / `CtorOf(` call sites, which
sees only the **build** side. It missed `case ("RawVar", 1)`, `("IdentTok", [var s])`, and the
rest of the readback switches entirely — **113 tags over 198 sites**, the single largest spelled
tier in the file. The first version reported *"constructor tags → 4"*, which was true only of the
four names my grep was written to look for. The corrected figure is above; the number 201 → 160
was never in doubt.

So what `71ce629` achieved is **the mechanism and the small tiers**, not the big ones. It built
the probe (`Ctor` with a `Tag`, `Leafs` with `CodeOf`, `Layout`) and used it on the record, leaf
and `Option`/`List`/`Bool` tiers — 11 tag comparisons in the file now go through a probed tag
(`name == _nil.Tag`, `_bool.CodeOf(...)`). The ADT tiers — `Expr`'s ~40 `Raw*`, `Decl`'s 16
`Decl*`, `Pattern`'s 10 `RawPat*`, `TokenKind`'s 8, the ~30 `Mk*` and role/rule tags — are
untouched on both sides.

**What that does to the route choice.** The declaration is **~160 names, not ~35** (24 type names
+ 45 build tags + 113 readback tags), which is the "a table mirrors the prelude rather than
declaring an interface" scale the ticket was worried about at 200. The choice was put to the user
again on 2026-09-27 against the corrected count; **it is the open item on this ticket**.

What is *not* true any more, measured: no struct **field** name is spelled (the tier the ticket
found nothing catches today), the record and leaf-enum tags are probed rather than spelled, and
the remaining type names are the tier that already fails loudly at load (`Reflection.cs:100-104`).

## Ruling (user, 2026-09-27): route (1) — declare all of it, at the corrected count

Put to the user again after the first version of the table above was found wrong, and looked at
side by side as code. **Decided: `PreludeAbi.cs` holds the whole interface, ~160 names, resolved
eagerly in `Prelude.Load`.** Route (2) — shrink the surface first, moving the tag spellings into
`.fun` — is **not** taken. The tags stay spelled in C#, but only inside the declaration, and every
consumer references the declaration.

What this means for whoever implements it:

1. **`PreludeAbi.cs`** holds three groups of **`const string`** — ~19 **builders** (`mk_span`,
   `mk_option`, `pat_wild`, …), 24 **type and module names** (`Syntax`'s 23 nominals plus
   `Decls`), and the 158 **tags** (45 built, 113 read back). `const` and not `static readonly`:
   198 readback sites are `case` labels, and only a compile-time constant is legal there.
2. **Where it lives is a real constraint.** `Fun.Expand` cannot reference `Fun.Compiler`, and
   three of the five consumers (`Expander.Macros.cs`, `Enforest.Macros.cs`, `Enforest.Roles.cs`)
   are inside `Fun.Expand`. So the **data** belongs in `Fun.Kernel` or `Fun.Expand` — never
   `Fun.Compiler` — while its **`Verify`** (which needs the loaded stage) belongs with `Prelude`.
   Decide and justify the placement in the report.
3. **Reference, do not restate, the unit paths**: `Prelude.Path`, `Stage1Path`, `Binding` are
   already single-sourced. `Type` is **not** in the interface — the compiler spells it and it
   belongs to the elaborator (`Elaborator.cs:151`), not to `std`.
4. **Resolve the whole declaration eagerly in `Prelude.Load`**, before the stage is returned, so a
   rename is a load error naming the member and its file. Today the check is lazy, at first
   reflection use (`Reflection.cs:100-104`).
5. **Rewire the consumers** — `Reflection.cs` (267 sites: 198 `case` labels, 45 `Con(` arguments,
   24 `Nominal`/`Member` paths) plus the four others in the ABI table: `QuoteHoles.cs`,
   `Expander.Macros.cs`, `Enforest.Macros.cs`, `Enforest.Roles.cs`. When this lands, **no bare
   prelude name is spelled outside the declaration**, and a sweep is the check: search for prelude
   names outside `PreludeAbi.cs`, and justify every remaining hit in the report.
6. **One xUnit test asserting the declaration resolves**, so `dotnet test` catches it without
   running a program; the eager check is the runtime half.
7. State in `PreludeAbi.cs` and in `std/README.md` that the declaration *is* the interface and the
   prelude is checked against it, with the note about why codegen was deferred, so it is not
   re-litigated.

**Cost accepted, stated plainly:** the declaration mirrors the prelude's shape. That is the
reading chosen over moving the spellings into `.fun`, and the reason is in this ticket's own
history — `.fun` cannot be the sole source, because the interface is a fact about the compiler's
*usage*: which names matter, in which role, and that `Type` is the elaborator's.

**For whoever measures next:** the first version of the table above was wrong by a factor of forty
on the tag tier. Count the build side, the readback side and the type names separately — a `Con(`
grep sees one of the three.
