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

**How many names must remain spelled in C#, per tier** (before `c5f2489` → after):

| tier | before | after |
| --- | --- | --- |
| struct field names | 15 (`file`, `start_byte` … `scope`) | **0** |
| constructor / leaf tags spelled at build sites | the ticket's ~65 `Con(` call sites, ~50 names | **4** — `IdentTok`, `Tok`, `RawVar`, `RawPatBind`, and only on the *readback* side, where a name is matched rather than built |
| type and module names | 43 `Nominal(`/`Member(` call sites | 31 sites: `Syntax`'s 23 nominals plus `Decls` |
| distinct quoted identifiers in `Reflection.cs` | 201 | **160** |

60 spelled names were removed and 19 added — the builders now called instead: `mk_option`,
`mk_list`, `mk_span`, `mk_id`, `mk_path`, `mk_path_choice`, the leaf-tag builders (`explicitness`,
`fixity`, `delim`, `assoc`, `hole_kind`, `atom_ty`, `macro_ann`), `i64_to_bool`, and the five
`Syntax` pattern builders (`pat_wild`, `pat_var`, `pat_atom`, `pat_prod`, `pat_or`).

**What the number says about the two routes.** The tier the ticket found *nothing catches today* is
gone: no struct field name and no built constructor tag is spelled in the compiler any more. What
survives is one table of `Syntax`'s nominal type names — the tier that already fails loudly at
load (`Reflection.cs:100-104`) — plus four readback tags. So route **(1) declare all of it** is now
a ~35-name declaration over a surface that no longer mirrors the prelude's shape, and route
**(2) shrink it first** has been paid for where it was expensive. **Neither is chosen yet**: that
choice is the next step, and it is now against this table rather than the 201-name estimate that
produced the ticket.
