---
title: Restructure std into a bootstrap layer and a library layer
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: "Closed 2026-09-27: merged as `16f9948`. `std/` is `bootstrap.fun` (the ABI), `list.fun`, `lib.fun`, `type.fun`, and `stage2.fun` as the `std` unit a program imports; `stage1.fun` is gone. Gate 0 build errors / xUnit 187/187 / `conformance: 806 cases, 0 failed`. One compiler change came with it — `export M` re-exports a unit's macros with its roles — which was checked against the record before merging and is a **port gap, not a semantic change**: [export-construct](export-construct.md) ruled it on 2026-09-16. Four of this ticket's decisions were stale because the ABI declaration landed mid-flight; each is recorded below with the reconciliation."
assignee:
blocked_by: []
---

# Restructure std into a bootstrap layer and a library layer

## Question

Reorganize `std/` along the seam the compiler actually imposes — a **bootstrap**
layer holding exactly the names C# looks up by spelling, and a **library** layer
holding everything else — and remove the damage left by transplanting the source:
the 4,788-character ADT line, the duplicated list recursions, and the untested
`pub` helpers. **No change to language semantics.**

## Why this is one ticket, and why it is a `task`

Nothing here is undecided: the five decisions below were ruled 2026-09-26. What
remains is literal work — split, rename, reformat, dedupe, test. The design of the
library's *public surface* is deliberately not in this ticket; it is
[Design the user-facing library surface of `std`](design-std-library-surface.md),
which is blocked by this one.

## Where the complaint comes from (measured 2026-09-26)

- **The prelude was never authored as source.** `std/stage1.fun` is byte-for-byte
  the deleted prototype's `stage1_source` OCaml string literal, and
  `std/stage2.fun` is its `stage2_source` (the only difference is
  `import "std/stage1"` for the port's unit loader). Verified:

  ```sh
  git show 37b41f1^:lib/semantic/typecheck/elab_prelude.ml   # stage1_source: line 11, stage2_source: line 108
  ```

  It was cut out of an OCaml `{| … |}` block, which is why line 32 of
  `std/stage1.fun` is 4,788 characters — the entire `Syntax` ADT as one
  `rec … and … and …;` chain.
- **Nothing has touched `std/` since 2026-09-18.** The stdlib did not regress; it
  became the only prelude when the prototype was deleted, and there is no longer a
  second implementation for its shape to hide behind.
- **There is no List library.** `tok_rev` and `groups_rev` are literally the same
  code at `List(TokenTree)` and `List(List(TokenTree))`; `tok_append` and
  `append_decls` likewise; `param_decls`, `ctor_alts`, `join_commas`,
  `type_members`, `type_opens`, `type_exports`, `param_decls`, `check_records`
  are hand-written recursions over the same two shapes. Ten recursive functions
  where a library would have four.
- **Three `pub` helpers are referenced nowhere** in `std/`, `src/`, or `test/`:
  `pat_var`, `id_eq`, `id_name`.

## The ABI, exactly

This is the bootstrap layer, and it is the whole of it. Everything else in `std/`
is library.

| What | Names | Where C# spells it |
| --- | --- | --- |
| `Syntax`'s nominal types | `Expr`, `Decl`, `Pattern`, `TokenTree`, `R`, `Explicitness`, `AtomVal`, `AtomTy`, `Fixity`, `MacroAnn`, `TokenKind`, `Delim`, `Assoc`, `Role`, `Order`, `RoleMeaning`, `Rule`, `RulePart`, `HoleKind`, `Replacement`, `Capture`, `Captured`, `Field`, `QuoteHole`, `Param`, `EffectRow`, `EffectOp`, `Ctor`, `Branch`, `PatField` | `Reflection.cs:25-53` |
| `Syntax`'s structural types | `Id`, `Span`, `Path`, `PathChoice`, `Decls` | `Reflection.cs:54-61` |
| Three prelude nominals | `Bool`, `Option`, `List` | `Reflection.cs:30-32` |
| Constructors read by name | `Tok`, `IdentTok`, `RawVar`, `RawPatBind` | `QuoteHoles.cs:27,56` |
| Syntax spelled when C# builds a form | `List`, `Decl`, `TokenTree`, `Type` | `Expander.Macros.cs:97,129,131`, `Enforest.Macros.cs:59,62`, `Enforest.Roles.cs:566` |
| Unit paths and the binding | `std`, `std/stage1`, `stdlib` | `Prelude.cs:18-22`, `Loader.cs:32,126` |

**Explicitly not ABI** — the compiler provides these, so they are not `std/`'s
responsibility at all: `<-`, `assignment`, `~>` (`Expander.Roles.cs:10-22`) and
`Type` (`Elaborator.cs:151`).

**On guarding the ABI — do not overstate this.** Drift is already caught
*incidentally*: `Reflection.Nominal` throws `the prelude's Syntax.R is not a
nominal type` (`Reflection.cs:100-104`), `ReflectionTests` triggers it, and
`PreludeTests.cs:23` pins the `std/stage1` path in an error message. What is
missing is not coverage but a **statement**: today the ABI exists only as a
scatter of string literals across four files, so a reader cannot tell which names
are load-bearing and the first failing test is whichever unrelated one happens to
touch reflection. Making that statement is
[Declare the bootstrap↔compiler interface once](declare-bootstrap-compiler-interface-once.md)
— whose measurement also corrected this paragraph: the drift is loud only for
*type* names, while constructor and struct-field renames are silent today.

## Decisions (user, 2026-09-26)

1. **Seam = the ABI, exactly.** Bootstrap keeps only what C# names by string:
   `Syntax` + its reflection builders, `Bool`, `Option`, `List`. Everything else
   moves to the library — `if`, `i64_to_bool`, the comparison/arithmetic operators,
   `Eq` + its impls, the fixity statements, and the `type` macro with its ~150
   lines of token helpers.
2. **Role names, split library.** See the target layout below.
3. **The refactor creates the generic functions.** `rev`, `append`, `map`, `fold`
   (and `Option`'s `map`/`bind`) land in `std/list.fun` as the library's first cut,
   and `std`'s duplicates are rewritten onto them. Accepted cost: this sets the
   library's first public API before the design ticket — but the polymorphism is
   the only thing that can remove the duplication, so the API is created here or
   not at all.
4. **All four scope items land** (reformat; ABI guard + prune; conformance cases
   for the library; centralize the `type` macro's error text), plus
   `std/README.md`.
5. **Keep `panic`, defer diagnostics.** A malformed `type` declaration is a
   genuine language error, which is what `panic`/`FunException` is for. The real
   gap is the missing source position — the map's existing fog item *"elaborator
   errors carry no source location"* owns that, and this ticket does not invent a
   second mechanism.
6. **This ticket is the frontier**; the design ticket is blocked by it, so it
   designs against real files and a real first cut instead of a plan.

## Precondition landed (integrator, 2026-09-27)

The `Relationship` note on [declare the bootstrap↔compiler interface
once](declare-bootstrap-compiler-interface-once.md) says that ticket is *better landed first*, "so
the restructure then moves files with the interface already declared and guarded". Its measurement
is now on `main` (`71ce629`): the prelude publishes the compiler's reflection builders — 19 of
them, in `std/stage1.fun` — and `Reflection.cs` calls them instead of spelling 60 constructor,
field and leaf-tag names. The reflection boundary this restructure would otherwise have reshaped
underneath itself is already reduced, and `std/stage1.fun`'s ABI surface is now one builder list
plus the `Syntax` nominals.

No declaration was made and no eager check was added — that choice is still open on its own
ticket, and it does not stand in this ticket's way.

## Landed 2026-09-27 — `16f9948`

Merged from `restructure-std-bootstrap-library` (`a78ff74` reformat + prune, `08e1a47` the split,
`031650c` `type.fun` onto the generics, `8ab591a` cases + README, `b266ccf` a merge of `main`).
Gate on the merge: `dotnet build` 0 errors, xUnit **187/187**, `conformance: 806 cases, 0 failed`
(798 → 806: the eight new pairs under `test/conformance/cases/std/`).

```text
std/bootstrap.fun   the ABI: Syntax, its builders, Bool / Option / List  (elaborated with no prelude)
std/list.fun        rev, append, map, fold
std/lib.fun         option_map, option_bind
std/type.fun        the `type` macro and its token helpers
std/stage2.fun      the `std` unit a program imports
std/README.md
```

`Prelude.cs` and `Loader.cs` now take a **list** of prelude units, lowest first
(`Order = [BootstrapPath, "std/list", "std/lib", "std/type", Path]`) rather than one name — the
mechanical consequence of the prelude no longer being one unit. `Prelude.Stage1Path` is renamed
`Prelude.BootstrapPath`.

### The one compiler change — a port gap, not a semantic change

The fork widened `ExportUnitRoles` into `ExportUnitSurface`, so `export M` re-exports a unit's
**macros** alongside its roles, and reported that the split needs it because it moved
`type`/`type_decls` into a different unit. This ticket says *"No change to language semantics"*, so
before merging it was checked against the record:

> **A unit's macros re-export like its roles** (`Expand_ctx.macro_reexports`, added to the driver's
> `macro_exports`).

That is [`export-construct`](export-construct.md)'s ruling of 2026-09-16, implemented in the
prototype's `staged-prelude` branch. **The port never carried it**, which is why the split broke on
it. So the change restores ruled behaviour instead of inventing a rule, and the ticket's "no
semantic change" holds in the sense that matters — the language's semantics already included this.
Pinned by `imports/re-exported-unit-macro` (it fails with `no public member same` without it).

### Four decisions the ABI declaration made stale — reconciled, not followed blindly

Those decisions were written before
[declare the bootstrap↔compiler interface once](declare-bootstrap-compiler-interface-once.md)
landed mid-flight:

- **`pat_var` is kept.** Decision 4 pruned it as referenced nowhere — true then, false now:
  `PreludeAbi` declares it and `Reflection` calls it, because that fork published it as a builder.
- **`i64_to_bool` stays in bootstrap.** Decision 1 moved it to the library; it is an ABI builder, so
  the library cannot own it.
- **Option's functions are `option_map` / `option_bind`** — there is no module on `Option`.
- **No dynamic token spelling** in the centralized `type` message: the language has no string
  concatenation, so the message is centralized but not concatenated.

**`pat_con` stays** as the ticket left it (unreferenced, and not in the declaration).

### Not done

- No new spans; `pat_con` was not deleted; the messages carry no source position, so the map's
  *"elaborator errors carry no source location"* fog item is untouched.
- A stale doc comment survives: `PreludeAbi.cs:22` still names `Prelude.Stage1Path`, which this
  rename replaced with `BootstrapPath`. One word, filed with
  [the spellings the declaration leaves](prelude-abi-remaining-spellings.md).

## Target layout

```text
std/bootstrap.fun   Bool, Option, List, Syntax — the ABI, and the whole of it.
                    Elaborated with no elaborator (Prelude.Load(file, std: null)).
std/list.fun        generic functions: rev, append, map, fold; Option's map, bind.
std/lib.fun         if, i64_to_bool, operators + their fixity, Eq + impls.
std/type.fun        the `type` macro and its token helpers (today's type_decls, …).
std/stage2.fun      -> the `std` unit: import the three, export + open them.
                       Shim pattern already in the file today:
                       Core = import "std/stage1"; export Core; open Core;
std/README.md       the seam rule, the ABI table, and why the ADT is one chain.
```

`std/stage1.fun` is renamed to `std/bootstrap.fun`. The two-stage **mechanism**
stays exactly as it is — bootstrap is still elaborated with no elaborator, library
units still against it; only the prototype-era names change, per CLAUDE.md's rule
to name after the domain model rather than the deleted prototype's abbreviations.

## Compiler-side work (this is not just new files)

`Prelude.cs` is a hard-coded two-stage special case and must grow to N units:

- `Lazy<Stage> Stage1` / `Stage2` (`Prelude.cs:28-29`) become an ordered set of
  stages: bootstrap first, elaborated with `std: null`, then each library unit
  elaborated with the stages below it as its prelude.
- `Of(path)` (`Prelude.cs:32`) currently maps only `std/stage1`, everything else
  to stage 2; it must cover each unit path.
- `Load` passes `new Dictionary<string, string>()` (`Prelude.cs:58`) and
  `Loader` serves the prelude by special case (`Loader.cs:32,126`), so library
  units need to be handed to the loader explicitly — either as sources it expands
  and elaborates, or as pre-elaborated stages like the two today. Prefer the
  latter: the current design elaborates the prelude once per process precisely so
  a macro it exports and the values it exports speak of the same metas
  (`Loader.cs:65`, `Prelude.cs:44-52`).
- `Prelude.Stage1Path` and the doc comments referencing it (`Loader.cs:15,23`,
  `Elaborator.cs:129-132`) follow the rename; `PreludeTests.cs:23`'s expected
  message follows with it.
- **No build change**: `Fun.Compiler.csproj:10` embeds `..\..\std\*.fun` by glob.

## What to do

1. **Split the sources along the seam** into the target layout; land the `std`
   shim unit; teach `Prelude.cs`/`Loader` about N units; rename `stage1` →
   `bootstrap` everywhere it is spelled (`Prelude.cs:22`, `Loader.cs` docs,
   `Elaborator.cs:129`, `PreludeTests.cs:23`).
2. **Reformat the ADT** one constructor per line, and wrap the rest of the file's
   long lines — the survivors after steps 3–4 are `Span` and `AtomVal`
   (`stage1.fun:18,24`), `pat_con` (`:65`), and `lib`/`type.fun`'s `tok_is`,
   `tok_scope`, `type_name`, `param_decls`, `type_member`, `type_exports`,
   `record_rhs`, `check_records` and the `type_decls` body. Keep the single
   `rec … and …` chain: `Expr` holds `List(Branch)`, `Branch` holds `Pattern`,
   `Pattern` holds `Expr`, so the mutual recursion is load-bearing and splitting
   it into separate `rec` declarations would break `Reflection`.
3. **Add the generic functions** to `std/list.fun` — `rev`, `append`, `map`,
   `fold`, plus `Option`'s `map`/`bind` — then rewrite `std`'s duplicates onto
   them and delete `tok_rev`, `groups_rev`, `tok_append`, `append_decls`.
4. **Delete `pat_var`, `id_eq`, `id_name`.** Note the ordering dependency:
   `i64_to_bool` can only leave the bootstrap once `Syntax.id_eq` is gone, since
   `id_eq` is its only bootstrap consumer. `i64_to_bool` has 2 non-`std` uses, so
   it lands in `std/lib.fun`, not the bootstrap.
5. **The executable ABI is a sibling ticket, not this one.**
   [Declare the bootstrap↔compiler interface once](declare-bootstrap-compiler-interface-once.md)
   turns the table above into one declaration resolved at load, and should land
   **before** this ticket, so the split happens with the interface already
   guarded. This ticket keeps the table as prose and keeps every ABI *name* fixed.
6. **Add `test/conformance/cases/std/`** for the library surface. The suite opens
   the prelude, walks the directory with nothing to register, and `.expect` is one
   of `42` / constructor / `ok` / `error` (`test/conformance/cases/README.md`).
   Note the suite cannot see `std` internals today — every existing case exercises
   `std` only indirectly.
7. **Centralize the `type` macro's error text** — one helper per message, and
   include the offending token's spelling. Keep `panic`; add no spans.
8. **Write `std/README.md`**: the seam rule, the ABI table, why the ADT is one
   `rec … and …` chain, and the fact that the source began as an OCaml string
   literal (so a reader does not mistake it for a deliberate style).

## Acceptance

- `dotnet build` succeeds; `dotnet test test/Fun.Tests` and the conformance suite
  are green, with the conformance count up by the new `std` cases.
- `std/stage1.fun` no longer exists; **no name in the ABI table changed**.
- The ADT is one constructor per line, and no other line in `std/` exceeds ~100
  characters.
- `tok_rev`, `groups_rev`, `tok_append`, `append_decls`, `pat_var`, `id_eq`,
  `id_name` are gone; the library defines `rev`/`append`/`map`/`fold` once each.

## Reading

- `src/Fun.Compiler/Prelude.cs` — the two-stage special case (`:22`, `:28-29`,
  `:32`, `:58`); `Loader.cs:32,126`; `Elaborator.cs:129-132`
- `src/Fun.Compiler/Reflection.cs:25-61` (the nominals) and `:100-104` (the throw)
- `src/Fun.Compiler/QuoteHoles.cs:27,56`; `src/Fun.Expand/Expander.Macros.cs:97,129,131`;
  `src/Fun.Expand/Enforest.Macros.cs:59,62`; `src/Fun.Expand/Enforest.Roles.cs:566`
- `src/Fun.Expand/Expander.Roles.cs:10-22` and `src/Fun.Compiler/Elaborator.cs:151`
  — what the compiler provides, so what is *not* ABI
- `test/conformance/cases/README.md`, `test/Fun.Tests/PreludeTests.cs:23`
- `git show 37b41f1^:lib/semantic/typecheck/elab_prelude.ml` — the transplant origin
