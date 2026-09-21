---
title: "Port: what prelude stage 2 left failing"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: All six causes closed. 13 failures → 3, and the 3 are E11's (values/elab-062, values/core-067, values/elab-067), moved to port-nominal-identity. core-165 was fixed by rewriting the case, not by a ruling.
assignee:
blocked_by:
---

# Port: what prelude stage 2 left failing

Triage of the 78 conformance failures remaining after
[port-prelude-stage2](port-prelude-stage2.md) landed (2026-09-18, 611/689).
Item 3 of the port ticket's handover.

**Update 2026-09-18 (`b8bf5bb`, `port/token-hole-capture`): 78 → 59.** Fixing item 6's
`macros/core-308` closed item 2 entirely as well — same root cause. `Expander.Roles.cs`
`Fill(...)` built a `SyntaxMapper` with no `Capture` hook, so a hole written as a macro
call's *whole argument* (`count($r)`) was never rewritten to its captured tokens and the
macro received the bare `$r` token. The stage-2 `type` macro is a `: Decl` form feeding a
`List(TokenTree)` hole to a macro, so every `pub type` case failed the same way. Ported
from the prototype's `capture` field (`lib/expand/expand.ml:304`).

Six causes, largest first. Each section is independently forkable; split one into its
own file when it is picked up. Counts are C# conformance cases.

## 1. ~~`type` in a block (35)~~ — CLOSED 2026-09-18

> elaboration failed: a declaration syntax form in a block writes only private lets,
> opens and syntax

The stage-2 `type` macro emits `export` (see `type_exports` in `dotnet/std/stage2.fun`),
and `Expander.Roles.cs:243` `DeclOver` rejects it when the form is used inside a block.

### Grilled (2026-09-18): the site adapts, and a written `export` is an error

**Ruling.** A declaration macro emits a canonical decl list; the **site** decides what
each item means there. This is the rule that already governs `pub`: `expand.ml:1045`
applies `Syntax.publish` to every binding a macro emitted, so `pub type Color = …`
publishes the `rec`, the `export` and nothing else, with the macro knowing nothing
about `pub`. The block site is the other half of the same rule.

1. A **generated** `export` at a block site is **dropped** — port the prototype's
   `ExportBinding { public = false; _ } -> body` (`expand.ml:569`).
2. A **source-written** `export` in a block is an **error**. `{ export Foo; 1 }` is
   nobody's intent.

   *Correction (2026-09-18, found while implementing):* this is **not** a divergence.
   The prototype already errors here — a written `export` never reaches `decl_over`
   from source, it dies earlier in the block statement parser — so both
   implementations agree and nothing is listed in `prototype-divergences.txt`. No
   provenance plumbing was needed either: the generated/written split is already
   `Public: false` vs `true`. The shared case is
   `test/conformance/cases/elaborate/written-export-in-block.fun` (expect `error`).

**Rejected for now: an ambient `$site` hole** letting the macro adapt itself. It
competes with `publish` rather than joining it, and it has exactly one client
(`type_decls` is the only `: Decl` form in the prelude). Split off to
[macro-owns-its-output](macro-owns-its-output.md), where it is tied to the removal of
`publish` — they are one design, and the user's position is that M1 should not land
without it.

### Also a port gap, not a question

C#'s `DeclOver` has five cases; the prototype's has nine plus the export line. Missing:
`EffectBinding`, `TraitBinding`, `ImplBinding`. Port them with the above.

## 2. ~~`pub type` in a module publishes no constructors (19)~~ — CLOSED 2026-09-18

> no public member `X` / unbound variable: `Red` / `T` is not a constructor in scope

Closed by `b8bf5bb` (see the update above) — the constructors were never missing; the
`type` macro was receiving one token instead of its argument. All 19 flipped to passing:
`elaborate/elab-072`, `imports/core-175/176/180/181`, `values/core-115/116/117/120/163`,
`values/elab-012/013/020/022/025/027/029/030`.

## 3. ~~`Export` in the name traversal (14)~~ — CLOSED 2026-09-18

> not ported yet: the names a Export binding uses

`Elaborator.Enum.cs:313` had no case for `Export` in the traversal collecting the names
a binding uses. Closed by `ac86a56`: 11 of the 14 pass; 3 reached further gaps and are
now their own failures (`elaborate/elab-059` renaming an `FMatch` frame,
`values/elab-062` wrong value, `values/elab-067` constructor pattern head on a
non-nominal).

**Items 1 and 3 did not share a cause** — verified, not assumed. All 35 of item 1 came
from the generated-`export` drop alone; porting `decl_over`'s missing `EffectBinding`,
`TraitBinding` and `ImplBinding` cases moved zero cases, and was kept only because the
prototype has them.

## 4. Method calls on a record (4) — `values/elab-128`, `elab-129`

Already an honest "not ported yet" (`Elaborator.Structs.cs:290`). Independent of the
`export` family.

## 5. Trait impls (2) — `values/core-152`, `macros/core-312`

> missing implementation of `Eq`

`core-152` is `impl Eq(Self)` in a struct; `core-312` is a re-exported named impl
reached after `open`. Two different causes sharing a message; check before assuming
one fix.

## 6. Singletons (4)

- `values/core-102` — the checker evaluated a term that performs `Ping.hit`
- `values/core-067` — a generative type escapes its binder (relates to the known
  generative-module-stamp stopgap)
- `macros/core-270` — a generated syntax form, then "function call must be adjacent to
  the callee; whitespace application is not supported"
- ~~`macros/core-308` — wrong value: expected 1010, got 110.~~ **Fixed 2026-09-18**
  (`b8bf5bb`), and it took item 2's 19 cases with it. Worth recording why: it was the
  only *wrong answer* in a suite of errors, and it was the cheapest 20 cases on the
  board. Silently wrong beats loudly missing as a triage signal.

## Note

**Items 1, 2 and 3 are all closed (2026-09-18). 690 cases, 13 failing.** The `export`
family was three separate causes wearing one error message, which is the lesson worth
keeping: none of the three shared a fix.

Remaining, all singletons or near-singletons:

| cases | failure |
|---|---|
| 4 | method calls on a record (`values/elab-128/129/130`, `method-row-outer-ref`) |
| 2 | missing implementation of `Eq` (`values/core-152` `impl Eq(Self)`; `macros/core-312` re-exported named impl) |
| 1 | `elaborate/elab-059` — renaming an `FMatch` frame |
| 1 | `values/elab-062` — expected 10, got 0 (**the only wrong answer left**) |
| 1 | `values/elab-067` — constructor pattern head on a non-nominal |
| 1 | `values/core-067` — a generative type escapes its binder |
| 1 | `values/core-102` — the checker evaluated a term that performs `Ping.hit` |
| 1 | `macros/core-270` — generated syntax form, then whitespace application |
| 1 | `imports/core-165` — unbound variable `Green` |

Take `values/elab-062` first, as `macros/core-308` was taken first: a wrong answer is a
better lead than a missing feature.

## Resolution (2026-09-20) — 13 failures → 3

Merged from branch `pi-agent-8accc24c-f625-44f` (`fbea929`), commits `50c8b03`
(method calls), `568a1ea` (`Eq` impls), `b720867` (`FMatch` rename), `ddb83e1` (two
singletons). **C# 13 → 3 failed**; xUnit 168/168 before and after; `dune test` and
`dune test test/conformance` green throughout.

| item | verdict |
|---|---|
| 4. Method calls on a record (4 cases) | **fixed** — `InferMember` grew a method branch (unify the value's type with the method's `Self`, apply the codomain, emit the row; a nullary `method m()` is a function of `()`), evaluation got the matching `Dot` case, and the work lives in a new `Elaborator.Methods.cs` (convention 7) |
| 5. `Eq` impls (2 cases) | **fixed, two independent causes** — `core-152`: evidence matching compared by read-back equality, which cannot see a partial `Self` struct against the complete type, replaced by a conversion test that solves no meta; `core-312`: `export M` dropped named public impls, so export now carries `BindingTerm.Impl`/`ModuleEntry.Impl` and the clash check counts impl names |
| 6a. `elab-059` (1) | **fixed** — rename a stuck `FMatch` (open each arm at its binders, rename under them) plus `FDot`/`FRefGet`/`FRefSet`; it now errors for a genuine reason (`cannot unify VAtom with VAtom`) |
| 6b. `core-102` (1) | **fixed** — `InferApWithPendingDicts` evaluated its argument eagerly; it now uses `ArgumentValue` and emits argument effects and the arrow row |
| 6c. `core-270` (1) | **fixed** — `ReadContext` minted a fresh scope *per token* rather than once per statement group, so a generated role's scope never reached its use |
| `imports/core-165` | **fixed by rewriting the case, not by a ruling** — the fork correctly stopped: its arms passed in the prototype only via the rejected by-name bare-constructor resolution, and the case was not listed as a divergence. The arms now qualify through the unit that declares the constructors, which passes in both runners; the rejected shape gets its own divergence case. See [bare-constructor-pattern-resolves-by-name](bare-constructor-pattern-resolves-by-name.md) "Correction (2026-09-20)" and commit `4fdab26` |

**The 3 that remain are all E11**: `values/elab-062`, `values/core-067` and
`values/elab-067`, and they are [port-nominal-identity](port-nominal-identity.md)'s.
`elab-067` turned out *not* to be the local one-case fix this ticket's table implied:
it is the `SymbolTable` program (`f(st1.Symbol) * 10 + f(st2.Symbol)` = `10`), and the
fork diagnosed the two reasons it cannot pass yet — `TypeHead` returns null on a stuck
`s1.Symbol`, and `ElaborateNominalHeadPattern` refuses generative nominals. Both are
in that ticket's paths, so the fork did not touch them.

### Not done (each a ticket line)

- **The grilled generic-impls ruling is implemented nowhere.** `trait-op-takes-innermost-impl`'s
  "Grilled (2026-09-18)" decided that a free name in an impl head binds; neither
  `Eq` case needed it, and no shared case exercises it. It stays open there.
- **`Nbe.Convertible` is still a stopgap.** The fix above added a localized no-solve
  conversion in `Elaborator.Traits.cs` rather than porting the prototype's full
  `Nbe.conv`. Careful as that is, the port now has two conversion notions.
- **A top-level `pub impl` still reports "not ported yet: the `pub` form"** (found by a
  diagnostic; no shared case reaches it). It belongs with
  [the parity conversions](port-parity-conversions.md).
