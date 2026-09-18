---
title: "Port: what prelude stage 2 left failing"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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

## 1. `type` in a block (35) — `elaborate/elab-043`, `elab-097`

> elaboration failed: a declaration syntax form in a block writes only private lets,
> opens and syntax

The stage-2 `type` macro emits `export` (see `type_exports` in `dotnet/std/stage2.fun`),
and `Expander.Roles.cs:243` `DeclOver` rejects an `export` from a declaration syntax
form used inside a block. Either a block-local `type` must not emit `export`, or
`DeclOver`'s list must admit it. **Decide which before forking** — it is a language
rule about what a declaration form may write in a block, not an implementation detail.

## 2. ~~`pub type` in a module publishes no constructors (19)~~ — CLOSED 2026-09-18

> no public member `X` / unbound variable: `Red` / `T` is not a constructor in scope

Closed by `b8bf5bb` (see the update above) — the constructors were never missing; the
`type` macro was receiving one token instead of its argument. All 19 flipped to passing:
`elaborate/elab-072`, `imports/core-175/176/180/181`, `values/core-115/116/117/120/163`,
`values/elab-012/013/020/022/025/027/029/030`.

## 3. `Export` in the name traversal (14) — `elaborate/elab-058`, `elab-059`

> not ported yet: the names a Export binding uses

`Elaborator.Enum.cs:313` has no case for `Export` in the traversal that collects the
names a binding uses. Mechanical next to 1 and 2, but its own site.

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

Item 2 is closed. 1 and 3 remain, worth 49 of the 59, and are still believed to be one
`export` family — but item 2 looked like that family too and was not, so verify the
shared cause before forking both. Question 1 (may a declaration syntax form in a block
write an `export`?) is still undecided and still blocks item 1.
