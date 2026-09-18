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

## 2. `pub type` in a module publishes no constructors (19) — `imports/core-180`, `elab-072`

> no public member `X` / unbound variable: `Red` / `T` is not a constructor in scope

`pub type Color = Red | Green` inside a module binds the nominal but its constructors
do not reach an importer. The macro's `export`/`open` of the nominal does not carry
them. Related to 1 and 3: all three are `export` under-supported.

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
- `macros/core-308` — wrong value: expected 1010, got 110. **The only silently wrong
  answer in the suite**; everything else above is an error. Look at this one first
  even though it is one case.

## Note

1, 2 and 3 are one family (`export`, as the `type` macro uses it) worth 68 of the 78.
A fork taking them should take all three, but only after question 1 above is decided.
