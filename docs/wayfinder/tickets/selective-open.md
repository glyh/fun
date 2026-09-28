---
title: A selective open, `open M.{a, b}`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# A selective open, `open M.{a, b}`

Opened 2026-09-27 by [design the library surface](design-std-library-surface.md).
This is the **mirror of a construct that already works**: `export M.{a, b}`
re-exports members, an enum's constructors, **named impls**, and a unit's roles and
macros ([export-construct](export-construct.md), `docs/STATUS.md:209`). The open
direction has no such form, and that asymmetry is the measured gap in
[impl visibility](../topics/impl-visibility.md): *"There is no selective open —
`open M` brings every public name M exports, and collisions shadow silently."*

## Why it matters

An impl reaches a use site only through a wholesale `open` (option A, reaffirmed by
the library-surface ticket). There is no selective open, so bringing one impl into
scope means accepting that module's entire export surface — and a module that
defines a type *and* its impl forces every user to open it wholesale. That is the
tax [impl visibility](../topics/impl-visibility.md) measures, and it is why the
deriving ticket says to settle impl reach *before* `derive` generates impls at
scale: generated impls live in the defining module, so deriving anything makes that
module mandatory-open.

```
M = module { pub rec Color = enum { R, G };
             pub impl color_eq : Eq(Color) = module { fn eq(x, y) { True } };
             pub helper = … };          # whole surface arrives with the open

open M;                                 # today: Color, color_eq, helper, …
open M.{color_eq};                      # proposed: the impl, and nothing else
```

Measured, so the shape is known to be the only way to separate them: `export`
flattens a module's members while `open` only scopes them, `open` does not propagate
to importers while `export` does, and a selective `export M.{Color, color_eq}`
carries the impl to whoever opens the result while leaving `other` unbound.

## What it would touch

- **Grammar.** `open` already takes a module path; `export` already parses a
  selection list after `M.`, so the production exists and the work is to accept it
  in `open` position. Note the narrow clearance: `.{` also spells export selection
  and (separately) `P{x = 1}` is record construction, so a *bare* `M.{a}` in
  expression position is not free — the form is only free where `open` already
  expects an operand.
- **Enforester and elaborator.** An open's width is its type's public members
  today; a selective open's width is the name list. Whatever the open carries —
  values, an enum's constructors, a named impl, a role, a macro — the selection
  must carry too, or the form is useless for exactly the case that motivated it.
- **Reflection and every scope-addition site.** If the open node gains a selection
  field, `CLAUDE.md`'s rule applies: update every construction site, or drop the
  field with a reason written down.
- **`OpenSuppliesRole`.** An open may not supply a member named like a role visible
  where it is written; that check currently reasons about a whole module's exports
  and needs an answer for a name list.

## Open sub-questions

1. **Does the list carry everything `export` does**, or only impls? The mirror
   argument says everything; the motivation is impls. A list that carries only
   impls needs a second spelling later for values.
2. **Does naming an impl also bring its trait's name into scope?** Calling
   `Eq.eq` needs the trait resolvable, and "the trait must be in scope to use its
   methods" is the axis Rust separates from where the impl is found.
3. **Duplicates and idempotence.** `open M.{a}` twice, `open M.{a}; open M`, and
   two opens supplying one name. [impl visibility](../topics/impl-visibility.md)
   already decided open *should* be idempotent and deduped by impl identity; this
   form must inherit whatever lands.
4. **Unknown name.** `export M.{nope}` is an error ("export of unknown member").
   Presumably so here, with the same shape.
5. **Is it statement-only**, as `open` is today (a block statement), or does it also
   get the local form `M.(e)`? **Not as the same ticket** — see below.

## Explicitly not this ticket

`M.(e)` — a local open expression, raised 2026-09-17 and **ruled B, not adopted,
2026-09-18** ([local-open-expression](local-open-expression.md)): a block already
opens a module for one expression, so it was a second spelling for one construct,
which this language trades away for consistency. It also does not close this ticket's
gap: opening `M` for an expression still brings M's whole surface, just briefly.
That ticket closed with a condition — reopen with call sites as evidence if the
double-brace idiom becomes a real irritant — and its grammar (`e.(…)`) stays free
until then.

## Not blocking

Nothing. The library surface does not need it: `std` controls its own modules and
re-exports the impls itself (`export Lists.{list_eq}`), so users write no `open`.
This ticket is for **user** modules and for `derive`, whose generated impls cannot
help but live in the defining module.

## Reading

- [Impl visibility — usability implications](../topics/impl-visibility.md) — the
  measurement this closes, and the A-versus-B analysis that kept resolution scoped
- [trait library deriving and protocols](design-trait-library-deriving-and-protocols.md)
  — owns the `derive` side, and named impls, the evidence-position twin of this form
- [`export` — re-export a module's or enum's members](export-construct.md) — the
  mirror, implemented, with the selection-list grammar to reuse
- [design the user-facing library surface](design-std-library-surface.md) — the
  ticket that opened this one, and why impl reach was left alone

## Decisions 2026-09-28 (grilling)

1. **The list carries everything `export` does** — values, an enum's constructors, a named impl, a
   role, a macro. One parse shared with export, no asymmetry inside one syntax, and nothing needs a
   second spelling later. The cost is accepted: the selection must carry every member kind, not
   just impls, which is the bulk of this ticket's work.

2. **A selective open supplies only what it names** (user, 2026-09-28: "any unnamed item shouldn't be
   brought into scope"). Listing an impl does **not** supply its trait — `open M.{i64_size}` leaves
   `Size` unbound — so the call site either names it too (`open M.{Size, i64_size}`) or qualifies it
   (`M.Size.size(5)`), which works either way.
3. **Duplicates and idempotence are inherited, not new.** `open` is already idempotent and deduped by
   impl identity (the named-impl work, landed 2026-09-27), and a selective open is an open with a
   width: opening the same list twice changes nothing, and `open M.{a}; open M` leaves `a` once.
4. **An unknown name is an error, in the export form's shape.** `export M.{nope}` already reports
   "export of unknown member"; decision 1 makes the list the same list, so the open side answers the
   same way rather than inventing a second behaviour.
5. **Statement-only, as `open` is today.** The local form `M.(e)` is its own ticket and is explicitly
   not reopened here.

With 1–5 recorded, this ticket is forkable: nothing is left undecided.
