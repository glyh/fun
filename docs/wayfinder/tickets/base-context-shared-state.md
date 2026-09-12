---
title: The base context borrows the importer's mutable expander state
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
blocked_by:
---

# The base context borrows the importer's mutable expander state

## Question

`Elab_ctx.Ctx.unit_base` hands an imported compilation unit the frozen base
context with three fields swapped in from the importer — `loader`,
`macro_table`, `expand_ctx` — and, implicitly, the meta context, since base and
importer descend from one `Ctx.empty`. That is a lot of shared mutable state
crossing a boundary whose whole point is strictness. Does any of it leak?

## Evidence

Sharing is not incidental: the unit must import and expand in turn, which needs
the loader and the expander; and meta ids index one `MetaContext`, so a unit
elaborated against a different meta context would produce `VFlex` ids that mean
nothing to the importer.

Each shared field was probed. Type-aware (`Expr(T)`) macros were used
deliberately — they are the only macros that defer to the elaborator and so the
only ones that travel through the shared `Elab_ctx` macro table.

| case | result | reading |
|---|---|---|
| two units export the same type-aware macro name, call A's | 1 | correct |
| same, import order reversed, call A's | 1 | order-independent |
| same, call B's | 2 | correct |
| one type-aware macro (control) | 1 | correct |
| unit calls a macro the *importer* defined | UnboundVariable | strictness holds |
| top-level type-aware macro used *after* an import | 1 | no damage from the mutation |
| polymorphic unit value used at two types in the importer | 3 | shared metas fine |

- **`macro_table` — not a leak, and not by luck.** One `Hashtbl` keyed by name
  shared between importer and unit is the same shape as the cross-unit macro
  collision fixed on the expander side. It does not collide, because a deferred
  macro call carries the *unit key* (`path \0 name`) from `macro_head_key` /
  `macro_member_key`, not the written name. The table is shared; the namespace
  inside it is not flat.
- **`metas` — shared on purpose, and must stay that way.** A unit's value can
  carry unsolved metas that the importer solves. Splitting it would be the bug.
- **`loader`** — the unit must import in turn. Nothing to say.
- **Strictness is unaffected.** A unit cannot see the importer's macros, matching
  the value and operator sides.
- **`expand_ctx` — the one real defect.** Elaborating an `Import` also did
  `ctx.expand_ctx <- Some expand_ctx`, so after an import the *importer's* field
  pointed at the imported unit's expander.

## Resolution

**The importer-side mutation is deleted.** It was never load-bearing: `unit_base`
copies `expand_ctx` into the unit context, and the very next line overwrites it
with the unit's own expander, so the assignment only ever changed the importer.
Dropping it leaves 842 tests green.

Two things are read out of `ctx.expand_ctx` during elaboration, not one:
`eval_and_apply` (which is `Nbe.apply` for every context, so the borrowed value
happened to be right) and `Expand_ctx.with_macro_fuel`, which reserves and
releases against the expander's own fuel counter. Fuel is balanced and each
unit's expander starts at the full limit, so nothing observable came of it — but
it means the post-import importer was charging macro depth to a foreign
expander, and the "inert" reading was one field short. Deleting the assignment
removes the whole category rather than arguing about which fields stay safe.

The remaining hazard is not changed and not worth code today: **if a macro key
ever reverts to a written name**, the shared `macro_table` becomes the flat
cross-unit namespace it looks like. The unit-key discipline is what holds it, and
no type distinguishes a unit key from a bare name. A port should give unit keys a
type rather than reproduce the convention.

## Related

The caching note on
[imported-module-elaboration-context](imported-module-elaboration-context.md):
the base scope's width is implicitly part of every cached term. Safe because the
cache lives in a per-run loader and is never persisted; a port that persisted it
would have to key on the base.
