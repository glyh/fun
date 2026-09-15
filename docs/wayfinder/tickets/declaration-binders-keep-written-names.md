---
title: Declaration binders keep their written name as their resolved name
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Declaration binders keep their written name as their resolved name

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariant

**M12** / surface **S5**: resolved names are always fresh; the elaborator finds a
binder by its resolved name, never by its written spelling.

## Where the code deviates

- Block-local `RecordTypeDef` / `TypeDef` / `EffectDef` / `TraitDef` bind with
  `~resolved_name:name.name` (`lib/expand/expand.ml:699, 704, 728, 733`), and
  constructors with `~resolved_name:cname.name` (`:721`, `:1009`).
- Module-item binders use `~resolved_name:binding_name` (`expand.ml:965, 978,
  987, 1023, 1030, 1037`); module macros too (`:1078`).
- The elaborator's macro table is string-keyed (`elab_infer.ml:977`,
  `elab_check.ml:139`).
- A recursive record's self-references are matched by `id.name`
  (`elab_syntax_util.ml:20-21`).

Only `Let`, parameters and `MacroDef` get fresh names (`extend_at_fresh`). Scopes
tell two same-named types apart at expansion, but the elaborator then looks them
up by string, so a local type shadowing an outer one is right only because the
newest binding wins.

## Needs a decision

Module *members* are labels read by spelling on purpose (`M.x`). Which binders
get fresh resolved names: only block-local declarations, or module items too,
with the member label kept separately (as struct fields already are)?

## Direction

Mint `name#n` for every declaration binder, carry the written label only where
it is a member label, and key the elaborator's macro table by resolved name.
