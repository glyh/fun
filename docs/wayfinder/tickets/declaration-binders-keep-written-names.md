---
title: Declaration binders keep their written name as their resolved name
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-15
resolution: Implemented. Every declaration binder (types, constructors, effects, traits, pattern synonyms, module items, macros) is bound by one helper, Expand.bind_declaration, with a fresh resolved name; what a declaration exports and a member is reached by is its label (Syntax.label). The elaborator keys its context by resolved name and gives Core binds, module fields, nominals and constructors the label.
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

## Grilled (2026-09-15): every declaration binder is fresh

Every binder gets a minted resolved name: types, constructors, effects, traits,
module items and macros, as `let` and parameters already do. A module item also
keeps its written label, used only for member access (`M.x`), as struct fields
do. The elaborator never finds a binder by its written spelling; its macro table
is keyed by resolved name.

```fun
type Tmp = Yes | No;
macro with_tmp(e) : Expr { quote({ type Tmp = A | B; $e }) };
with_tmp(Tmp.Yes)   // the user's Tmp, not the macro's
```

## Implemented (2026-09-15)

- `Expand.bind_declaration` binds every declaration binder at a fresh scope with
  a minted `name#n`, replacing the `~resolved_name:name.name` sites.
- `Syntax.label` reads a binder's label off its minted name; `Syntax.path_last`
  returns a label. It is only read off a binder a declaration owns, never used to
  find one.
- The elaborator keys `Ctx.define`/`Entry` by resolved name and uses the label
  for `LetBind`/`TypeBind`/`ModuleField`/`StructField`, nominal and constructor
  names, and a signature's fields (`Elab_type_expr`). The recursive-record
  rewrite compares resolved names. Module macro exports carry the label.
- A pattern synonym's right-hand side is now expanded (its heads resolve by
  scope set) and its name is bound like any declaration.
- Test: `declaration binders are fresh` (a macro's `type Tmp` does not take the
  caller's `Tmp`).
- Not covered: syntax-form roles (`SyntaxBinding`) keep their own resolution by
  scope set and are not renamed.
