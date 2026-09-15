---
title: A resolved name can be forged, and context-less ids fall back to spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A resolved name can be forged, and context-less ids fall back to spelling

Found by the domain-model audit (2026-09-15), re-verified on `main` `fa2f32d`.

## Invariants

- **M11**: an `Id` built from a name alone is unbound.
- **M12**: no name is found by its spelling alone.

## Where the code deviates

- `Expand.resolve_occurrence` (`lib/expand/expand.ml:500`) treats any name
  satisfying `Expand_ctx.is_resolved_name` (`expand_ctx.ml:199`: contains `#`) as
  already resolved. Source cannot spell `#` (it begins a comment), but a macro
  can: `Syntax.new_id("x#5")` (`elab_prelude.ml:225`, empty scopes) reaches
  whichever binder was minted `x#5`.
- `macro_head_key` (`expand.ml:~417-421`): an empty-scope id falls back to the
  macro table by its spelling. The closed `macros-have-no-quoted-syntax` ticket
  says such an id reaches only the base context.
- Operator macros fall back to their spelling (`expand.ml:813`).

## Example

```fun
x = 5;
macro steal(_) { Syntax.RawVar(None, Syntax.new_id("x#5")) };  // x's minted name
steal(0)   // reaches x today; should be unbound
```

## Needs a decision

Should `new_id` exist at all now that `quote(…)` and `Id` parameters carry
scopes? If it stays, is its result unbound everywhere, or does it reach only the
base context?

## Direction

Mark "already resolved" structurally rather than by spelling, so no string a
macro builds can equal a minted name. Delete the empty-scope macro-table and
operator spelling fallbacks.

## Grilled (2026-09-15): delete string-built ids

`Syntax.new_id` is deleted, and so is every builder that makes an id from a
string (`Syntax.var("x")`, `Syntax.lam("x", …)`, …). A macro gets a name only
from `quote(…)` / `quote { … }` (the macro's site, hygienic: a name it binds is
fresh) or from an `Id` parameter or hole (the user's site). With no
string-built ids there is nothing for the `#` check or the empty-scope spelling
fallbacks to serve: "already resolved" becomes structural and the fallbacks go.
Migrate the tests and prelude builders that use strings.
