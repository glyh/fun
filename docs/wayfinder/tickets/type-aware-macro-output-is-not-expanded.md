---
title: Type-aware macro output is not expanded
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Both call sites now share one helper, `run_type_aware_macro` (elab_resolve.ml). It expands the output through the macro runtime's new `expand` capability before lowering, and a non-syntax result raises `MacroDidNotReturnSyntax` naming the macro instead of minting a meta. The probe is a regression test.
closed_date: 2026-09-14
blocked_by:
---

# Type-aware macro output is not expanded

## Question

A type-aware macro (`: Expr(A)`) runs inside the elaborator. The `Syntax.t` it
returns is lowered and elaborated directly (`elab_infer.ml:951`,
`elab_check.ml:166`) and never goes through `Expand.expand`. Anything in that
output that needs expanding is therefore skipped: nested macro calls, operator
templates, and binder renaming. Untyped macros do get their output expanded.

## Evidence

Found by [syntax-vs-surface-ir-layer](syntax-vs-surface-ir-layer.md) (defect A).

```
do macro one(_) -> Syntax.i64(1)
   macro m(e) -> Syntax.ap(Syntax.var("one"), e)
   y : I64 = m(0); y end                                   → 1
do macro one(_) -> Syntax.i64(1)
   macro m(e) : Expr(A) do do _ = A; Syntax.ap(Syntax.var("one"), e) end end
   y : I64 = m(0); y end                                   → ElabError(UnboundVariable "one")
```

## Direction

In both call sites, expand the macro output before lowering it. The elaborator
already holds the macro-runtime capability
([expander-handle-is-a-capability-not-a-context](expander-handle-is-a-capability-not-a-context.md)),
so expanding should be reachable through it rather than through a borrowed
expander. The two sites are near-duplicates, so they should share one helper.
Add the probe above as a regression test.

Do this before [delete-surface-ir](delete-surface-ir.md), which changes the same
call sites.

## Second defect at the same site — silent meta on failed unwrap

Found by the [macro domain-model pass](../topics/core-tt-domain-model-macros.md)
(M6): when the type-aware macro's result fails to unwrap as syntax, the
elaborator's `None` branch silently mints a fresh meta — a failed expansion
becomes an unsolved hole, surfacing (if at all) as an unrelated type error far
from the macro call. The model makes it an error naming the macro, and the fix
belongs in the same shared helper as the expansion fix above.
