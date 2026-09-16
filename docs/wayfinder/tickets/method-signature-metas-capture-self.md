---
title: A meta in a method's signature captures `self`, so calling the method fails
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A meta in a method's signature captures `self`, so calling the method fails

Found by the small-followups-2 run (2026-09-16); pre-existing, unrelated to
effect rows.

## Defect

A method's parameter types are elaborated with `self` already bound
(`Ctx.bind_anonymous` in `Elab_infer`'s `method_type` and `elaborate_method`), so
an **inserted meta** in a parameter's type carries `self` in its spine. At a call
the method's `VPi` is applied to a real struct value, that spine entry is no
longer a variable, and solving the meta is no longer a pattern problem.

```fun
{ S = struct { k : I64; pub method keep(r : Ref(I64)) : Ref(I64) { r } };
  s = S{ k = 2 }; x = ref(40); y = s.keep(x); 7 }
// UnifyError(VarNotInSpine(59))

{ S = struct { k : I64; pub method bump(r : Ref(I64)) ->{Mutate(r)} I64 { deref(r) } };
  s = S{ k = 2 }; x = ref(40); s.bump(x) }
// UnifyError(NonVariableInSpine)   — the spine's first entry is the record
```

`Ref(I64)` is `Ref(?h)(I64)`, so the hidden heap is the meta; any implicit
argument in a parameter type does the same. The declaration alone elaborates; only
the call fails. The same signature on a plain `fn` works, because there is no
`self` binder to substitute.

## Direction

The method's signature should not capture the self binder: elaborate parameter
types, the result and the row in a context where `self`'s entry is not part of a
meta's spine (e.g. create signature metas before binding `self`, or abstract them
over it), so a call substitutes only variables. Check `method_type` and
`elaborate_method` together — they build the same `VPi` twice and
`unify_method_types` compares them.

Regression tests: both programs above, plus a method whose parameter type is a
user type with an implicit argument.
