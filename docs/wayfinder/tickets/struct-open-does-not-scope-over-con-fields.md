---
title: Struct open does not scope over con_fields
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Struct open does not scope over `con_fields`

## Question

In a `struct … end`, an `open` scopes over the later *bindings* but not over the
record field types. Should it, or is the asymmetry the intended rule?

## Evidence

```
M = module pub T = I64 end
R = struct open M; f : T end    (* UnboundVariable "T" *)
S = struct open M; pub m = k end (* works *)
```

`Elab_infer`'s `Struct` case elaborates `con_fields` as a group *before* the
binding fold, so nothing in the binding list — an open included — can affect a
field type. The module form has no such split and behaves as expected.

Found while implementing
[module-level open](module-level-open-strict-imported-modules.md); the current
behaviour is documented on `Surface.OpenBinding` rather than fixed, because the
fix is a change to how structs elaborate, not to the open.

## Sketch of the work

1. Decide the rule. A struct is a record type *and* a namespace; "field types
   see nothing from the body" is defensible, but it makes `open` mean different
   things in `module` and `struct`, against Consistency > Flexibility.
2. If fields should see earlier opens, `con_fields` and `bindings` have to
   elaborate in one source-ordered pass instead of two phases — which is also
   what a field type referring to a type *bound in the same struct* would need,
   so check whether that is wanted at the same time.

## Resolution

_Unresolved._
