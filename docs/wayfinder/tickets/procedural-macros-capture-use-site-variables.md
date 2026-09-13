---
title: Procedural macros capture use-site variables
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Procedural macros capture use-site variables

## Question

Suppose a procedural macro builds a binder with the `Syntax.*` builders and puts
its argument inside it. The binder then captures the caller's variable of the
same name. Syntax templates don't do this. Why do procedurally built ids escape
hygiene?

## Evidence

Found by [syntax-vs-surface-ir-layer](syntax-vs-surface-ir-layer.md) (defect B).
The expected result in every case is 1.

```
do x = 1; macro m(e) -> Syntax.ap(Syntax.lam("x", e), Syntax.i64(2)); y : I64 = m(x); y end   → 2
do x = 1; macro m(e) : Expr(A) do do _ = A; Syntax.ap(Syntax.lam("x", e), Syntax.i64(2)) end end; y : I64 = m(x); y end → 2
do x = 1; syntax li do | li $body -> do x = 2; $body end end; y : I64 = li x; y end          → 1
```

Untyped and type-aware macros both capture, so this is not caused by the missing
expansion in
[type-aware-macro-output-is-not-expanded](type-aware-macro-output-is-not-expanded.md).
The existing `test_macro_hygiene_*` tests pass only because their macro-built
binder never wraps the argument.

## Leads (unverified)

- `Syntax.new_id` in the prelude creates ids with `scope = 0`. The macro-
  introduction scope may never be added to ids that come back from
  `Macro_eval.unwrap_stx` / `value_to_id`, so the binder and the use-site `x`
  would end up with scope sets where one is a subset of the other.
- Compare with the template path (`enforest_template.ml`), which handles this
  case correctly.

Diagnose with instrumentation first: print the scope sets on the binder and on
the use site at `Binding.resolve`.
