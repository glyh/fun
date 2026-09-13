---
title: Procedural macros capture use-site variables
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Fixed by the Racket-faithful direction. Scope sets survive the round trip. Every macro application (untyped, type-aware, decl and operator) goes through `Expand.application`, which adds a use-site scope and an intro scope to what the macro receives and flips the intro scope on what it returns. All three repros answer 1 in `test_macro_does_not_capture_argument`, which fails with an ambiguous binding when the contract is disabled.
closed_date: 2026-09-14
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

## Diagnosis (done — from the domain-model pass)

Instrumented by dumping every id's scope set after `Expand.expand` on repro A.
The binding table tells the whole story:

```
table[x] scope={4} resolved=x__0   <- the macro's Syntax.lam("x", …) binder
table[x] scope={0} resolved=x      <- the caller's x = 1
occurrences: … x__0{4}; x__0{4} …  <- binder, and the SPLICED e — now x__0{4}
```

The spliced `e` arrives with its occurrence scope already destroyed, so at
resolve time it carries only the binder's fresh scope `{4}` and cannot see the
caller's `{0}`. The capture is real: the macro's binder legitimately won.

Two mechanisms compound:

1. **The macro value boundary drops scope sets.** A macro argument travels
   `Syntax.t` → value → `Syntax.t`, and the way back is `Macro_eval.value_to_id`
   (macro_eval.ml:237), which hardcodes `scope = Scope_set.empty`. The scope
   the occurrence had at the call site is gone before expansion ever sees it.
   The forward direction is equally lossy — `id_to_value` writes a literal
   `scope = 0` field nothing reads back.
2. **Scope addition is bounded by source region, and synthetic regions are
   always inside** (`Expand.span_contains`: `outer.synthetic || inner.synthetic`).
   So when the `Lam` case adds the binder's fresh scope `{4}` within the
   macro-built lam, the *substituted* occurrence receives it too. A
   scope-carrying splice would become `{0,4}` — ambiguous-binding error, loud
   rather than silently captured; a scope-less splice becomes `{4}` — captured.

Templates are immune because `substitute_template_captures` splices the
**captured `Syntax.t` directly** — no value round-trip, no scope loss — which
is why case C answers 1.

### Direction

Minimal (stops the silent capture, does not reach Racket semantics): make the
round trip lossless — carry the scope set through `id_to_value` / `value_to_id`
so a splice keeps its occurrence scope. Repro A then fails as an ambiguous
binding instead of answering 2.

Full (Racket-faithful): a macro-introduced binder's scope must reach only ids
*written* in the expansion, not ids substituted into it. That needs the
substituted-vs-written distinction the region rule approximates — e.g. an
intro scope on the expansion plus scope addition that skips spliced subtrees.
Note templates already mint one (`Enforest_template.fresh_intro_scope`);
procedural macro output gets none, which is also why free ids built with
`Syntax.var` fall through to string resolution (see the domain model's
two-tier resolution, S5).

Either way, `test_macro_hygiene_*` pass today only because their macro-built
binder never wraps the argument, as this ticket's evidence already suspected.
Add the three repros here as regression tests when fixing.
