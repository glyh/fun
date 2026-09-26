---
title: Mutual type chains in scoped do-heads (TypeDef)
parent: ../fun-design-map.md
status: open
assignee:
blocked_by:
---

# Mutual type chains in scoped do-heads

> ## Re-measured 2026-09-26 — the premise does not reproduce on the port
>
> Unblocked (`mutually-recursive-nominal-types.md` closed) and re-framed: the ticket describes
> `parse_type_binding` in `enforest.ml`, which went with the prototype, and its surface spelling
> (`do type A = … and B = …; body`) went with the `do … end` syntax. Probed through the suite's
> `--file` mode on the port (2026-09-26), the chains **work**:
>
> | probe | result |
> | --- | --- |
> | `{ type A = MkA \| MkB and B = MkC; (MkA : A) }` | `VALUE MkA` |
> | `{ type A = MkA \| MkB and B = MkC; (B.MkC : B) }` | `VALUE MkC` |
> | `{ rec A = enum { MkA } and B = enum { MkB(A) }; 1 }` | `VALUE 1` (and `{ …; (A.MkA : A) }` is `VALUE MkA`) |
> | `{ rec A = struct { x : I64 } and B = struct { a : A }; 1 }` | `VALUE 1` |
>
> Not one of them hit the targeted "scoped head accepts exactly one type" error this ticket was
> written about — so the expression-position group knot appears to be **already built**, and what
> remains is a decision, not work: close it, or turn the statement into the case the suite lacks.
>
> One neighbour measured on the way, **not** this ticket's question: a **mixed** group is refused —
> `rec A = enum { MkA(B) } and B = struct { a : A }` → *"a rec … and … group holds enums, struct
> types or functions, not a mix"*, at block scope and in a module alike. Where exactly that
> boundary sits is unprobed (`elaborate/elab-109` mixes a struct type with a function and passes).
>
> The original text is kept below; its mechanism names the deleted prototype.

## Question

Let `and` chains appear in scoped type heads — `do type A = … and B = …; body`
(`TypeDef`), where the group binds over a body expression rather than over
following bindings. Currently the parser's `parse_type_binding` is shared between
module items, struct items, and scoped heads (`scoped_binding_to_expr`, enforest.ml
~1313); the scoped-head site accepts exactly one type and a chain there errors.

## Context

- Deferred during the grilling of
  [mutually-recursive-nominal-types.md](mutually-recursive-nominal-types.md)
  (decision: chains in module and struct binding positions first; scoped heads
  reject chains with a targeted error until this ticket).
- Cost of full support: `TypeDefGroup`-shaped variants in both Syntax and Surface,
  and updates at ~11 sites across 9 files — the 1:1 lowering maps,
  `enforest_template.ml` captures, both `expand.ml` walkers (scope algebra for N
  names × params × ctors threaded into payloads and body), two
  `elab_effect_collect.ml` analyses, `elab_surface_rewrite.ml`, and the
  expression-position knot in `elab_infer.ml:801` (group knot inside `infer`,
  body elaborated under N defined names).
- No known use case yet; Reflect-Match needs module-level `Expr`/`Branch`, which
  does not require this.

## Resolution

_Unresolved._
