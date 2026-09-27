---
title: Mutual type chains in scoped do-heads (TypeDef)
parent: ../fun-design-map.md
status: closed
closed_date: 2026-09-27
resolution: Closed 2026-09-27 without work, by the integrator. Chains in scoped heads work on the port, and the suite already covers the shape (`values/elab-014` is the ticket's own program, plus `elab-107`, `elab-163`) - so there is no case to add and nothing to build. The re-measure note's one open neighbour is closed too: `elaborate/elab-109` does not "pass" a mixed group, its `.expect` is `error`, and the refusal is uniform across four spellings at block and module scope (probed).
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
> types or functions, not a mix"*, at block scope and in a module alike.
>
> **Correction 2026-09-27** — that boundary is not unprobed, and the example given for it was read
> wrong. `elaborate/elab-109` (struct type + function) has `.expect` = `error`; it is a case *for*
> this refusal, not a counterexample. Probed at four spellings — enum+struct, struct+fn in both
> orders, and enum+struct inside a module — every one gives the same refusal, so there is no
> separate boundary question here.
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

**Closed 2026-09-27 — nothing to build.** Measured, not inferred:

| probe | result |
| --- | --- |
| `{ type A = MkA \| MkB and B = MkC; (MkA : A) }` | `VALUE MkA` |
| `{ rec A = enum { MkA } and B = enum { MkB(A) }; (B.MkB(A.MkA) : B) }` | `VALUE MkB` |

and the shape is already in the suite: `values/elab-014` is the first probe verbatim, with
`elab-107` (structs, block scope) and `elab-163` (enums, block scope) alongside. The "Cost of full
support" section above prices work against the deleted prototype's mechanism names;
`TypeDefGroup` in Syntax and Surface is not needed, because the port's `TypeBinding` carries one
node per chain through the whole pipeline (see
[mutually-recursive nominal types](mutually-recursive-nominal-types.md)).

What was *not* built is the prose: the ticket's own `## Question` asked for a kind of head the
port already accepts. The only residue was the mixed-group boundary named in the note above, and
that is settled there.
