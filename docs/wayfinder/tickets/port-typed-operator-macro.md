---
title: "A type-aware operator macro"
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A type-aware operator macro

**Post-port work, by ruling — not a port gap.** Decided by the user on 2026-09-24, while
[the unverified rows](port-unverified-rows.md) were being probed (its row 1): a typed
operator macro is deferred, because **parity here is "neither implementation has it"** —
the OCaml prototype has no working one, it loops — and this is something wanted *after*
the port, not before it.

## What the probe measured (integrator, `32aa27e`)

```fun
{ infix (foo) (a, b) { Syntax.i64(9) };
  macro foo(x : Expr(I64)) : Expr(I64) { x };
  1 foo 2 }
```

| runner | output |
|---|---|
| OCaml | **hangs** — no output, exit 124 at 25 s |
| port | `not ported yet: a type-aware operator macro \`foo\`` (`Expander.Macros.cs:306`) |

The port's refusal is the honest-runner form (convention 2) and **stays exactly as it
is**: a present-but-unimplemented path raises `NotImplementedException`, never a
`FunException` — a language error is a different thing, and a case expecting `error`
must not start passing because a feature is missing. No port change follows from this
ticket.

The *untyped* form already works in both, which is why row 1 split in two:

```fun
{ infix (foo) (a, b) { Syntax.i64(9) }; macro foo(e) { Syntax.i64(1) }; 1 foo 2 }   -- 1 in both
```

## What is known when it is picked up

- The prototype's **intent is legible even though its execution loops**:
  `lib/expand/enforest_util.ml:142` (`syntax_operator_arg`) admits an operator entry
  whose `Signature` is not null, and `lib/expand/enforest.ml:681` defers the use as a
  `MacroCall`, folding the **whole operator use into one argument**.
- The port's `Expander.Macros.cs:306` splits the operands by arity instead, which is why
  its typed case has nothing to do.
- So the open question is the **shape**, and it is a language question rather than a bug:
  does the macro receive one syntax argument covering `1 foo 2`, or one per operand? The
  prototype's intent reads as the former — `x : Expr(I64)` is one expression.
- **The prototype's hang is a prototype defect**, recorded here because it is the reason
  this could not be probed into a shared case. The prototype is not maintained after the
  port, so nothing gets fixed there; the record is for whoever implements this.
- **It cannot become a shared conformance case while the prototype loops.** The OCaml
  runner runs every case, so the suite would hang, and `prototype-divergences.txt` covers
  a case that *fails*, not one that never returns. Whoever takes this must decide the
  test story explicitly — a port-only xUnit test naming this ticket is the likely
  exception to convention 6 ("a source-to-result test is a conformance case, never
  xUnit"), and it should be argued in the ticket rather than slipped in.

## Reading

- `dotnet/src/Fun.Expand/Expander.Macros.cs:306`, `Enforest.Roles.cs`, and
  `ExpandMacroCall` in `Expander.Macros.cs`
- `lib/expand/enforest.ml:681`, `lib/expand/enforest_util.ml:142`
- [the unverified rows](port-unverified-rows.md) row 1 for the probe and its evidence
