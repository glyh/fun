---
title: "Port: a type-aware operator macro"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a type-aware operator macro

Verdict of [the unverified rows](port-unverified-rows.md), row 1 — its `r1b` half (the
`r1a` half is parity: an **untyped** operator macro works in both runners). Verified by
the integrator 2026-09-24 at `32aa27e`.

## The program

```fun
{ infix (foo) (a, b) { Syntax.i64(9) };
  macro foo(x : Expr(I64)) : Expr(I64) { x };
  1 foo 2 }
```

| runner | output |
|---|---|
| OCaml | **hangs** — no output, killed at 25 s (`timeout 25 dune exec fun` → exit 124) |
| port | `not ported yet: a type-aware operator macro \`foo\`` (`Expander.Macros.cs:306`) |

The refusal is real: the port raises it for an operator entry whose `Signature` is not
null, which the prototype's `syntax_operator_arg` (`lib/expand/enforest_util.ml:142`)
admits. And the prototype never answers — it loops — so this is neither parity nor a
pass: an unported path in the port **and** a prototype defect.

## The ruling this needs — do not implement before it

The prototype's intent is the `MacroCall` deferral at `lib/expand/enforest.ml:681`: it
folds the whole operator use into **one** argument. The port instead splits the operands
by arity, and it is that split whose typed case has no implementation. So the shape of a
typed operator macro's argument is a semantic choice, not an implementation detail.

Take to the user: **the prototype's deferral** (one argument, the whole operator use) is
the recommendation — `foo`'s written parameter is `x : Expr(I64)`, one expression, not
one per operand — with the alternative being to keep the port's arity split and define
what each typed parameter means then.

## The test problem, which the ruling has to solve too

This program **cannot** join `test/conformance/cases` while the prototype loops: the
OCaml runner runs every case, so the suite would hang, and `prototype-divergences.txt`
only works for a case that *fails*, not one that never returns. Either the ruling
chooses a shape the prototype also refuses (then it can be a listed divergence), or this
becomes a documented exception to convention 6 — a port-only test in
`dotnet/test/Fun.Tests` naming this ticket — because conventions 6 and 8 cannot both be
satisfied by a program the prototype hangs on. Say which in the ruling, not in the code.

## Reading

- `dotnet/src/Fun.Expand/Expander.Macros.cs:306` (the refusal), `Enforest.Roles.cs`
  (operator roles and `ExpandOperatorUse`), `Expander.Macros.cs`'s `ExpandMacroCall`
- `lib/expand/enforest.ml:681` (the deferral), `lib/expand/enforest_util.ml:142`
