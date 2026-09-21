---
title: "Port: the 13 parity throws become language errors (convention 2)"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: the parity throws become language errors

From [the unported-path audit](port-unported-path-audit.md), section "Parity: the
throw should become a `FunException`". Mechanical work, no design, but it is the
difference between an honest runner and a lying one.

## Why

[Porting convention 2](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16): an
unported form raises `NotImplementedException`; `FunException` is only for a genuine
language error, and **a missing feature must never make a case expecting `error`
pass**. Each site below is a **parity** row: the prototype refuses that form too, so
the port's refusal is correct — but it is raised as the wrong *kind* of refusal, so
an `expect error` case fails in C# instead of passing.

The live example, reproduced end to end by the integrator (2026-09-20):

```fun
{ 1 + 2 ~> 3 }
```

The prototype parses `(1 + 2) ~> 3` and type-errors; the port answers
`not ported yet: the polymorphic arrow ~>` because
`dotnet/src/Fun.Expand/Enforest.Roles.cs:148` throws **before** its `continues` guard.
A case expecting `error` fails in C#, not for lack of coverage but because the refusal
is the wrong kind. Fix the guard order here at least.

## The sites, with the prototype's refutation

| site | prototype |
|---|---|
| `Fun.Expand/Enforest.cs:220` | `enforest.ml:1445` |
| `Fun.Expand/Enforest.cs:291` | `enforest.ml:514` |
| `Fun.Expand/Enforest.cs:396` | `enforest_util.ml:342` |
| `Fun.Expand/Enforest.cs:412` (bare brackets) | `enforest.ml:87` |
| `Fun.Expand/Enforest.Roles.cs:148` (`~>`) | `enforest.ml:684` |
| `Fun.Expand/Enforest.Roles.cs:338` (dotted order group) | an open item on [brackets-decide-grouping](brackets-decide-grouping.md) |
| `Fun.Expand/Enforest.Traits.cs:143` | `enforest.ml:407` |
| `Fun.Compiler/Elaborator.cs:506` | `elab_resolve.ml:387` |
| `Fun.Compiler/Elaborator.Export.cs:52` | `elab_infer.ml:150` |
| `Fun.Compiler/Elaborator.Patterns.cs:47` | `elab_patterns.ml:187` |
| `Fun.Compiler/Elaborator.Structs.cs:80` | `enforest.ml:1509` |
| `Fun.Compiler/Elaborator.Structs.cs:209` | `elab_infer.ml:936` |
| `Fun.Compiler/Elaborator.Structs.cs:244` | `elab_type_expr.ml:74` |

Two of the prototype references are themselves open decisions in this repo
(`brackets-decide-grouping`), so for those the port must produce *some* language
error; which one is a detail, not a ruling.

## Also in this ticket

The audit's other two classes, which are cheap and belong with it:

- **Unreachable (17 sites)** — delete the lying catch-all. Convention 2 again: a
  catch-all that claims "not ported yet" about a path no program reaches misleads the
  next reader about what exists.
- **`Nbe.StuckMatch.cs:12`** — the one stopgap the audit called a real divergence
  rather than a shared choice: only an unknown *scrutinee* waits, where a known
  scrutinee with an unknown *part* should also wait (the prototype's rule). It is
  covered, with its undecided half, by
  [the latent form gaps](port-latent-form-gaps.md#g6-a-stuck-match-on-a-known-scrutinees-unknown-part).

## Tests

Each converted site that a program can reach gets a shared `expect error` case — that
is the point of the change, and the reason the count will move *up* rather than
holding. Verify each new case fails in the prototype before listing it in
`test/conformance/prototype-divergences.txt`; a case that passes in both is a normal
case, not a divergence.
