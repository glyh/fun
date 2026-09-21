---
title: "Port: a match stuck on a known scrutinee's unknown part waits"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a match stuck on a known scrutinee's unknown part waits

G6 of [the latent form gaps](port-latent-form-gaps.md), and an **undecided** row of
[the unported-path audit](port-unported-path-audit.md) — decided **2026-09-20 by the
user**, so this ticket is the ruling and its consequences.

## The ruling

> When a match's scrutinee head is **known** but a pattern inspects a sub-position
> holding a variable, the match is **stuck** — it waits. It does not take the default
> arm.

The port was already right about this and the prototype is wrong: this is a
**recorded divergence**, not parity (per the user's standing position that C# may be
more correct than the prototype, which is not maintained after the port).

## Why the prototype is wrong here

`lib/backend/interp/nbe.ml:784-810` (`eval_match_result_value`) states its own rule:

> Only a value whose head is unknown makes the match stuck. Every other value —
> constructors, atoms, types, products, records, closures — is matched by the decision
> tree; **a shape no pattern inspects has the `[Unknown]` domain**, so a variable or
> wildcard binds it.

So a known scrutinee always goes through the tree, and an occurrence whose value does
not resolve gets the `Unknown` domain — which means a pattern that *does* inspect it
fails to match and **the default arm wins**. The result is that a type-level test the
checker cannot make is silently decided by a default arm the runtime would never take.
Neutral types exist to defer exactly this; taking an arm instead makes conversion
depend on which patterns happen to be written after it.

Note this only arises when the **checker** evaluates a match (i.e. inside a type). At
run time values are closed and every pattern is decidable.

## Where the port refuses

`dotnet/src/Fun.Compiler/Nbe.Match.cs:86-88`:

```csharp
private static T Stuck<T>(Value value) => value is Value.VNeutral or Value.VVar or Value.VMeta
    ? throw new NotImplementedException("not ported yet: a match stuck on an unknown value")
    : throw new InvalidOperationException($"a match reached a {value.GetType().Name} its type rules out");
```

reached from `ValueAt` (`:63-79`, the occurrence walk) and from
`Nbe.Patterns.cs:14`/`:76`. `Nbe.StuckMatch.cs` already implements the *head*-unknown
half (`StuckMatch` returns null for a known scrutinee, and its note records this exact
gap). The fix is to extend that deferral to an unknown **occurrence**: an unresolvable
sub-value must produce a stuck match (an `FMatch` frame carrying the scrutinee) rather
than a default arm or a refusal — deliberately **not** copying the prototype's
`Unknown`-domain fallback.

Two more refusals in the same file become reachable once a stuck sub-occurrence is
representable, so they are part of this work, not separate:
`Nbe.StuckMatch.cs:50` (`ArmBinders` on a pattern kind) and `:54` (reading back an
unreachable arm of a stuck match).

## First task: the shared case

**Construct the program that reaches it before writing any code.** The shape is

```fun
{ T = fn(x : Option(I64)) { match (Some(x)) { Some(Some(y)) => A, _ => B } };
  … }
```

with `x` a variable, and — the part that makes it observable — the match must be
**demanded by the checker**, i.e. it must appear where its *value* is needed, such as a
type annotation or a type-case scrutinee. A match in a lambda body that is never
applied is not evaluated at all and proves nothing (probed 2026-09-20: three plausible
shapes reached nothing). Budget real time for this; if the case cannot be made
observable, say so and report rather than porting blind.

Then, per convention 5 (a prototype defect): add the case with the `.expect` the
*correct* behaviour gives, and **list the case in `test/conformance/prototype-divergences.txt`**
naming this ticket. The OCaml runner expects a listed case to fail — it will, because
it takes the default arm — and it reports a listed case that *passes*, which is how the
divergence gets noticed if the prototype is ever corrected.

## Reading

- `lib/backend/interp/nbe.ml:556-558` (`stuck_match` — the frame the port mirrors),
  `:784-810` (the rule and the fallback this ticket rejects)
- `dotnet/src/Fun.Compiler/Nbe.StuckMatch.cs` (all of it — the head-unknown half and
  the note naming this gap), `Nbe.Match.cs`, `Nbe.Patterns.cs`
- [the latent form gaps](port-latent-form-gaps.md) G6, and
  [match on a closure crashes the evaluator](match-on-a-closure-crashes-the-evaluator.md)
  (closed) for the decision that *only* an unknown head makes a match stuck — this
  ticket refines it: an unknown head **or an unknown part a pattern inspects**.
