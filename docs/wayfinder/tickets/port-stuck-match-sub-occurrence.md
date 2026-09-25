---
title: "Port: a match stuck on a known scrutinee's unknown part waits"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed by the integrator, 2026-09-25 - the port half of the ruling was already landed (680187d) with the case values/stuck-match-sub-occurrence; the fork closed the one refusal it left reachable (a pruned arm) in 7be55ab, merged d7bb999, plus the case values/stuck-match-pruned-arm. Verified after merging - 183 xUnit, 751 conformance 0 failed, dune test green with 31 divergences.
assignee:
blocked_by:
---

# Port: a match stuck on a known scrutinee's unknown part waits

> ## Resolution (2026-09-25) — closed
>
> **The port half of the ruling was already in** when the ticket was picked up: `680187d`
> (*Port the stuck match on unknown parts*) landed the deferral for an unknown **occurrence**,
> and with it the case `values/stuck-match-sub-occurrence`. A fork spawned against this ticket
> measured that first rather than assuming it, then closed the one refusal the landed work left
> reachable.
>
> **What the fork added** (`7be55ab`, merged `d7bb999`): a **pruned arm**. A match whose tree
> inspects a sub-position holding a variable waits as an `FMatch` frame; reading it back opens
> **every** arm, and an arm an earlier one subsumes has no leaf to count its binders from —
> exactly the `Nbe.StuckMatch.cs:50` refusal this ticket listed as becoming reachable.
> `Term.Match` now carries its arms' `Patterns` (as the prototype's pattern-carrying frame
> always has) and `ArmBinders` takes a pruned arm's binder count from its pattern. Sites:
> `Core.Match.cs`, `Elaborator.Match.cs`, `Nbe.StuckMatch.cs`; new case
> `values/stuck-match-pruned-arm` (`expect` `5`) listed in `prototype-divergences.txt`, since the
> prototype cannot elaborate even the program whose own first arm matches.
>
> **Verified by the integrator after merging**, not taken on the fork's word: port
> `183/183` xUnit and `conformance: 751 cases, 0 failed`; `dune test` green with
> `conformance: 751 cases, 0 failed, 31 known prototype divergences`.
>
> The ticket's other "becomes reachable" site (`Nbe.StuckMatch.cs:54`, reading back an
> unreachable arm) is the second half of the same fix.
>
> Two follow-ups, filed rather than fixed here:
>
> - [a nested field pattern must work — it hangs today](port-nested-field-patterns.md)
>   — found while probing, re-verified by the integrator as pre-existing, and **ruled the same
>   day**: a nested field pattern is to work, so this is a feature (the port ahead of the
>   prototype on purpose), and the first `HANG` the repo's corpus could state.
> - `Term.Match.Patterns` is **not shifted** by `Core.Shift.MatchArm` (it uses `match with`), so
>   the stored patterns keep their elaboration-time indices. Safe today because only `Binders()`
>   is read — a count, invariant under shifting — and `Handler` is precedent for a non-shifted
>   field on the same record; but anything that later reads a pattern's terms must shift it.

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
