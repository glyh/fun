---
title: "Port: the generative former's identity residue"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed by the integrator, 2026-09-25 - section 1 (a type-case telling two former instances apart) was already implemented and merged the same day this ticket was written (1f70e82, merged 5a685ba, with the case values/nominal-generative-former-type-case-separates). Section 2 was split out to port-generative-former-phantom-parameter, ruled 2026-09-25 and queued.
assignee:
blocked_by:
---

# Port: the generative former's identity residue

> ## Resolution (2026-09-25) — the ticket was stale; both halves are closed
>
> **Section 1 was already fixed when this ticket was written.** The fix landed 2026-09-24
> 21:04 as `1f70e82` (*E11: capture a generative former's stamp in type-case*, merged
> `5a685ba`), with the case at 21:12; the ticket's own "the port reports `11`" prose was
> written around 2026-09-24 before that merge. Re-measured by the integrator 2026-09-25 on
> `9183016`:
>
> ```text
> C#     --file values/nominal-generative-former-type-case-separates.fun  → VALUE 10
> OCaml     _build/default/bin/differential.exe (same file)               → VALUE 10
> .expect                                                                  → 10
> test/conformance/prototype-divergences.txt entries for it                 → 0
> ```
> The fix is in `dotnet/src/Fun.Compiler/Elaborator.RecTypes.cs`, not the `Nbe.Generative.cs`
> comparison this ticket points at: two sites dropped the declaration-site scope (which
> carries the stamp) — `PredictCaptures`/`Body` reset `Enclosing` while peeling the former's
> parameters, and the recursive-enum re-elaboration loop ran the enum body through `InferLam`.
> Both now bind the parameters over the declaration-site context without resetting `Enclosing`,
> matching the prototype's `elab_type_group`.
>
> **Section 2 is ruled and queued**, not open work here:
> [an unused type parameter is an error at its declaration](port-generative-former-phantom-parameter.md).
>
> A fork was spawned against this ticket on 2026-09-25 and correctly reported *nothing to do*.
> Measure a ticket's gap in the runners before spending a fork slot on it.

One remaining E11 gap. The fork that landed
[the parametric nominal in a generative module](port-generative-former-nominal.md) found
two; the second was split out on 2026-09-24 because it waits on a ruling and this one does
not — it is
[a generative former with an unused type parameter](port-generative-former-phantom-parameter.md).
What is left here was **re-verified by the integrator** in both runners on 2026-09-24,
after that work merged (`921da47`+).

## 1. A type-case cannot tell two former instances apart (prototype `10`, port `11`) — **CLOSED, see the Resolution above**

```fun
{ Mk = fn(u : Unit) { module {
    table = ref(0);
    pub type Box(A) = Bx(A);
    pub mk = fn(A : Type, a : A) { table <- deref(table) + 1; Bx(a) } } };
  b1 = Mk(()); b2 = Mk(());
  f = fn(t : Type) { match (t) { b1.Box(I64) => 1, _ => 0 } };
  f(b1.Box(I64)) * 10 + f(b2.Box(I64)) }
```

| runner | output |
|---|---|
| OCaml | `10` |
| port | `11` |

The two `b1.Box(I64)` and `b2.Box(I64)` are distinct types — the companion case
`values/nominal-generative-former-rejects-other` passes, so sealing keeps them apart in
the *elaborator*. But a former's nominal does not capture its module's **stamp**, so
type-case cannot separate them, and the port reports `11`. The non-parametric versions
already separate (`values/nominal-generative-type-case-separates`), so the missing piece
is the stamp on a former's nominal — the same "stamp not captured" fact the paused fork
reported as `captures = [Var 0]`.

Test: the program above is an **ordinary** shared case once fixed (`expect` `10`; the
prototype answers `10`), so nothing is added to `prototype-divergences.txt`.

## 2. Moved out (2026-09-24) — the unused type parameter

Now its own ticket — [a generative former with an unused type parameter](port-generative-former-phantom-parameter.md)
— because it waits on a ruling and this ticket's gap does not. The evidence below is kept
as the record it was first written from.

```fun
{ Mk = fn(u : Unit) { module {
    table = ref(0);
    pub type Box(A) = Bx;
    pub mk = fn() { table <- deref(table) + 1; Bx } } };
  b1 = Mk(());
  g = fn(x : b1.Box(I64)) { 1 };
  g(b1.mk()) }
```

| runner | output |
|---|---|
| OCaml | `1` |
| port | `not ported yet: sealing a generative former with an unused type parameter` (`Elaborator.Generative.cs`, the guard the landed work added) |

That guard is honest — it replaced a `Skip(-1)` crash — but it is a refusal where the
prototype accepts, so it is a gap, not a decision. **The same shape without a generative
module is accepted in both** (`{ M = module { pub type Box(A) = Bx; pub mk = fn() { Bx } };
g = fn(x : M.Box(I64)) { 1 }; g(M.mk()) }` → `1` in each), so the missing piece is
specifically the sealing path: an unused parameter is not among the nominal's captures,
so re-applying the parameters cannot reconstruct it.

**This half may need a ruling before it is implemented:** the prototype treats the
parameter as *phantom* (`Box(I64)` and `Box(Char)` are then the same type), and sealing
cannot re-apply an argument that was never captured — so the choice is between mirroring
the phantom parameter (and defining what an application of it means afterwards) and
documenting the refusal as the port's limit. Ask; do not guess.

## Reading

- [the parametric nominal in a generative module](port-generative-former-nominal.md) —
  its Resolution describes the label map and the `GenerativeNominal(Label, NumParams)`
  the sealing path now reads
- `dotnet/src/Fun.Compiler/Elaborator.Generative.cs` (`NominalHeadOf`, `Seal`)
- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md)
