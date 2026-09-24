---
title: "Port: a generative former with an unused type parameter"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a generative former with an unused type parameter

Split out of [the generative former's identity residue](port-generative-former-identity-residue.md)
on 2026-09-24, so that it **waits on a ruling without blocking the other gap** in that
ticket. Verified by the integrator in both runners, at `32aa27e`.

## The program

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
| port | `not ported yet: sealing a generative former with an unused type parameter` (`Elaborator.Generative.cs` — the guard the landed E11 work added, which replaced a `Skip(-1)` crash) |

The guard is honest, but it refuses where the prototype accepts, so it is a gap rather
than a decision. **The same shape without a generative module is accepted in both**:

```fun
{ M = module { pub type Box(A) = Bx; pub mk = fn() { Bx } }; g = fn(x : M.Box(I64)) { 1 }; g(M.mk()) }
```
→ `1` in each runner. So the missing piece is specifically the **sealing path**: an unused
parameter is not among the nominal's captures, and re-applying the parameters cannot
reconstruct an argument that was never captured.

## The ruling this needs — do not implement before it

Two candidate answers, and the choice is semantic, not mechanical:

- **Mirror the prototype**: the parameter is *phantom*, so `b1.Box(I64)` and
  `b1.Box(Char)` are the same type. Then say what an application of such a former means
  afterwards (nothing observable?) and what the sealed `Dot(Var, label)` is applied to.
- **Keep the refusal, on purpose**: document it as the port's limit, in the port's own
  words rather than `not ported yet`, and record the prototype's phantom parameter as a
  divergence.

Recommendation to take to the user: mirror the prototype (the language's own two
implementations should agree on a phantom parameter; refusing it makes a legal
declaration unusable), with the divergence route as the fallback if the phantom behaviour
turns out to be a prototype accident.

## Tests

Whichever way it goes: the program above, plus a companion that applies the former at two
different types if the phantom route is taken — probed in both runners first, and listed
in `prototype-divergences.txt` only if the prototype proves to be the wrong one.

## Reading

- [the parametric nominal in a generative module](port-generative-former-nominal.md) — its
  Resolution describes `GenerativeNominal(Label, NumParams)` and the sealing path
- `dotnet/src/Fun.Compiler/Elaborator.Generative.cs` (`NominalHeadOf`, `Seal`, and the
  `NumParams > Captures.Length` guard)
- the residue ticket's section 2 for the evidence as it was first written up
