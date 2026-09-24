---
title: "Port: the generative former's identity residue"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: the generative former's identity residue

Two remaining E11 gaps, both found by the fork that landed
[the parametric nominal in a generative module](port-generative-former-nominal.md) and
both **re-verified by the integrator** in both runners on 2026-09-24, after that work
merged (`921da47`+). They are one ticket because they are the same nominal (a *former* in
a generative module); if the fixes diverge, split it.

## 1. A type-case cannot tell two former instances apart (prototype `10`, port `11`)

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

## 2. A generative former with an *unused* type parameter is refused (prototype `1`)

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
