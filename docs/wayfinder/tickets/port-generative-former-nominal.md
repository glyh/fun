---
title: "Port: a parametric nominal in a generative module"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a parametric nominal in a generative module

Found by [port-nominal-identity](port-nominal-identity.md)'s fork on 2026-09-20, and
**verified by the integrator**: the prototype answers `1`, the port throws

```
not ported yet: sealing a generative nominal that is not bound as a module member
```

(`dotnet/src/Fun.Compiler/Elaborator.Generative.cs:52`) for

```fun
{ Mk = fn(u : Unit) { module {
      table = ref(0);
      pub type Box(A) = Bx(A);
      pub mk = fn(A : Type, a : A) { table <- deref(table) + 1; Bx(a) } } };
  b1 = Mk(());
  g = fn(x : b1.Box(I64)) { 1 };
  g(b1.mk(I64, 3)) }
```

This was an *undecided* row in [the unported-path audit](port-unported-path-audit.md)
("Undecided — needs the user", item 8): the probe decides it, and the verdict is **real
gap**, no ruling needed — sealing simply has not been taught about a former.

## Cause, per the fork that found it

The label map that records which nominal ids a generative module declared only
recognises a binding whose definition *is* a `Term.Nominal`; a type former's definition
is a `Lam` chain, so `Box` never gets labelled and sealing cannot find it. A correct fix
needs the prototype's separate `NomRef.params`, because C# `Term.Nominal` carries
captures but no parameter list — and an *unused* type parameter is not captured, so the
arity cannot be recovered from the captures either. Compare
[nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md)
("Finished (2026-09-16)", stamps and sealing by identity) for the prototype's shape.

Check first whether this is reachable more cheaply than by adding a param list to
`Term.Nominal`: `elab-067` used the *sealed head resolver*, so the pieces may already be
adjacent. Whatever the route, the fix is on the E11 paths, so read that ticket's
resolution before changing them.

## Tests

The program above is a shared case (`expect` `1`): the prototype answers `1`, so it is an
ordinary case, not a divergence. Add the companion errors that the same machine should
still produce (a `b1.Box` used where `b2.Box` is expected, with two `Mk(())`
evaluations) so the stamp keeps separating them once formers are labelled — the
non-parametric versions of both already exist as
`values/nominal-generative-type-case-separates` and `-rejects-other`.
