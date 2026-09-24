---
title: "Port: a parametric nominal in a generative module"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Resolution (2026-09-24) — closed

Merged from `pi-agent-ad9f5b16-e600-450` (`d7bc78c`, `9fb72ce`). The last E11 shape
lands: the ticket's program answers `1`, and the companion stays rejected.

- **Cause confirmed** (the third time this area's stated cause has had to be checked
  rather than trusted): the label map filtered `l.Def is Term.Nominal`, and a former's
  definition is a `Lam` chain over the `Term.Nominal`, so `Box` was never labelled,
  `GenerativeNominals[decl]` was `null`, and sealing refused. The former's captures are
  `[Var 0]` with the stamp **not** captured.
- **Fix, and why no `Term.Nominal` shape change was needed** (shapes are the port's to
  choose, ruled 2026-09-20): `GenerativeModule` now labels a binding by peeling its
  lambdas (`NominalHeadOf`, which also counts its parameters); the map's value became
  `GenerativeNominal(string? Label, int NumParams)` in `MetaContext.cs`; and sealing
  re-applies a former's trailing `NumParams` captures to the member projection, the way
  the prototype's `seal` does. `Term.Nominal`/`Value` are untouched, so `Unify`,
  `Nbe.Quote` and `Core.Shift` do not ripple.
- Cases added: `values/nominal-generative-former-shares-own` (`expect` `1`) and
  `values/nominal-generative-former-rejects-other` (`expect` `error`) — both ordinary,
  the prototype agreeing on each.
- Verified by the integrator after merging: C# conformance **721 → 723, 0 failed**;
  xUnit 182/182; `dune test` and `dune test test/conformance` green (723 cases, 0 failed,
  22 divergences).
- **Spun out**: two E11 gaps the fix does not reach, both re-verified here — a type-case
  still cannot separate two former instances (prototype `10`, port `11`), and a
  generative former with an *unused* type parameter is refused where the prototype
  accepts → [the generative former's identity residue](port-generative-former-identity-residue.md).

## Paused (2026-09-24) — resume here

The implementation fork was killed by a provider usage limit (resets 2026-09-24
18:33:48) after ~119 tool calls, one step short of starting the fix: its last words were
"Diagnosis is complete and confirmed on both sides", and it was heading off to check
the tests that reference `ctx.Metas.GenerativeNominals` before changing its type.

**Its branch `pi-agent-6cd6cf45-5d98-492` (`4fbcd72`) is instrumentation only** — two
`Console.Error.WriteLine` probes in `Elaborator.Generative.cs` (every member's `Def`
during labouring, and a stack trace where sealing throws). **Merge nothing from it**;
reuse the traces.

What it did establish, from its own words, all of which sharpens the fix:

- **The ticket's stated cause is confirmed this time** (the ticket itself warns that a
  neighbouring diagnosis, `elab-062`'s, was wrong once): `Box`'s definition is a `Lam`
  chain whose body is the `Term.Nominal`, and the label map filters on
  `l.Def is Term.Nominal`, so the former is never labelled.
- **The former's captures are `[Var 0]`, so the stamp is *not* captured** — the arity
  cannot be recovered from captures, which is exactly why the ticket says a parameter
  list is needed.
- **The prototype's nominal conversion compares `params`** as part of identity, which is
  what a port-side parameter list has to reproduce.
- **An *unused* type parameter errors `NonVariableInSpine` in the prototype** — a shaky
  corner there, to be probed or ruled on rather than copied.

Resume with `resume: "generative-former"` (its context still holds the traces and the
reasoning). Its full transcript is at
`/tmp/pi-subagents-1000/home-lyh-pullground-fun/01a0d1e2-8670-75e3-a2ef-72dadaf596b5/tasks/6cd6cf45-5d98-492.output`
— under `/tmp`, so treat it as a convenience and this section as the record.
