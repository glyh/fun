---
title: "Port: an implicit lambda checked against a function type instantiates"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: generalizing the argument to an unknown-typed function under a check

Found by [the implicit application](port-implicit-application.md) fork while
strengthening its cases, **verified by the integrator in both runners**: an inline
polymorphic lambda and the same lambda bound to a name behave differently, and the two
implementations are wrong in *opposite* directions.

```fun
-- inline: implicit-lambda-argument-inline.fun  (expect 7)
{ h = fn(g) { g[I64](7) }; h(fn[A : Type](a : A) { a }) }

-- named: implicit-lambda-argument-named.fun  (expect 7)
{ ch = fn[A : Type](a : A) { a }; h = fn(g) { g[I64](7) }; h(ch) }
```

| program | prototype | port |
|---|---|---|
| inline | `7` | `type mismatch: cannot unify VAtomTy with VVar` |
| named | `UnifyError(CannotUnify(function type vs function type))` | `7` |

## Ruled (integrator, 2026-09-20): both must be accepted

**Reaffirmed and widened by the user (2026-09-20): all three of these are accepted**, and
none of them is an error —

```fun
{ h = fn(g) { g[I64](7) }; h(fn[A : Type](a : A) { a }) }              -- inline, → 7
{ ch = fn[A : Type](a : A) { a }; h = fn(g) { g[I64](7) }; h(ch) }    -- named,  → 7
{ h = fn(g) { g[I64]; 7 }; h(fn[A : Type](a : A) { a }) }             -- no result applied, → 7
```

— so the third is already correct in both runners, and each implementation must stop
rejecting its half of the first two.

The two programs differ only by let-inlining — the same term, one written inline and one
bound first. **Generalization must not depend on that.** This is the project's own
stated priority (`Consistency > Flexibility > Correctness`, root `README.md`) applied to
the one place the port has already decided it is allowed to be more correct than the
prototype: the prototype is not maintained after the port, so its rejection of the named
form is a **prototype defect** and the port's rejection of the inline form is a **real
gap**. Both get fixed in C#; neither implementation gets to keep its half.

So this ticket has three pieces of work:

1. **Fix the port's check path** so the inline polymorphic lambda generalizes — the
   inline program must answer `7`. This is in `Check`/generalization (`Elaborator.cs`,
   `Elaborator.Generalise.cs`), not in the `Implicits` code the implicit-application fork
   mirrored, which is why that fork correctly left it alone.
2. **Record the named form as a divergence** (convention 5): the shared case
   `implicit-lambda-argument-named` with `expect` `7`, listed in
   `test/conformance/prototype-divergences.txt` naming this ticket. The OCaml runner will
   report a listed case that *passes*, which is how the divergence is noticed if the
   prototype is ever fixed.
3. **Add both as shared cases** and run them through both runners as the acceptance
   condition — after the fix the inline one is an ordinary case and the named one is a
   divergence; before the fix the inline one fails, which is the point.

## The cause — **corrected 2026-09-24: it is not generalisation**

The analysis this file used to carry claimed let-generalisation was the root. **It was
wrong, and the fork that re-examined it proved so with the port's own traces:**
`Generalise` is *called* on `h` and returns early, because `ClosedUnder(body, 1)` is false
for `h = fn(g) { g[I64](7) }` — its body mentions the global `I64`. Both implementations
decline to generalise, so neither candidate (a) nor (b) is the mechanism. The integrator
verified the three code facts this rests on: the two implicit-expected cases in `Check`
(`Elaborator.cs:313`, `:319`) are guarded by
`stx is not Syntax.Lam { Param.Explicitness: Explicitness.Implicit }`; the `(Lam, VPi)`
case unifies the written parameter type against `pi.Domain` (`:334`) and binds its own
parameter rigidly; and `ClosedUnder` is `v.Index >= under + depth`.

**The real mechanism.** A *name* in check position against an implicit `Pi` takes
`CheckUnderImplicit`, which instantiates it (`InsertImplicitArgs`) and unifies — that is
why the named form works in C#. An *inline implicit lambda* skips that path by the guard
above, lands in `(Syntax.Lam lam, Value.VPi pi)`, and binds its own implicit parameter
**rigidly**. For `h(fn[A : Type](a : A) { a })`, with the call site's domain already
solved, the trace is:

```text
[C] param A#2 pi.Domain=VMeta[?563] forced=VU
[C] param a#3 pi.Domain=VMeta[?565] forced=VAtomTy
[U] … ?565 -> VAtomTy(I64) … ?566 -> UNSOLVED   <- unify(?565, TypeValue(a : A)), A rigid
[G] unsolved=[566] typehead=VPi / [G] ?566 -> UNSOLVED / [X] open var idx=56  <- ClosedUnder says no
```

So `a : A` meets `I64` while `A` is a rigid variable — the error message the wrong
analysis predicted, from a different cause. A lambda checked against a function type
should *instantiate*, exactly as the already-decided
[check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md)
rule does for names.

**The plan** (the research fork's, with the integrator's verification of the cited code):

1. Let `CheckUnderImplicit` fire for an implicit `expected` `Pi` even when `stx` *is* an
   implicit lambda — drop the `stx is not Syntax.Lam { Implicit }` guard.
2. Restrict the `(Lam, VPi)` case to explicit lambdas
   (`when lam.Param.Explicitness == Explicitness.Explicit`), so a type-level implicit
   lambda falls through to infer + `InsertImplicitArgs`.

**The one spot that needs care, and may need a ruling:** the scoping must distinguish a
type-level implicit parameter (`[A : Type]`) from a dictionary or effect-row implicit
parameter, or `InsertHiddenDicts` is lost — the fork *observed* that a naive version
breaks the prelude (`dotnet/std/stage2.fun:23-24` defines `(==)`/`(!=)` as
`fn[A : Type](lhs, rhs) { … }` checked against `[A : Eq] -> …`), with "missing
implementation of Eq" / "cannot unify VPi with VPi". Two routes: gate on the implicit
parameter's written type being `Type`, or teach the instantiated path to run
`InsertHiddenDicts`. Do not paper over that.

Tests: `values/implicit-lambda-argument-inline` (`expect` `7`, an ordinary case) and
`values/implicit-lambda-argument-named` (`expect` `7`, listed in
`prototype-divergences.txt` naming this ticket). The third program already passes in both
runners, so it is covered by neither.

## Resolution (2026-09-24) — closed

Fixed and merged (`port-generalise-under-check` @ `7ae6084`, `02e8406`, merged as
`6a56b8d`). The inline program answers `7`; the named and no-result programs were already
right and stay right. **Verified by the integrator:** C# 731 → **733 cases, 0 failed**;
xUnit 182/182; `dune test` and `dune test test/conformance` green — 733 cases, 0 failed,
**27** divergences (the 26 plus this ticket's named-form entry).

- **Cause, confirmed in code:** the guard this ticket cited sent *every* implicit lambda
  to `(Lam, VPi)`, which unifies the written parameter type and binds the parameter
  rigidly; at the call site the domain is already `I64`, so `a : A` failed with `A` rigid.
- **The hazard was wider than this ticket predicted**, and only probing found it: the
  naive guard-drop broke **16** cases, not merely `stage2.fun`'s `(==)`/`(!=)`. The whole
  type-case family (`elab-041`, `core-072..084`, `type-case-refines-variable`) and the
  value-level implicit `[n : I64]` case need the rigid treatment — heterogeneous matches
  are not inferable, and a value implicit's runtime value must flow through the lambda.
- **The gate, which is the actual finding:** `BindsImplicitParameterItself`
  (`Elaborator.Implicits.cs`) binds the parameter itself iff the expected domain is not
  `Type` (dictionary / effect-row / value parameter), or the codomain probed at a fresh
  meta `Unify.Mentions` the parameter — i.e. **dependency decides**. A param-independent
  expected (`[A : Type] -> I64 -> ?r`) instantiates, exactly as a name does. Hidden
  dictionaries are covered by the same rule, so `InsertHiddenDicts` is untouched.
- **Also fixed on the way:** inline and named implicit lambdas now behave identically
  against *explicit* annotations — `g : I64 -> I64 = fn[A : Type](a : A) { a }` used to
  throw "applying non-function" while the named form passed.
- **Spun out** (no suite case exercises either, so not defects): the gate's two uncovered
  corners — a *parameter-independent* dictionary whose body needs the evidence, and a
  written parameter type that mentions the parameter inside a non-inferable body →
  [when an implicit lambda should bind rigidly, and when it should instantiate](implicit-lambda-rigid-or-instantiate.md).

## Also recorded

Cosmetic, from the same fork: the prototype prints `<lam>` where the port prints `VLam`
when a value is described. Not worth a ticket on its own; fix it if a case ever becomes
observable.

## The first attempt (2026-09-24) — superseded

**Superseded the same day:** the replacement fork finished the work, and its verdict is
the corrected cause above. The traces recorded below are what produced that verdict, so
they are kept as the record of how it was reached.

The research fork was killed by a provider usage limit (resets 2026-09-24 18:33:48)
mid-turn, after ~70 tool calls. Its own last words were that "the real causes are now
pinned", but **that analysis never reached the integrator**, so nothing here is settled:
the recommendation is still open and the ticket still needs its answer.

What did survive is its instrumentation, as a single auto-squashed commit on
`pi-agent-e3a08282-3aab-45d` (`83f0bbb`): markers in `Elaborator.Generalise.cs`
(`[G]` unsolved metas and what each solved to) and `Elaborator.cs` (`[U]` the metas at a
unification failure, `[C]` the Pi domain a written lambda parameter is checked against,
`[X]` the variable that fails `ClosedUnder`). **It is trace output only — no functional
change — so merge nothing from that branch**; reuse the traces.

Resuming: `Agent({resume: "generalise-check"})` keeps the dead agent's context, so its
pinned diagnosis may still be in there; if that fails, a fresh fork should start from
the traces above rather than from scratch. Either way, the deliverable is the same: which
side moves (a) or (b), with the evidence, and no feature code.
