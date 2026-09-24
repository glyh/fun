---
title: "Port: the implicit-lambda gate ignores the codomain's residual shape"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: the implicit-lambda gate ignores the codomain's residual shape

**A verified port bug, not a design question** — and one the shared suite cannot see, because
no case reaches it. Raised as a design question by the fork that landed
[an implicit lambda checked against a function type instantiates](port-generalise-under-check.md),
then probed by a second fork on 2026-09-24 and **confirmed by the integrator in both runners**.
The negative-result route the original framing left open is closed: the corners are real.

## The two programs, and what each runner does (integrator, `4cb7e4f`)

```fun
-- probe1: the codomain continues *implicitly*
{ trait Size(A) = sig { size : A -> I64 }; impl Size(I64) = module { size = fn(x) { 8 } };
  g : [A : Type] -> [B : Size] -> B -> I64 = fn[A : Type, B : Type](b : B) { Size.size(b) };
  g(3) }
```

| runner | output |
|---|---|
| OCaml | `8` |
| port | **fails**: `missing implementation of `Size`` |

```fun
-- probe2b: the codomain is not a Pi at all
{ f : [A : Type] -> I64 = fn[T : Type] { match (T) { I64 => 1, _ => 0 } }; f[Bool] }
```

| runner | output |
|---|---|
| OCaml | `0` |
| port | **stuck**: returns `VNeutral` |

A third shape is *correct* today and must stay correct: an **explicit** Pi codomain
(`f : [A : Type] -> I64 -> I64 = fn[T : Type](x : T) { match (T) { I64 => x, _ => 0 } }; f(5)`)
→ `5` in both.

## The cause (the probing fork's, and it matches the evidence)

The prototype's check path (`lib/semantic/typecheck/elab_check.ml:24-30`) has **no instantiate
path at all** — an implicit lambda always binds its parameter rigidly, one parameter at a
time, which is what keeps binder identity aligned through hidden dictionaries and connects
the written `T` to the call's implicit argument. The port *needed* an instantiate path (that
is the landed ruling's fix: a name instantiates, and so must a lambda against a
parameter-independent expected type) — but its gate decides on **dependency alone** and
ignores the codomain's **residual shape**:

- codomain continues **implicitly** (`[B : Size] -> …`) → instantiating desynchronizes the
  parameter walk, the residual is re-inferred under a fresh binder, and the hidden dictionary
  evidence is lost → `missing implementation of `Size``;
- codomain is **not a Pi** (`I64`) → instantiating orphans the lambda's own parameter, and
  nothing can solve the inserted meta → `VNeutral`.

## The fix direction

Keep the dependency test, then add the residual shape: **instantiate only when the probed
codomain is an explicit Pi or still a meta; otherwise bind rigidly.** Dependency decides
*whether*, shape decides *how*.

A sketch of exactly that — the gate narrowed in `Elaborator.Implicits.cs`, plus the four probe
cases as `zz-probe*` — is on branch `pi-agent-5385293b-26d9-47e`, auto-squashed, **unverified
and not mergeable as it stands**: its author died on the provider's 5-hour cap one step before
running it. Re-derive from the direction above; reuse the sketch only as a cross-check.

## Tests

All three programs belong in the suite once the fix lands. `probe1` and `probe2b` cannot be
committed today (the port fails them — convention 8), and they are **not** divergences: the
*prototype* answers them correctly. The explicit-Pi shape is already covered by
`values/implicit-lambda-argument-inline`; add the two as ordinary cases with the fix, and
keep the whole gate honest by re-running the 16 cases that motivated it (the type-case
family, the value-level implicit `[n : I64]`, and `stage2.fun`'s `(==)`/`(!=)`).

---

## The original framing (kept: it is how the corners were found)

Raised by the fork that implemented
[an implicit lambda checked against a function type instantiates](port-generalise-under-check.md):
it had to choose a gate to land that fix, chose one that keeps every suite case green, and
reports the corners its choice does not cover rather than leaving them in a conversation.

## The situation

An implicit lambda in check position has two possible treatments, and the language needs
both:

- **bind its parameter rigidly** — check the lambda as the ∀ it claims to be. This is what
  a *dependent* expected type needs: `[T : Type] -> T -> T` (whose body type-cases `T`) and
  `[A : Eq] -> …` (whose hidden dictionaries are among the mentions) are not inferable, and
  a value-level implicit `[n : I64]` must have its runtime value flow through the lambda.
- **instantiate** — insert the parameter first and infer, exactly as a *name* does
  ([check-against-implicit-type-inserts-first](check-against-implicit-type-inserts-first.md)).
  This is what a call site's parameter-independent expected type needs
  (`[A : Type] -> I64 -> ?r`).

The port now decides by **dependency**: `BindsImplicitParameterItself`
(`dotnet/src/Fun.Compiler/Elaborator.Implicits.cs`) binds the parameter itself when the
expected domain is not `Type` (dictionary / effect-row / value parameter), or when the
codomain, probed at a fresh meta, `Unify.Mentions` the parameter. Otherwise it instantiates.

That gate was chosen because the naive alternative broke **16** suite cases — the whole
type-case family, the value-level implicit case, and `(==)`/`(!=)` in `stage2.fun` — which
is evidence that both treatments are genuinely needed, not that one is an accident.

## The corners it does not cover

Neither is exercised by any suite case, so neither is a defect today — they are the places
where the gate's rule and the "right" rule could part company:

1. **A parameter-independent dictionary.** `[A : Type] -> [Eq(I64)] -> …` whose body needs
   the evidence: nothing about the *expected* type mentions `A`, so the gate instantiates,
   and then the hidden dictionary may not be insertable where the body needs it. The
   prototype's behaviour on such a program has not been probed.
2. **A written parameter type mentioning the parameter inside a non-inferable body** — an
   explicit annotation like `fn[T : Type](x : T) { … }` checked against a
   parameter-independent expected, where the *body* (not the expected type) is what forces
   `T` to be rigid.

## What settling this looks like

Probe both runners on the two shapes above — the prototype is a map, not the spec, and this
area has produced three recorded causes that probing overturned, so a program beats an
inference. Then either:

- **the gate is right** (the differences are unobservable in both implementations) → close
  this ticket saying so, with the programs; the gate stays as it is; or
- **a shape differs** → fix it the way the ruling
  [an implicit lambda checked against a function type instantiates](port-generalise-under-check.md)
  was fixed, and add the case (ordinary if the prototype agrees, listed in
  `prototype-divergences.txt` if it does not).

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.Implicits.cs` — `BindsImplicitParameterItself`, the
  comment above it is the rationale
- [an implicit lambda checked against a function type instantiates](port-generalise-under-check.md)
  — the ruling, the corrected cause, and the hazard
- `dotnet/std/stage2.fun:23-24` — the `(==)`/`(!=)` definitions the naive version broke

## Resolution (2026-09-25) — closed

Fixed and merged (`fix/implicit-lambda-codomain-shape` @ `398f913`, fast-forwarded). Both
verified bugs now answer the prototype's values: `probe1` → `8`, `probe2b` → `0`, and the
explicit-Pi shape stays `7`.

**The gate as it now reads** — dependency decides *whether*, shape decides *how*:

```csharp
if (stx is not Syntax.Lam { Param.Explicitness: Explicitness.Implicit }) return false;
if (ctx.Force(pi.Domain) is not Value.VU) return true;              // dict / effect-row / value param
var probe = ctx.RawMeta();
var codomain = ctx.Force(Nbe.ApplyClosure(ctx.Metas, pi.Codomain, probe));
if (Unify.Mentions(ctx.Metas, ((Value.VMeta)probe).Id, codomain)) return true;   // dependent
return codomain is not (Value.VPi { Explicitness: Explicitness.Explicit } or Value.VMeta);
```

It agrees with the sketch on `pi-agent-5385293b-26d9-47e` verbatim — re-derived from the
direction in this ticket rather than merged from it. **That sketch branch is superseded and
has been deleted**, along with the four `zz-probe*` cases it carried; the two probes are now
real shared cases.

- **Tests:** C# conformance **736 → 738, 0 failed**; xUnit 182/182; `dune test` and
  `dune test test/conformance` green — 738 cases, 0 failed, **27** divergences, i.e. the two
  new cases are ordinary (the prototype answers `8` and `0` correctly). New cases:
  `values/implicit-lambda-codomain-continues-implicitly`, `values/implicit-lambda-codomain-not-pi`.
- **The 16 cases that motivated the gate are all green**, by the full runs: the type-case
  family (`core-072..084`, `elab-041`), `values/type-case-refines-variable`, the value-level
  implicit family, and `stage2.fun`'s `(==)`/`(!=)`. The domain-not-`Type` branch still binds
  those rigidly, so the narrowing did not reach them — which was the thing to check.
- Verified by the integrator after merging, not taken on the fork's word.
