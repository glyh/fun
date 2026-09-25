---
title: "Port: a former's captures must not include the enclosing function's bindings"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a former's captures must not include the enclosing function's bindings

**Reopened 2026-09-25: two merged pieces of work are in tension here, and the case is held back
until the user rules.** The file keeps its old name; the title is what the issue actually is.

## What happened

The claim as first written — "the port over-captures enclosing names" — was the **wrong way
round**. A probe found a program and showed the *prototype* over-captures while the port did not,
so the case was added with `expect` `1` and listed as a prototype divergence.

Then the other fork landed. The integrator's A/B, on the merged tree, reverting only
`Elaborator.RecTypes.cs` to its pre-`former-stamp` state:

| tree | `values/rec-enum-former-ignores-outer-name` | `values/nominal-generative-former-type-case-separates` |
|---|---|---|
| with `former-stamp`'s `RecTypes.cs` | **fails** — `type mismatch: cannot unify VAtom with VAtom` | passes (`10`) |
| with `RecTypes.cs` reverted | passes (`1`) | **fails** — `expected 10, got 11` |

One file, one 15-line change, and the two cases want opposite outcomes.
[The generative former's identity residue](port-generative-former-identity-residue.md)'s fix binds
a former's parameters over the **declaration site's** `Enclosing`/`ScopeCaptures` — which is what
supplies the module's **stamp** for generative identity — and that same change also admits the
enclosing *function's* scope, and so its value binding, into the capture set.

## The program

```fun
{ F = fn(n : I64) { y = n; rec T = fn(A : Type) { enum { X(A) } }; T };
  a = F(1); b = F(2); take = fn(z : a(I64)) { 1 }; take(b(I64).X(3)) }
```

| runner | output |
|---|---|
| OCaml | `UnifyError(NominalMismatch(T, T))` — `a` and `b` are distinct |
| port, with `former-stamp` | a type mismatch — also distinct |
| port, before `former-stamp` | `1` — the same type |

The `rec` keyword alone flips the prototype: drop it and **both** runners agree `n` is not
captured, which is what made the probe call the prototype the over-capturer.

## Already decided by the model — the question was mine, not the user's

The user's answer was to read the docs first, and the docs settle it:

- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) (closed,
  2026-09-16), footgun **6**: *"Identity over **all** captures makes unused variables split
  types. Hence the declaration's own free variables only."*
- the same decision: *"A nominal's identity is its declaration plus the values of its own free
  variables, compared by conversion"*, and
- its stamp rule: *"Every module has a private stamp slot its nominals capture: `()` at check
  time and for a pure module, a fresh cell at run time for a module whose evaluation performs
  something — so type-case separates evaluations."*

So a former's captures are **its own free variables, plus the enclosing modules' stamps** — and
nothing else. Measured against the variants the integrator ran in both runners:

| program | rule says | runners today |
|---|---|---|
| `F = fn(n) { y = n; T = fn(A) { enum { X(A) } }; T }` (no `rec`) | no free var, no stamp → same type | `1` in both ✓ |
| `F = fn(A) { rec T = fn(B) { enum { X(A) } }; T }` (declaration names `A`) | `A` is free → distinct | error in both ✓ |
| `F = fn(n) { y = n; rec T = fn(A) { enum { X(A) } }; T }` (names nothing) | no free var, no stamp → **same** | error in both ✗ **the port over-captures** |
| `Mk = fn(u) { module { … pub type Box(A) = Bx(A) } }` (performing) | stamp → distinct | `10` in both ✓ |

So **the prototype over-captures** — `NominalMismatch` on a declaration that names nothing outside
itself — and `former-stamp`'s mechanism inherited that by taking the declaration site's
`ScopeCaptures` wholesale. The port must be narrowed to *free variables + stamps*, and the stamp
case must stay green while it is.

Note what the narrowing is **not**: the model's stamp rule is exactly why `ScopeCaptures` was the
natural place to look (the stamp *is* a scope), so the fix is not "stop using the declaration
site" but "take the stamp, not the enclosing function's bindings".

## The five-example contract (user-ruled 2026-09-25)

| # | program | ruled | today |
|---|---|---|---|
| 1 | `F = fn(n : I64) { y = n; rec T = fn(A : Type) { enum { X(A) } }; T }`, `a = F(1); b = F(2)`, `take(b(I64).X(3))` | **must typecheck** (`1`) | error in both — the bug |
| 2 | same, but the declaration names the outer parameter (`F = fn(A : Type) { rec T = fn(B : Type) { enum { X(A) } }; T }`) | **must not typecheck** | error in both ✓ |
| 3 | `Set = fn(T : Type) { module { pub type S = Leaf \| Node(T); pub leaf = Leaf } }` used as `a = Set(I64); b = Set(I64)`, `take(b.leaf)` | **must typecheck** (`1`) | `1` in both ✓ |
| 4 | the same `S` from `Set(Char)` against `Set(I64)`'s | **must not typecheck** | error in both ✓ |
| 5 | a *performing* module's two evaluations (`Mk(())` twice) | **distinct** (`10`) | `10` in both ✓ |

Example 3's answer is the model's own use case, not an inference — from
[nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) under
*"Applicative — sharing required"*:

```fun
a = Set(I64, compare_i64); b = Set(I64, compare_i64)
a.union(x_from_a, y_from_b)            -- must typecheck
```

with the reason given there: *“the checker re-evaluates `Set(I64, cmp).T` during conversion, so a
type minted per evaluation would not equal itself”* — i.e. a per-call nominal would not even be
equal to **itself**, which is why example 3 is mandatory rather than a convenience.

**Caveat (user, 2026-09-25): that reason is a symptom, not the design.** Identity must be a pure
function of the declaration, its free variables and its stamp, so *no* re-evaluation can change
the answer — and if a pipeline recalculation mints something new, the **recalculation** is the bug
to fix. That is [its own ticket](port-identity-survives-reevaluation.md), with the port's
nominal-head match as its first audit target.

So the target is exactly: identity = the declaration + its own free variables + the enclosing
module's stamp. Examples 2, 3, 4 and 5 are already right; **1 is the bug**, and 2/4 are the guards
that keep a fix from over-correcting in the other direction.

## Held back, per convention 8

The case pair and its divergence entry were **removed from the tree** while this is open —
"never commit a case the port cannot pass" — so `main` stays green (750 → 749 cases, 30 → 29
divergence entries). They return with whichever fix the ruling implies. Do not re-add them
before it: either outcome makes one of the two current behaviours wrong, and the case's `expect`
is what changes.

## Kept: the probe's unverified lead

The port's `Choice` (OpenChoice) lacks the prototype's base-names fallback — a possible
**under**-capture, the opposite direction. No reproducing program yet; recorded rather than
ticketed separately.

## Reading

- `dotnet/src/Fun.Compiler/Elaborator.RecTypes.cs` — `PredictCaptures`, `EnumCaptureLevels`,
  `CompletePending`; `Elaborator.Enum.cs`
- [the parametric nominal in a generative module](port-generative-former-nominal.md) and
  [the generative former's identity residue](port-generative-former-identity-residue.md) — what
  the stamp capture is for
- E11's rule: `docs/wayfinder/topics/nominal-identity-applicative-by-purity.md`
