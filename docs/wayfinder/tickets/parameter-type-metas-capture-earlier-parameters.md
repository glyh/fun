---
title: A parameter's type meta captures the earlier binders, not only `self`
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# A parameter's type meta captures the earlier binders

Found by the [method-signature metas](method-signature-metas-capture-self.md) fork (2026-09-27) as
the same failure one layer out, and **measured by the integrator on both `main` and base
`a2c0044`** — the numbers below are identical on both, so this is pre-existing, not a regression
from that fix.

## The defect

A meta inserted while elaborating a parameter's type lists the binders already in scope in its
spine. At a call, every spine entry must be invertible — a *variable*. `self` is never one (the
receiver is a value), which is what the method ticket fixed by marking that entry `Defined`. An
**earlier parameter** is a variable only if the caller happens to pass one, and nothing about a
`Ref(I64)` parameter's hidden heap depends on the parameter before it — the dependency is a
by-product of where the meta is inserted, not of what the type needs.

| probe | result, `main` and base alike |
| --- | --- |
| `f = fn(a : I64, r : Ref(I64)) : I64 { a }; x = ref(40); f(0, x)` | `a meta's spine argument is not a variable` |
| same, with `n = 0` then `f(n, x)` | same failure |
| same, called from a λ with a real parameter: `g = fn(y : I64) { x = ref(40); f(y, x) }; g(0)` | **`VALUE 0`** |

So the difference is not literal-versus-variable at the source level: `n = 0` is let-bound and
unfolds to the literal at the call, while a λ parameter stays a variable and inverts. That is the
whole of the mechanism — and it is why the failure is easy to miss: the same function works or
fails depending on *how* its argument was produced.

## The same failure in three other shapes (all five probes measured)

| program | `main` | base `a2c0044` |
| --- | --- | --- |
| plain `fn` with a literal argument | fails | fails |
| plain `fn` with a let-bound argument | fails | fails |
| `Box[I64]{…}.get(x)` where `Box = fn[A : Type] { struct { … pub method get(r : Ref(I64)) … } }` | fails | fails |
| `f[I64](1, x)` where `f = fn[A : Type](a : A, r : Ref(I64)) …` | fails | fails |
| `g(b, x)` where `g = fn(o : Box[I64], r : Ref(I64)) …` and `b = Box[I64]{ v = 1 }` | fails | fails |

The last three share the story: the ambient `A` or the struct value is substituted at `Box[I64]` or
passed as a record, so the spine entry is not a variable.

## Adjacent, different mechanism, also pre-existing

A user type with an **unsupplied** implicit is refused in type position — `Ref(Pair)` where
`Pair = fn[A : Type, B : Type] { struct { … } }` gives `cannot unify VStruct with VU`, for a method
and a plain `fn` alike (measured on `main`; base not checked, because both shapes fail identically
and neither is touched by the method fix). `Ref(Pair[I64, Bool])` works. Whether an implicit should
be inserted inside `Ref(…)` is a separate question from this ticket's, and the two should not be
fixed together.

## Direction

The fix is the method ticket's, generalised: a parameter's written type should insert its metas so
they abstract over no *earlier parameter* either, and let unification discover any real dependency
instead of the insertion order dictating one. The method fix is the model —
`Context.WithoutSelfInMetas` marks an entry `Defined` for exactly this purpose — but it is keyed to
one entry, so the generalisation needs a decision: which earlier entries a signature meta should
skip (all written-type positions? only non-dependent ones?), and what that does to
`f : (a : T) -> (r : Ref(…a…)) -> …`, where the dependence is genuine and must survive.

**That decision is why this is a ticket and not a fork**: the fix has a shape to choose, and a
wrong choice would break dependent parameter types that work today. Probe a genuinely dependent
signature before choosing.

## Reading

- [a meta in a method's signature captures `self`](method-signature-metas-capture-self.md) — the fix
  this generalises, and the three findings it recorded
- `src/Fun.Compiler/Elaborator.Structs.cs` — `MethodType`/`Params`/`MethodBody` and
  `Context.WithoutSelfInMetas`
- `src/Fun.Compiler/Unify.cs:144` (`Invert`) — where a non-variable spine entry is refused
- `src/Fun.Compiler/Nbe.cs` — `InsertedMeta`, which reads the `EntryKinds` spine

## Recon (base `f177993`, 2026-09-27; measured, not fixed)

Scratch programs under `/tmp/param-meta-recon/`, run as
`dotnet test/Fun.Conformance/bin/Debug/net10.0/Fun.Conformance.dll --file <p>.fun` after
`dotnet build`. Baseline on this base: **conformance 865/0, xUnit 188/188**.

### 1. The ticket's five probes — all five still fail, unchanged

| program | result | message |
| --- | --- | --- |
| `f = fn(a : I64, r : Ref(I64)) : I64 { a }; x = ref(40); f(0, x)` (literal) | fail | `ELAB type mismatch: a meta's spine argument is not a variable` |
| same, `n = 0` then `f(n, x)` (let-bound) | fail | same message |
| same called from a λ: `g = fn(y : I64) { x = ref(40); f(y, x) }; g(0)` | **`VALUE 0`** | — |
| `Box[I64]{ v = 1 }.get(x)`, `Box = fn[A : Type] { struct { v : A; pub method get(r : Ref(I64)) : I64 { 3 } } }` | fail | spine message |
| `f[I64](1, x)`, `f = fn[A : Type](a : A, r : Ref(I64)) : A { a }` | fail | spine message |
| `g(b, x)`, `g = fn(o : Box[I64], r : Ref(I64)) : I64 { o.v }`, `b = Box[I64]{ v = 1 }` | fail | spine message |

The literal/let-bound/λ split and the message are byte-identical to the ticket's numbers.

### 2. A genuinely dependent signature

- **Works today:** `f = fn[A : Type](a : A, r : Ref(A)) : A { a }`,
  `g = fn[B : Type](y : B) { q = ref(y); f[B](y, q) }; g[I64](7)` → **`VALUE 7`**. The later
  parameter's element type `A` depends on the earlier type parameter, the meta's spine holds the
  variables `[B, y]`, and `Invert` succeeds. Same function with a monomorphic call
  `q = ref(7); f[I64](7, q)` → **spine failure** (`dep10`), so the dependence is abstract-generic
  only.
- **Value-indexed dependence:** `F = fn(n : I64) { if (n == 0) { I64 } else { Bool } }`,
  `f = fn(a : I64, r : Ref(F(a))) : I64 { a }`. The declaration elaborates; the call
  `x = ref(0); f(0, x)` → **spine failure** (`dep4`). It is the only well-typed call (a generic
  caller cannot build the `Ref(F(y))`), so the genuine value-indexed case is *unreachable today*.
- **Blocked (separate bug, not this ticket):** `Ref(Id[A])` with `Id = fn[A : Type] { struct { v : A } }`
  fails at **declaration** with `cannot unify VStruct with VU` (`d14`), while `Ref(Id[I64])` works
  (`d12`) and `Ref(Pair[I64, Bool])` works (`d11`). So a struct-former applied to a *bound type
  variable* inside a written type is a distinct pre-existing failure; it could not be used for the
  dependent probe.

**What must stay true:** marking an entry `Defined` edits only `EntryKinds`; `Environment` and
`Names` are untouched (`WithoutSelfInMetas` proves this). The element/result dependence of `dep4`
and `dep9` is an ordinary environment lookup, so it survives every shape. The only hidden meta in a
written type here is `Ref`'s implicit heap (`Ref : [h : Type] -> Type -> Type`) plus trait evidence;
no measured program needs that heap meta to abstract over an earlier *value* binder.

### 3. Candidate shapes, measured

The two shapes below were each implemented in the worktree, built, run over the probes and the
suite, then reverted. Sites changed, common to both: `Elaborator.cs` `InferLam` (the written-domain
elaboration and the Lam-against-Pi check) and `Elaborator.Structs.cs` `Params`, `MethodBody` and
`MethodType`'s `Result`.

| shape | change | probes fixed | probes / cases it leaves failing | suite |
| --- | --- | --- | --- | --- |
| **S0** status quo | — | none | p1 p2 p4 p5 p6 p8 p10 dep10 | 865/0, 188/188 |
| **S1** skip *every* bound entry | helper maps all `EntryKind`s to `Defined` | p1 p2 p4 p5 p6 p8 p10 dep4 dep10 | none measured | **865/0, 188/188** |
| **S2** skip only non-`Type` bound entries | helper keeps `Bound` iff the entry's type forces to `Value.VU` (the `FreshRowMeta` pattern, `Elaborator.Effects.cs:424`) | p1 p2 p6 p8 p10 dep4 | **p4, p5, dep10** (an ambient/instantiated `A : Type` still non-variable) | 865/0 |
| **S3** skip only non-dependent entries | thread the written type's mentioned levels into meta insertion; `Defined` unless the elaborated type mentions the entry | unmeasured | unmeasured | unmeasured |
| **S4** tolerate non-variable spine at solve | in `Unify.Solve`/`Invert`, partition the spine: variable entries abstracted, non-variable entries substituted (meta becomes a constant in them) | unmeasured | unmeasured | unmeasured |

Exact S1 result: p1 `VALUE 0`, p2 `VALUE 0`, p4 `VALUE 3`, p5 `VALUE 1`, p6 `VALUE 1`, dep4
`VALUE 0`, dep10 `VALUE 7`; p3 `VALUE 0`, p7 `VALUE 41`, p9 `VALUE 41`, dep9 `VALUE 7` all keep
working. Exact S2 result: p1/p2/p6/p10/dep4/dep9 pass; **p4 and p5 still fail with the spine
message**, dep10 too. S2's failures are the explicit-instantiation shapes: `A` is a `VU` binder,
stays `Bound`, and `f[I64]` / `Box[I64]` substitute a non-variable.

`S3` and `S4` are **unmeasured**; the descriptions are the change each would need, not a predicted
outcome. `S3` also has to be threaded to the `InsertImplicitArgs` site (`Elaborator.Implicits.cs:11`)
that actually inserts the meta, which is one call below the written-type site. `S4` changes
`Invert`'s contract and is a different mechanism (substitution, not spine masking).

### 5. Integrator's check of S1 (2026-09-27, base `001fd42`)

S1 was rebuilt from this section's description — the helper mapping every `EntryKind` to
`Defined` (`Elaborator.Structs.cs:415`, `WithoutSelfInMetas`), **plus** the two sites the
"sites changed" line names, which the helper alone does not reach: `InferLam`'s written-domain
elaboration (`Elaborator.cs:594`) and the Lam-against-`Pi` check (`Elaborator.cs:360`). All three
now route a written parameter type through that context.

Result — **the method half and the explicit-instantiation half reproduce exactly**:

| probe | this section's S1 row | my S1 |
| --- | --- | --- |
| p1 literal | `VALUE 0` | `VALUE 0` |
| p2 let-bound | `VALUE 0` | `VALUE 0` |
| p3 called from a λ (control) | `VALUE 0` | `VALUE 0` |
| p4 `Box[I64]{…}.get(x)` (method) | `VALUE 3` | `VALUE 3` |
| p5 `f[I64](1, x)` | `VALUE 1` | `VALUE 1` |
| p6 `g(b, x)` | `VALUE 1` | **`ELAB cannot unify VStruct with VU`** |

Suite: `conformance: 865 cases, 0 failed`, xUnit 188/188 — the same as the row claims.

**The p6 entry is wrong, in this table and in §1.** As written in §1, p6 fails with
`cannot unify VStruct with VU` on **base as well as under S1** — it never reaches the spine
machinery. Isolated: `g = fn(o : Box[I64]) : I64 { o.v }` alone fails the same way on base,
with no spine-shaped expression in sight. So p6 measures the *adjacent* mechanism of §"Adjacent,
different mechanism" — and extends it: that bug bites a struct former **with** its type argument
supplied (`Box[I64]`), not only an unsupplied implicit (`Ref(Pair)`). Whichever shape is chosen,
p6 stays failing until that separate bug is fixed, and no shape's row should count it.

Two smaller consequences: §1's p6 row should read `cannot unify VStruct with VU`, not the spine
message; and the "probes fixed" column for S1 should be read as p1 p2 p4 p5 p8 p10 dep4 dep10.

Also confirmed while checking: S1's helper change alone, without the two `InferLam` sites, fixes
**only** p4 — p1/p2/p5 keep the spine message. That is the §4 structural finding made concrete:
the plain-`fn` half of the fix is not reachable from `WithoutSelfInMetas`, because `Syntax.Lam` is
single-parameter and `InferLam` cannot tell an earlier parameter from an enclosing binder.

### 4. Structural finding for whichever shape is chosen

`Syntax.Lam` is **single-parameter** (`src/Fun.Kernel/Syntax.cs:44`); `fn(a, r)` is nested `Lam`s, so
`InferLam` cannot distinguish "earlier parameters of this same `fn`" from enclosing binders. The
method fix could key on `selfLevel` because `Params` walks a parameter *list*; the plain-`fn` site
has no such marker. Any "skip earlier parameters only" shape therefore needs either a new notion of
signature start or a signature-level traversal replacing `InferLam`, not a one-line change there.

