---
title: A closure capture gives two answers — conversion says equal, type-case says different
parent: ../quill-design-map.md
labels:
  - wayfinder:grilling
status: closed
closed_date: 2026-09-27
resolution: Closed 2026-09-27 - implemented by a fork (Oracle, commits 7b8d0b5 arm and 0f3d156 cases) and verified by the integrator. The type-case now answers 1 for the ruled pair while both negative controls stay 0. Conformance 783 cases 0 failed, xUnit 185/185. The arm compares the eta-applied results with Nbe.Convertible rather than recursing SameInstance, which cannot work (the applied results are stuck neutrals and SameInstance has no neutral arm); the residual that difference could leave was probed and is not reachable. Details below.
assignee:
blocked_by:
---

# A closure capture gives two answers

> ## Resolution (user, 2026-09-27): the type-case is the wrong one — closures compare as conversion does
>
> `a.T` and `b.T` are the **same type**. `SameInstance` gains the λ arm `Unify` already has — apply
> both closures to one fresh variable and compare the results — so both paths answer "one type".
> `Unify.cs:48` is the shape to match, including which side it fires on.
>
> | program | before | after |
> | --- | --- | --- |
> | `g = fn(x : a.T) { 1 }; g(b.Leaf)` | `1` | `1` (unchanged) |
> | `f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } }; f(b.T)` | `0` | **`1`** |
>
> **The negative control the arm must not break**: two closures whose bodies differ, or whose
> captured cells differ, are still **different** types. A rule that makes every pair of closures
> equal is not this ruling — it is a worse bug than the one being fixed.
>
> `SameInstance`'s doc comment — *"a capture need not have a structural reading"* — is the sentence
> this ruling retires; it becomes "a capture is compared the way conversion compares it".
>
> The cases land with the implementation; this ticket closes when they are green.

Found by the [identity audit](port-identity-survives-reevaluation.md) fork (2026-09-27) as the one
place the ruled property did not hold, and **confirmed by the integrator by re-running both
programs** on `ae7ff62`. Every other site that audit probed came back innocent; this is the residue.

## The two programs

```quill
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  mkset = fn() { Set(I64, fn(x : I64, y : I64) { x < y }) };
  a = mkset(); b = mkset();
  g = fn(x : a.T) { 1 };
  g(b.Leaf) }
```

```quill
{ /* the same prelude as above */
  mkset = fn() { Set(I64, fn(x : I64, y : I64) { x < y }) };
  a = mkset(); b = mkset();
  f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } };
  f(b.T) }
```

| program | what it asks | measured |
| --- | --- | --- |
| the first | is `b.Leaf` acceptable where `a.T` is expected (conversion) | **yes** — `VALUE 1` |
| the second | is `b.T` the same type as `a.T` (type-case) | **no** — `VALUE 0` |

Same pair of types, two answers, one command apart. The second one is not a checking quirk that
the first hides: the two paths are asked about the same two types in the same program.

## Why they disagree

`Set`'s capture is the `cmp` argument, and here that argument is a λ *term*: `mkset()` evaluates it
twice, so `a` and `b` capture two distinct closure objects.

- **Conversion says equal.** `Unify` eta-applies a closure — `case (_, Value.VLam b)` at
  `src/Quill.Compiler/Unify.cs:48` applies the left value and the closure's body to the same fresh
  variable, so the two closure objects are compared extensionally.
- **A type-case says different.** Matching a nominal head goes through `SameInstance`
  (`src/Quill.Compiler/Nbe.Generative.cs:52-76`), which has arms for `VNominal`, `VRef`, `VEffect`,
  `VAtom`, `VAtomTy`, `VU`, `VProd` and `VProdTy` — and no λ arm at all, so a closure reaches
  `default: return false` and is equal only by `ReferenceEquals`, the function's first line.

## Why it may be deliberate

`SameInstance`'s doc comment records the choice ("a capture need not have a structural reading"),
and the deleted prototype's own comparison ended in `| _ -> false` (`lib/nbe/nbe.ml:652`), so this
is **inherited parity, not a port regression**. The audit read that from `git`, not by running it —
the prototype is gone.

## The ruling this touches

Ruled by the user, 2026-09-25 (see [identity survives re-evaluation](port-identity-survives-reevaluation.md)):

> A type's identity is a pure function of its declaration, its own free variables and its module's
> stamp. Re-evaluating the same declaration with the same values **must** give the same type — so a
> recalculation that changes the answer is the bug, not a reason to design identity around it.

Whether "the same values" covers two eta-equal closures is the question. Read as **yes**, the
type-case answer is the bug and `SameInstance` needs a λ arm. Read as **no** — a closure is an
opaque value identified by its object — then conversion's eta rule is what over-equalizes in a
capture position, and the *typing* answer is the bug.

## Options (all three answered 2026-09-27)

- **A. Give `SameInstance` a λ arm, comparing as conversion does.** **Taken.** One rule for both
  paths.
- **B. Keep closures opaque and stop eta-applying a capture.** Not taken.
- **C. Rule it a documented limit.** Not taken.

The cost A pays, on the record: identity now reads λ bodies, so the doc comment's "a capture need
not have a structural reading" is retired, and a comparison that recurses into bodies shares the
termination question `Unify` already answers (and `Unify` answers it under the budget — the arm
should have the same shape rather than a new mechanism).

- **A. Give `SameInstance` a λ arm, comparing as conversion does.** One rule for both paths; the
  cost is deciding how far the comparison goes (bodies as written, or after eta-expansion), and it
  makes identity depend on a structural reading the doc comment currently says a capture need not
  have.
- **B. Keep closures opaque and stop eta-applying a capture.** Leave `SameInstance` alone and make
  conversion not eta-expand in a capture position. Smaller surface, but eta is what the
  applicativity model leans on for `Set(I64, cmp).T` elsewhere.
- **C. Rule it a documented limit.** State that a capture which is a λ term is identified by object,
  and record both answers in the ticket. **A conformance case cannot encode this option**: two cases
  asserting `1` and `0` for one pair of types would enshrine the inconsistency, and the suite's rule
  is that `.expect` states the model's behaviour, not the implementation's.

## Closed 2026-09-27 — the type-case now answers `1`

Implemented by a fork (Oracle, running on deepseek-flash because the GLM provider 429'd mid-session;
`7b8d0b5` the arm, `0f3d156` the cases) and verified by the integrator: **conformance 783 cases,
0 failed**, xUnit **185/185**.

| program | before | after |
| --- | --- | --- |
| `g(b.Leaf)` — conversion | `1` | `1` |
| `f(b.T)` — the type-case, the ruled one | `0` | **`1`** |
| bodies differ (`x <= y` vs `x < y`) | `0` | `0` |
| same body, differing captures (`n = 0` vs `n = 1`) | — | `0` |

Cases: `values/core-320` (conversion), `core-321` (the ruling), `core-322` and `core-323` (the two
negative controls). `SameInstance` now threads the context `width` so it can mint the fresh
variable, and its doc comment was retired as the ruling asked.

**The arm is not literally what this ticket sketched, and the difference is worth knowing.** The
sketch was "apply both closures to one fresh variable and compare the results". Comparing them by
recursing `SameInstance` **does not work**: for a stuck body the applied results are **neutrals**,
and `SameInstance` has no neutral arm, so it answers `false` and the fix would never fire. The arm
therefore compares the eta-applied results with `Nbe.Convertible` (`Nbe.Rec.cs:102`, readback
equality — the comparison effect instances already use), which is also closer to the ruling's own
wording: *compared the way conversion compares it*.

Two consequences of that choice, both measured rather than assumed:

- **Its one visible weakness is not reachable from a program.** `Convertible` is documented as able
to say "not convertible" where full conversion would not (no eta, no unfolding), so I probed the
shape where that could bite — two capture lambdas whose bodies differ only by an unfoldable
definition, `x <= y` against `le(x, y)`. Type-case and conversion **both answer `1`**: application
*evaluates* the body, so `le(x, y)` reduces to the same stuck primitive before readback sees it.
- **The neutral-capture case was already fine without the arm.** A capture that is a *variable*
  (`f(Set(I64, cmp).T)` inside `fn(cmp) { … }`) answers `1` on the merged tree and answered `1`
  before it, by physical sharing — which is why the missing neutral arm has never bitten.

Two limits the fork could not clear, recorded as it reported them: no case exists where an eta
result is a `VRef` (`Convertible` throws on those, but a nominal's capture cannot have a ref
result); and the literal "λ capturing a ref created per call" control **cannot be written at all** —
a nominal's capture cannot mention a ref heap, so no typeable program has one, and `core-323` (same
body, different captured values) is the nearest typeable form. Over-application was checked by
replacing the arm with `return true`: both controls then answer `1`, so they genuinely catch it.

## Reading

- `src/Quill.Compiler/Nbe.Generative.cs:52-76` — `SameInstance` and its `default` arm
- `src/Quill.Compiler/Unify.cs:48` — the λ arm that eta-applies
- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) — the
  decision this generalises, and the model's `a.union(x_from_a, y_from_b)` instance
- [identity must survive the pipeline's re-evaluation](port-identity-survives-reevaluation.md) — the
  audit that found it, and the per-site innocences
- the auditor's probes were scratch (`/tmp/identity-audit/p4.qll`, `p5.qll`, `p6.qll`); the two
  programs above are their durable form
