---
title: A closure capture gives two answers — conversion says equal, type-case says different
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# A closure capture gives two answers

Found by the [identity audit](port-identity-survives-reevaluation.md) fork (2026-09-27) as the one
place the ruled property did not hold, and **confirmed by the integrator by re-running both
programs** on `ae7ff62`. Every other site that audit probed came back innocent; this is the residue.

## The two programs

```fun
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
  mkset = fn() { Set(I64, fn(x : I64, y : I64) { x < y }) };
  a = mkset(); b = mkset();
  g = fn(x : a.T) { 1 };
  g(b.Leaf) }
```

```fun
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
  `src/Fun.Compiler/Unify.cs:48` applies the left value and the closure's body to the same fresh
  variable, so the two closure objects are compared extensionally.
- **A type-case says different.** Matching a nominal head goes through `SameInstance`
  (`src/Fun.Compiler/Nbe.Generative.cs:52-76`), which has arms for `VNominal`, `VRef`, `VEffect`,
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

## Options

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

## Reading

- `src/Fun.Compiler/Nbe.Generative.cs:52-76` — `SameInstance` and its `default` arm
- `src/Fun.Compiler/Unify.cs:48` — the λ arm that eta-applies
- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) — the
  decision this generalises, and the model's `a.union(x_from_a, y_from_b)` instance
- [identity must survive the pipeline's re-evaluation](port-identity-survives-reevaluation.md) — the
  audit that found it, and the per-site innocences
- the auditor's probes were scratch (`/tmp/identity-audit/p4.fun`, `p5.fun`, `p6.fun`); the two
  programs above are their durable form
