---
title: "An unused type parameter is an error at its declaration"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# An unused type parameter is an error at its declaration

Split out of [the generative former's identity residue](port-generative-former-identity-residue.md)
on 2026-09-24 because it waited on a ruling, so the other gap there could be worked.
**Ruled by the user 2026-09-25**, answering the two options this ticket put up (mirror the
prototype's phantom parameter, or keep the port's refusal) with a third that neither was:
**reject the declaration**.

## The ruling

> A type former's parameter that does not occur in the former's body is a **language error
> at its declaration** — in both implementations.
>
> Scope, ruled the same day in two follow-ups: it covers **every type former**, enum and record
> alike, implicit parameters included; and a lambda **is** a type former iff its result is a
> type, which is what makes `Pair = fn[A, B] { struct { fst : A } }` checkable without naming
> the surface form.

The asymmetry that made this a gap:

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
| --- | --- |
| OCaml | `1` — the parameter is **phantom**: `b1.Box(I64)` and `b1.Box(Char)` are the same type |
| port | `not ported yet: sealing a generative former with an unused type parameter` (`Elaborator.Generative.cs`) |

and the same shape without a generative module (`{ M = module { pub type Box(A) = Bx;
pub mk = fn() { Bx } }; g = fn(x : M.Box(I64)) { 1 }; g(M.mk()) }`) is accepted by both →
`1`. So the port's refusal was a gap, and the prototype's acceptance was a defect: nothing
observable distinguishes `Box(I64)` from `Box(Char)`, and a declaration whose parameter
means nothing should say so rather than silently ignore it.

**Rejected: the phantom reading.** It was the recommendation on this ticket and the model's
own analogy (footgun 6 of
[nominal identity](nominal-identity-applicative-by-purity.md): *"identity over all captures
makes unused variables split types, hence the declaration's own free variables only"*) points
the same way. The ruling takes the other route deliberately: rather than define what a
phantom parameter *means*, refuse to write one. That is the stricter reading and it is now
the decided one — do not re-litigate it in the fork.

## Scope — ruled (user, 2026-09-25): all type formers

**What counts as a type former — ruled (user, 2026-09-25): a lambda whose result is a type.**
A lambda is checked iff its result is in the `Type` spine (`Type`, or `B -> … -> Type`), so
`fn(A : Type) { I64 }` is refused and so is a nested chain
(`fn(A : Type) { fn(B : Type) { I64 } }` — both lambdas are formers). `fn(A : Type) { 1 }`
stays legal: its result is a value, so it is an ordinary function of a type argument and no
identity is at stake. The offered alternative — "any `: Type` parameter that is unused,
wherever it appears" — was declined for exactly that reason.

That single criterion covers both declared shapes (`pub type Box(A) = Bx` is a rec-bound
lambda over a nominal; `Pair = fn[A, B] { struct { fst : A } }` is a let-bound lambda over a
struct type) without special-casing the surface form.

- **Every type former**, not only the generative path: the check is on the *declaration*,
  before any module or sealing is involved. The ruling's own table shows the non-generative
  shape is accepted today, so that is exactly where the new case bites.
- **Enum formers AND record (`struct`) formers.** `Pair = fn[A, B] { struct { fst : A } }` has
  an unused `B` and is the same defect; the rule is uniform across the two kinds of former the
  language has, per the project's "one construct, many roles" bias.
- **Implicit parameters count** (`fn[h : Type] { … }`), and the error names the parameter.
- **A parameter is "used" iff it occurs in the former's body** — the desugared body, i.e.
  after the `type` macro has run (`rec Box = fn(A : Type) { … }`), not the surface spelling.
  The check reads the elaborated former, so no macro can hide an unused parameter.
- **Checked in the prelude too.** Checked as of the ruling: `dotnet/std/stage{1,2}.fun` declares
  no former with an unused parameter (`Option(A)` and `List(A)` both use `A`; the only `fn[…]`
  binders in stage 2 are the `(==)`/`(!=)` functions), so the prelude must stay green. If it
  does not, that is a finding to report, not a reason to weaken the rule.

## What to implement, once a fork slot is free

`Elaborator.Generative.cs` is not free: [the identity residue](port-generative-former-identity-residue.md)
is in it right now, and this ticket's guard lives in the same file. **Queued behind it.**

1. **Raise the error where the declaration is elaborated**, not where it is sealed: the check
   is a property of the former, so it does not need an environment or a stamp. A `FunException`
   naming the parameter (convention 2: a language error, never `not ported yet`).
   The criterion is about a lambda and its *result type*, so the site that must certainly see it
   is wherever a `rec` type binding's former is peeled (the port's `Elaborator.RecTypes.cs`,
   where the E11 capture fix lives; beside the prototype's `elab_type_group`,
   `lib/semantic/typecheck/elab_infer.ml:323`). Whether that and the `let`-bound record former
   are one check at the lambda site or two is the fork's call. "Used" is an occurrence test on
   the elaborated body — `term_mentions_var` is the precedent, and the closed
   `term-mentions-var-ignores-inserted-metas` ticket records the trap it had.
2. **Delete the sealing guard** (`NumParams > Captures.Length` → `NotImplementedException`) and
   the `NominalHeadOf` arity bookkeeping it needed: once step 1 is in, that path is unreachable
   (the audit's verdict 3 — delete the throw, do not convert it).
3. **Do the same in the OCaml prototype.** The ruling says both implementations, so this case
   is an ordinary shared one, *not* a divergence entry: `elaborate/<name>` with `.expect error`,
   green in both runners. This is the rare OCaml edit — it is a language rule, not port work —
   and `dune test` must stay green.
4. **xUnit** (convention 6 — a source-to-result test is a conformance case, but *which* error,
   at *which* site, is internals): assert the error is the declaration-site one naming the
   parameter, so a future change that moves it back to the sealing path is caught.

### Tests

- `elaborate/unused-type-parameter` — the generative program above, `.expect error` (ordinary:
  the prototype errors too, once step 3 lands).
- a companion with no generative module at all, the same `.expect`, so the rule is shown to be
  about the declaration rather than about sealing.
- a positive control, `A` used in the body, still `ok`/`1` — `Option`/`List` in the prelude are
  the standing control, but one in the same area keeps the pair adjacent.
- if the record-former case: one record former with an unused parameter, `.expect error`.
- one xUnit test naming the error (step 4).

## Reading

- [the generative former's identity residue](port-generative-former-identity-residue.md) —
  section 2 is this ticket's evidence, and its section 1 is the file-sharing blocker
- [the parametric nominal in a generative module](port-generative-former-nominal.md) —
  `GenerativeNominal(Label, NumParams)`, `NominalHeadOf`, `Seal`
- `dotnet/src/Fun.Compiler/Elaborator.Generative.cs` — the `NumParams > Captures.Length` guard
  that replaced a `Skip(-1)` crash
- [nominal identity is applicative by purity](nominal-identity-applicative-by-purity.md) —
  footgun 6, the argument this ruling declines to follow
