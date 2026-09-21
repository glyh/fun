---
title: "Port: finish E11 — nominal identity (captures, sealing, run-time stamps)"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: E11 is finished in the port. One private stamp slot per module, type-case compares declaration + captures + stamp, sealed heads resolve through the sealing context. values/elab-062, values/core-067 and values/elab-067 pass; C# conformance 695 cases, 0 failed. One E11 shape remains unported (a parametric nominal in a generative module) — port-generative-former-nominal.
assignee:
blocked_by:
---

# Port: finish E11 in C# — nominal identity

Step 1 of [port-parity-plan](port-parity-plan.md). The spec is the closed
[nominal-identity-applicative-by-purity](nominal-identity-applicative-by-purity.md)
— read it whole, especially "Implemented (2026-09-16, branch generative-nominals)"
and "Finished (2026-09-16, branch e11-finish)", which describe the prototype's
design; and the four domain-model passes for vocabulary. Follow the
[porting conventions](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

This is the one place the port is **less correct** than the prototype rather than
merely incomplete. C# has captures and sealing in some form; it has **no run-time
stamp**, and at least one case shows the consequences.

## Failure 1 — `values/elab-062` (expects `10`, gets `0`)

```fun
{ Set = fn(Elem : Type, cmp : Elem -> Elem -> Bool) { module {
              pub type T = Leaf | Node(T, Elem, T);
              pub lt = fn(x : Elem, y : Elem) : Bool { cmp(x, y) } } };
            less = fn(a : I64, b : I64) { a < b }; greater = fn(a : I64, b : I64) { a > b };
            a = Set(I64, less); b = Set(I64, less); c = Set(I64, greater);
            f = fn(t : Type) { match (t) { a.T => 1, _ => 0 } };
            f(b.T) * 10 + f(c.T) }
```

`Set` is pure, so `a.T` and `b.T` are the same type by captures
(**applicative**) and `c.T` differs. Expected `10` = `f(b.T)=1, f(c.T)=0`; C#
answers `0`, so it matches neither. **Diagnose before fixing**: two candidate
causes, both live, and the fix differs.

- the nominal instances do not compare equal by captures at a type-case head, or
- the type-case head `a.T` is evaluated by a nested `Eval` when matched
  (`dotnet/src/Fun.Compiler/Nbe.Patterns.cs:88`, a recorded stopgap) and the
  nested evaluation mints or loses an instance.

## Failure 2 — `values/core-067` (expects `2`)

An effect handler over `State(I64)`. C# reports a generative type escaping its
binder where the prototype accepts. Same family: absent stamps make the sealing
and escape checks fire where they should not. Diagnose which check fires and why
before changing either.

## The refusal — type-case on a generative nominal

`dotnet/src/Fun.Compiler/Elaborator.Patterns.cs:198-200`:

```
// Two evaluations of a generative module differ only by their run-time stamp.
throw new NotImplementedException("not ported yet: type-case on a generative nominal (run-time module stamps)");
```

## What to build

Per the prototype's design (see the spec ticket for the argument):

1. **One stamp slot per module**, its first private slot — `()` at check time and
   for a pure module, `ref(())` at run time when the module's evaluation performs
   something. Every nominal the module declares captures that one slot, so all its
   constructors read the same stamp. The three `ponytail:` notes that record its
   absence: `Elaborator.cs:368` (module stamp slot), `Elaborator.Enum.cs:48`
   (declaration identity), `Elaborator.Generative.cs:12` (the run-time stamp).
   Both sides read `BindingTerm.Slots()` (convention 4 / I2), so adding the slot
   moves no index by hand — verify that claim rather than trusting it.
2. **Type-case compares nominal instances by declaration, captures, and stamp.**
   A match head is read in the match's scope (`CPatNominalHead` in the prototype);
   a sealed head (`st1.Symbol`) names its declaration through the sealing context.
   Then the refusal above goes, along with `Nbe.Patterns.cs:88`'s nested `Eval` if
   it is what makes the comparison wrong.
3. Keep the generative set as the prototype has it: `Core.generative_nominals` =
   the nominal ids declared while a performing module elaborated; sealing rewrites
   exactly those.

Prototype reference (supporting material, not the spec): `seal_generative` and the
escape checks in `lib/semantic/typecheck/elab_effects.ml:280-300`,
`Core.generative_nominals` (`lib/core_kernel/core.ml`), the stamp in
`lib/semantic/typecheck/elab_infer.ml`, `CPatNominalHead` in
`lib/semantic/typecheck/elab_patterns.ml` and `lib/semantic/match/core_match_compile.ml`,
readback in `lib/backend/interp/nbe_quote.ml`.

## Tests

- The two existing cases must pass (`values/elab-062`, `values/core-067`).
- **Add the generative half to the shared suite** — it has no case today, which is
  why this gap was invisible. From the spec's use cases, the sealed `SymbolTable`
  program (`st1.Symbol` distinct from `st2.Symbol`; `g(st1.intern("x"))` ok;
  `g(st2.intern("x"))` and `st2.name(st1.intern("x"))` rejected) and a type-case
  that separates two evaluations of a generative module. Run each through **both**
  runners first with a temporary file: if the prototype fails one, that is a
  prototype defect (convention 5 — reproduce, ticket it, list it in
  `test/conformance/prototype-divergences.txt`) or the case is wrong, not something
  to paper over. Never commit a case the port cannot pass (convention 8).
- xUnit is for internals: the kernel's stamp slot, `BindingTerm.Slots()` width
  after the new slot, and that a pure module's stamp is `()`.

## Report

Do not edit this ticket, `fun-design-map.md` or `docs/STATUS.md`; report and the
integrator records. Report: the diagnosis of each failure before the fix, branch,
commits, the conformance and xUnit counts, files touched, and any question you
stopped on with a concrete example.

## Resolution (2026-09-20) — C# conformance 695 cases, 0 failed

Merged from branch `port/nominal-identity` (E11 commits `74a4745`, `0f94076`,
`aaf3b40`, `3f549c7`, `752bbab`, `faf4e79`, `597d1a6`, plus the merge of `main`
`f4b3823`). **690/13 → 695/0**, xUnit 168 → **172**, `dotnet build` clean, `dune test`
and `dune test test/conformance` green (695 cases, 0 failed, 20 divergences).

### The diagnoses — and the first one was not the guess the ticket made

**`values/elab-062` is not the nested-`Eval` stopgap.** Instrumenting
`MatchesNominalHead` showed the head and the scrutinee carrying the *same* declaration
and two captures, while `SameInstance` returned false: the captures include the closure
`less`, and C#'s structural comparison had no closure case, so `a.T` did not compare
equal to **itself**. The prototype's `runtime_value_equal` short-circuits on physical
identity (a cell by identity) — ported that, and `elab-062` answers `10`. The
`Nbe.Patterns.cs:88` nested `Eval` was innocent and stays; its note moved with the code
to the new `Nbe.Generative.cs` with the reason restated as a *shared choice, not a
defect*. (Ticket's candidate (a)/(b) were both wrong; the lesson is the one the ticket
asked for — diagnose before fixing.)

**`values/core-067`.** `InferLet` ran `CheckSealedStays` for *every* performing let, so
a body whose type names an entry at or above the binder tripped it even though no
nominal was generative. The prototype checks only when the value's type mentions a
generative nominal (`mentions_generative`); gated on the sealing report being non-empty.

### Built

One private **stamp slot per module** (`()` at check time and for a pure module,
`ref(())` at run time when the module's evaluation performs); the slot is contributed
through `BindingTerm.Slots()`, read by both sides, so no index moved by hand (verified
by an xUnit slot-width test). **Type-case compares nominal instances by declaration +
captures + stamp**, and a **sealed head resolves through the sealing context**, which
retired the `Elaborator.Patterns.cs:198-200` refusal and fixed `elab-067` (the
`SymbolTable` type-case) as a side effect. New `Nbe.Generative.cs` (own partial file,
convention 7); `Elaborator.{cs,Generative,Enum,RecTypes,Patterns}.cs`,
`Nbe.Patterns.cs`, `NominalTests.cs`.

### Cases added (the generative half had no coverage — that is why the gap was invisible)

`values/nominal-generative-symbol-table-shares-own` (1),
`-rejects-other`, `-rejects-name-across` (error), `nominal-generative-type-case-separates`
(10) — all four pass in **both** runners. Note recorded by the fork: the family already
existed as `elab-063`…`elab-069` and only the type-case (`elab-067`) was failing, so
these add coverage rather than being its only source.

### Open / not done

- **A parametric nominal (a type former) in a generative module is still unported** —
  `not ported yet: sealing a generative nominal that is not bound as a module member`.
  Verified real gap (prototype `1`, port throws); it needs the prototype's separate
  `NomRef.params`, since C# `Term.Nominal` has captures but no param list and an unused
  parameter is not captured. → [port-generative-former-nominal](port-generative-former-nominal.md)
- **Type-case branch refinement to a sealed projection is refused by *both* runners**
  ("rigid vs neutral"). Parity, so no case was added, but it is a real limitation of
  the rule as it stands, worth its own ticket if a use appears.
