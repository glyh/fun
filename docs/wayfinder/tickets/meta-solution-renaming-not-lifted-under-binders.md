---
title: Solving a meta applied to a spine fails on a dependent right-hand side
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-20
resolution: Pattern unification with a partial renaming lifted under every binder; fixed in the C# port only.
assignee:
blocked_by:
---

# Solving a meta applied to a spine fails on a dependent right-hand side

Found while porting unification to C# (2026-09-16). **Fixed in the C# port only**
(`dotnet/src/Fun.Compiler/Unify.cs`); the OCaml prototype keeps the defect.

## Defect

`Unify.rename` (`lib/semantic/typecheck/unify.ml`) builds the solution of
`?M[x0 … xn] = rhs` by reading `rhs` back under a renaming from the spine
variables' *context* levels to the solution's lambda positions. Under a `VLam` or
`VPi` in `rhs` it introduces the binder as `VRigid { lvl = d }` with `d` counted
in the *solution's* numbering (`d` starts at the spine length), then looks every
rigid variable up in the same renaming - which is keyed by context levels and is
never lifted to cover the new binder. So an occurrence of a binder inside `rhs`:

- raises `VarNotInSpine d` when no spine variable sits at context level `d`, or
- is renamed to the wrong spine variable when one does.

A non-dependent right-hand side never mentions its own binders, which is why
ordinary code does not hit it.

## Reproduction

```
{ id = fn[A : Type](a : A) { a };
  f = fn(x : I64) { id(fn(T : Type, t : T) { t }) };
  1 }
```

fails with `UnifyError(VarNotInSpine(1))`: `A` is an inserted meta whose spine is
`[x]`, and its solution `(T : Type) -> (t : T) -> T` mentions `T` under a binder.
The same program with `fn(t : I64) { t }` in place of the dependent lambda checks.

## Conformance

`elaborate/meta-solution-dependent-spine` (`ok`) is the reproduction above,
listed in `test/conformance/prototype-divergences.txt`. The port's unifier test
(`dotnet/test/Fun.Tests/UnifyTests.cs`) pins the fix until the port reads
implicit parameter lists, which the case needs.

## Resolution in the port

Pattern unification with a partial renaming (domain/codomain sizes plus a
level map), **lifted under every binder**: entering a binder maps the codomain's
next level to the domain's next level. A rigid variable found in neither is an
escape, reported as a unification failure; the meta itself in `rhs` fails the
occurs check.

## Resolution (2026-09-20)

Closed after re-running both runners on `main @ d58af64`.
`dune test --root . test/conformance` reports `690 cases, 0 failed, 19 known
prototype divergences`, so `elaborate/meta-solution-dependent-spine` fails in the
prototype as listed; `cd dotnet && dotnet run --project test/Fun.Conformance
--no-build` passes it (not among the 13 unrelated residue failures), so the port
is correct.
