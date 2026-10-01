---
title: The prelude spellings the ABI declaration leaves behind
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# The prelude spellings the ABI declaration leaves behind

Opened 2026-09-27 while closing
[declare the bootstrap↔compiler interface once](declare-bootstrap-compiler-interface-once.md).
That ticket's mechanism landed: `src/Fun.Kernel/PreludeAbi.cs` holds 160 names and
`Prelude.Load` verifies them eagerly, so no prelude name is spelled outside the declaration. This
ticket is the residue — the handful of decisions the declaration deliberately did not take, each of
which is a *ruling* rather than a measurement.

## 1. `Id` is both a declared type and a hole-kind keyword

The declaration carries `Id` as `Syntax.Id`. But the surface also spells `Id` as a **hole kind**
(`HoleKind.HoleId`, alongside `HoleExpr`, `HoleBlock`, `HoleDecl`, `HoleOneDecl`, `HolePattern`,
`HoleTokens`). Decide whether the type and the hole kind are one interface entry, two, or whether
the hole-kind spelling belongs somewhere this declaration does not reach. The other six hole kinds
arrive through the published builder (`hole_kind`, a `Leafs` probe), so `Id` is the odd one out.

## 2. `Block` (hole-kind only) stays spelled

`Block` is a hole-kind spelling only and maps to `Syntax.Expr`; it never reaches the declaration.
Decide whether it should, or whether it is a surface keyword with no prelude name to declare.

## 3. Recorded as deliberate, not gaps

So that a later sweep does not re-open them:

- `EffectRow` (`Elaborator.cs:152`, `Elaborator.PolyArrows.cs:21`) is a **compiler builtin**, a
  sibling of `Type` — and `Type` is not in the interface by explicit ruling, because it belongs to
  the elaborator rather than to `std`.
- `assoc` (`Enforest.Roles.cs:399`) matches the `order … : assoc(left)` **clause keyword**, not the
  `Syntax.assoc` builder.
- `Nil`/`Cons`/`Id`'s fields are resolved through probed accessors (`Reflection.ListNil`,
  `ListCons`, `IdFields`), so they are not spelled and need no entry.

## 4. Sequenced behind the `std` split — **done 2026-09-27**

The declaration had never been exercised against the restructured prelude when it landed. It has
been now: the [`std` split](restructure-std-into-bootstrap-and-library.md) merged as `16f9948`
(`bootstrap.fun`, `list.fun`, `lib.fun`, `type.fun`, `stage2.fun`), and the eager check was
demonstrated against the renamed unit by breaking one constant and rebuilding:

```
ELAB invariant (InvalidOperationException): PreludeAbi declares Syntax.Expr.RawVarX,
which the prelude std/bootstrap does not define
```

Restored, the prelude loads and the suite is 806 cases, 0 failed. So the declaration's purpose
survives the rename, and the check names both the member and the unit it looked in.

Two things the merge left, both one-liners — **the first was already fixed when this was
checked on 2026-10-01**:

- ~~**`PreludeAbi.cs:22`'s doc comment is stale.**~~ **Not stale any more** — the comment now reads
  `Prelude.Path`, `Prelude.BootstrapPath` and `Prelude.Binding`, and `grep -rn Stage1Path src/`
  returns nothing, so the rename's doc residue was cleaned up somewhere between this bullet being
  written and 2026-10-01. Recorded here so a later sweep does not go looking for it: the paragraph
  was older than its commit, which is the failure mode the map warns about in writing.
- **The unit paths stayed single-sourced**, as the design intended: `Prelude.Path`,
  `Prelude.BootstrapPath` and the `Order` list are one place each, and the error message quotes
  `BootstrapPath` rather than spelling it. `std/README.md` was written by the split and the
  declaration both; the split's merge kept both sides, and it now states the seam and the ABI table.

## Why this is not urgent

Everything here is a spelling or a placement question with no behaviour behind it — the interface
is declared, checked and green (187 xUnit, 798 conformance cases) as it stands. Take it when a
second spelling of `Id`, or a rename in the restructured `std`, would otherwise cost somebody an
afternoon.
