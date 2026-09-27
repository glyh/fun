---
title: Nbe's module re-evaluation drops Vars and Bounds
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# Nbe's module re-evaluation drops `Vars` and `Bounds`

Reported 2026-09-27 by the fork that implemented
[a generic impl's head variable carries no bound](generic-impl-head-var-has-no-bound.md),
as a pre-existing hazard it met while working. **Not independently reproduced by the
integrator** — recorded because an impl's bounds now travel as data
(`TraitEvidence.Bounds`, `ModuleEntry.Impl.Bounds`), so anything that rebuilds a module
value has to carry them.

## The claim

A module value re-evaluated by `Nbe` loses its entries' `Vars` and `Bounds`: the
rebuilt `ModuleEntry.Impl` keeps the name, kind and dictionary type but not the
variables the impl's head bound, nor the dictionaries it now takes. If the rebuilt entry
is then exported or selected, an impl that was generic becomes one that is not — silently,
since the head's variables are not part of the name or the dictionary type.

The fix in that ticket worked around nothing here: the paths it exercised (top-level,
module and unit impls, through `open`, `export` and `import`) keep their `Bounds`, which
is how the gate passes. This ticket is for the paths nobody measured.

## Why it is plausible rather than speculative

A `ModuleEntry.Impl` is a record: a re-evaluation that reconstructs it from the *value*
layer has only what the value carries, and the value is a dictionary — evidence erasure
is the design ([traits](../topics/traits.md): trait evidence is not a user-facing value).
So the safe shape is that `Vars`/`Bounds` live in a place a re-evaluation cannot lose,
and an implementation that keeps them only in the entry is one reconstruction away from
dropping them.

## What to measure before designing anything

1. Construct a module value that is re-evaluated and then exported — the shortest is a
   module built by a function and returned, then `export`ed or selected from.
2. Read the resulting `ModuleEntry.Impl.Vars`/`.Bounds` and compare with the source impl's.
3. Only if they differ: decide whether the fix is that the entry is carried rather than
   rebuilt, or that the impl's `Vars`/`Bounds` are derivable from something the value
   already has (its head, its dictionary type) — the second is cheaper but only works if
   the head survives in the dictionary type.

## Measured 2026-09-27 — the claim is **true**, and a program shows it

**Verdict: true.** A module whose impl carries `Vars` goes through a function return and
`open`, and the rebuilt entry has `Vars` empty — the generic impl stops matching, so the
program that should answer `3` fails. This is not just an internal field difference; the
two programs below differ by exactly the re-evaluation and produce different verdicts.

### The probe

`/tmp/nbe-vars-recon/d.fun` (function-built, re-evaluated) and the control
`/tmp/nbe-vars-recon/f.fun` (same module written in place, no re-evaluation):

```fun
{ trait Size(A) = sig { size : A -> I64 };
  impl Size(I64) = module { size = fn(n) { 1 } };
  Make = fn(u) { module { pub impl s : Size(Option(A)) = module { size = fn(o) { 3 } } } };
  M = Make(Unit);          # control f.fun: M = module { pub impl s : Size(Option(A)) = module { size = fn(o) { 3 } } };
  open M;
  Size.size(Some(5)) }
```

Run as `dotnet test/Fun.Conformance/bin/Debug/net10.0/Fun.Conformance.dll --file <path>`:

| program | source entry | `open`'s entry | result |
| --- | --- | --- | --- |
| control `f.fun` (module literal) | `Vars=1` | `Vars=1` | `VALUE 3` |
| `d.fun` (module returned by `Make`) | `Vars=1` | **`Vars=0`** | `ELAB missing implementation of \`Size\`` |

The values are read by a temporary `Console.Error.WriteLine` in `OpenImpl`
(`src/Fun.Compiler/Elaborator.Traits.cs`) naming `impl.Vars.Length`; the baseline (no
instrumentation, no other edit) is `ELAB missing implementation of \`Size\`` for `d.fun`
and `VALUE 3` for `f.fun`. The failure is the described symptom exactly: `Size(Option(A))`
is generic; with `Vars=0` its head term `Size(Option(Meta))` no longer matches
`Size(Option(I64))` in `Matches`, so resolution reports a missing implementation.

The same instrumented run, on the existing passing
`test/conformance/cases/values/trait-generic-impl-bound-through-export.fun`, shows a
`Bounds`-carrying impl being read back and rebuilt with the fields present in neither
place: `QUOTE-IMPL-BOUNDS name=s vars=1 bounds=1` then `REBUILD-IMPL-PI name=s`. That
case still answers `5` only because its consumers (`open E`, `export M`) read `Vars`/`Bounds`
from the module's *type*, which is elaborated rather than re-evaluated.

### Where the loss happens

- **Origin — readback.** `Nbe.Traits.cs` `QuoteEntry` builds
  `BindingTerm.Impl(...)`, and `BindingTerm.Impl` (`src/Fun.Kernel/Core.Traits.cs:54`)
  has **no `Vars`/`Bounds` field**. The entry loses them the moment a module *value* is
  read back to a term. Callers: `Nbe.Quote` for `VModule` (`Nbe.cs:574`) and
  `QuoteStruct` (`Nbe.Structs.cs:113`) for `VStruct`/struct bindings. `Unify.cs`
  `RenameEntry` reconstructs the same way.
- **Rebuild.** `Nbe.cs`, the `case Kont.ModuleSlot` in the eval loop (~line 347)
  constructs `new ModuleEntry.Impl(f.Slot.Name, f.Slot.Kind, implType, value)`, defaulting
  `Vars`/`Bounds`; `f.Slot` is built by `Core.cs` `BindingTerm.Slots()`, which carries only
  `ImplType`. `Nbe.Generative.cs`/`Nbe.RecTypes.cs` do not touch impl entries.
- **The route that triggers it** is not only a module returned from a function: in
  `Elaborator.cs` `InferLam`, the codomain is `new Closure(ctx.Environment,
  inner.Quote(bodyType))`, so a lambda whose body type is a module reads the module back to
  a `Term.Struct` there and re-evaluates it at every application. `Make(Unit)`'s *type*
  is therefore already `Vars=0` before the value is even considered.

### Which fix is viable

**Carry them (verified).** Add `Vars`/`Bounds` to `BindingTerm.Impl`, to `Slot`, and thread
through `QuoteEntry`, `Kont.ModuleSlot`, `ElaborateImplItem`, `InferExport`, and `Unify.RenameEntry`.
Built as a temporary edit and measured: `d.fun` → `VALUE 3` (`OPEN-IMPL name=s vars=1`),
`trait-generic-impl-bound-through-export.fun` → `VALUE 5` with `REBUILD-IMPL-PI name=s
vars=1 bounds=1`, and the full gate stays green — `conformance: 865 cases, 0 failed`,
xUnit 188/188. The edit was reverted; no `src/` change is in this branch's diff.

**Derive from the dictionary type (not verified).** The head does survive there: at the
rebuild site the `DictType` contains the head meta (`Args = [VTraitDict{Size, [VMeta 950]}]`
for a bound impl; `[Nominal{Option, [.., Meta 950]}]` otherwise), so a scan could recover
`Vars`. It is unproven for `Bounds` (the Var→trait mapping lives in the implicit Pi domains)
and for solved metas; not attempted, since carrying is demonstrated working.

### Not done / not pursued

- The function-local impl in `/tmp/nbe-vars-recon/b.fun` — head variable free in an impl
  inside a function, body using its evidence — still fails under the carry fix, because the
  bound is never promoted (`QUOTE-IMPL-BOUNDS name=s vars=1 bounds=0`). That is
  [an impl declared inside a function does not promote](impl-in-a-function-does-not-promote.md),
  an independent bug; the probe above was chosen with a bound-free body precisely to keep the
  two apart.
- Did not touch `src/`, other tickets, `docs/wayfinder/fun-design-map.md`, or `docs/STATUS.md`.
- Follow-up needed: a fix ticket for the carry (the edit above is a working sketch), and the
  adjacent function-local-promotion ticket.

## Reading

- `src/Fun.Compiler/Nbe.cs`, `Nbe.Generative.cs`, `Nbe.RecTypes.cs` — where a module
  value is rebuilt
- `src/Fun.Compiler/Elaborator.Traits.cs` — `TraitEvidence.Bounds`, `ImplBound`,
  `ModuleEntry.Impl`
- `src/Fun.Kernel/Core.Traits.cs` — `ModuleEntry` and what a re-evaluation can reconstruct
- [port: identity must survive re-evaluation](port-identity-survives-reevaluation.md) —
  closed, and the closest earlier encounter with this family of problems
