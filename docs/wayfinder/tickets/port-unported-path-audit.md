---
title: "Port: audit every unported path against the prototype"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: audit every unported path

Step 3 of [port-parity-plan](port-parity-plan.md). **This is an audit — it writes no
feature code.** Its deliverable is this file: one verdict per site, so the invisible
delta becomes a number and the real gaps become tickets.

`dotnet/test/Fun.Conformance/Program.cs` counts a `NotImplementedException` as a
failure *only when a case reaches it*. So a `not ported yet` path that no shared case
exercises is a silent hole in "the port is complete". There are **60
`NotImplementedException` sites in 25 files** under `dotnet/src` (58 of them spelled
"not ported yet") and **15 `ponytail:` stopgaps** — the stopgaps already state their
own reason, so they need only a verdict, not a diagnosis.

## Method

For each site: read the throw and the switch/list around it, name the program form
that reaches it, then decide which of three it is.

1. **Real gap** — the prototype handles that form. Write the smallest program in a
   scratch file (not in `test/conformance/cases`), run it in both runners, and
   report the prototype's outcome. The real gap is a ticket line; the case is added
   by whoever ports it (the conformance count must never drop).
2. **Parity** — the prototype also refuses (or has no such form). Then the throw is
   wrong about the *language*: it must become the proper `FunException` so a case
   expecting `error` can pass for the right reason (porting convention 2). Cite the
   prototype file and line that refuses it.
3. **Unreachable** — no program reaches it; it is a lying catch-all. Say why.

Use the prototype as a *map*, not as the spec — where they disagree, the domain
model decides and a prototype defect is its own verdict (convention 5). Two
verdicts are already established and are examples of 2:
`Enforest.cs:412` bare bracket expressions (`lib/expand/enforest.ml:87`, "not in
Phase 7A") and `Enforest.Roles.cs:338` a dotted order-group reference (an open item
on [brackets-decide-grouping](brackets-decide-grouping.md)).

## Resolution (2026-09-20)

Audit of `main @ d58af64` (snapshot). The grep finds **62** sites; the table below
also adds the two `Elaborator.Match.cs` sites the ticket's list missed and folds
`Elaborator.Generalise.cs:44` into `Core.Shift.cs:67` (it is that catch-all reached
from `ClosedUnder`). Counts:

- **Real gap: 17** — 15 new, plus 2 already triaged by
  [port-stage2-residue](port-stage2-residue.md).
- **Parity (→ `FunException`): 13** — the throw is wrong about the language.
- **Unreachable: 17** — exhaustive catch-alls and driver-injected callbacks.
- **Undecided — needs the user: 9** — recorded with a program.
- **Owned by another fork: 4** — marked, not diagnosed.

Two real gaps were reproduced with programs and run through both runners; the rest
of the real gaps are latent (a form no conformance case reaches) and carry the
*form* rather than a program. Verdicts rest on reading the prototype's
`map_subterms` (`core.ml:618`), the reflection reader (`macro_eval.ml`), the
enforester and the elaborator; where the prototype and the domain model could
disagree, the row is undecided, not guessed.

This is a snapshot: `fork-e11` and `fork-residue` are writing C# against the same
base, so some rows below may be stale by merge. `main` has since advanced four
docs-only commits (the audit was merged with them); the C# under test is still
`d58af64`.

### Verdict table

Legend: **RG** real gap · **P** parity (→ `FunException`) · **U** unreachable ·
**?** undecided · **OWNED** another fork.

| # | Site | Verdict | Prototype reference / note |
|---|---|---|---|
| 1 | `Fun.Kernel/Syntax.Map.cs:174` | **U** | all 43 `Syntax` variants are handled; the catch-all is dead until a variant is added |
| 2 | `Fun.Kernel/Syntax.Map.cs:225` | **U** | all 15 `Binding` variants handled |
| 3 | `Fun.Kernel/Syntax.Map.cs:256` | **U** | all 10 `Pattern` variants handled |
| 4 | `Fun.Kernel/Core.Shift.cs:67` | **RG** | `map_subterms` (`core.ml:618`) walks every kind; this switch misses `Tunnel`, `RecursiveOccurrence`, `RecordConstruct`, `Sig`, `TraitRef`, `TraitDictTy` — see G2 |
| 5 | `Fun.Kernel/Core.Shift.cs:107` (`MapBindings`) | **RG** | prototype's `bindings` helper handles `ImplBind`; C# handles only `Let`/`Open` — `BindingTerm.Impl` (`Core.Traits.cs:54`) is unwalked — see G2 |
| 6 | `Fun.Expand/Expander.cs:212` | **U** | all 43 `Syntax` forms handled (verified by case list) |
| 7 | `Fun.Expand/Expander.cs:357` | **U** | all 15 `Binding` forms handled (`Trait`/`Impl` at `:331`) |
| 8 | `Fun.Expand/Expander.Macros.cs:306` | **RG** | prototype's `syntax_operator_arg` handles a typed operator macro; no program constructed — see G4 |
| 9 | `Fun.Expand/Enforest.cs:220` | **P** | `enforest.ml:1445` `unsupported "unsupported module item"` |
| 10 | `Fun.Expand/Enforest.cs:291` | **P** | `enforest.ml:514` `unsupported "unsupported Phase 7A keyword"` (plus the `do`/`let` errors at `:456`,`:471`) |
| 11 | `Fun.Expand/Enforest.cs:387` | **RG** | `f{e}` is an implicit argument: `enforest.ml:632-644` — see G1 |
| 12 | `Fun.Expand/Enforest.cs:396` | **P** | `enforest_util.ml:342` `unsupported "unconsumed terms after expression: <+>"` |
| 13 | `Fun.Expand/Enforest.cs:412` | **P** | established: `enforest.ml:87` "not in Phase 7A" |
| 14 | `Fun.Expand/Enforest.Traits.cs:143` | **P** | `enforest.ml:407` `error "an impl in a signature must be named"` |
| 15 | `Fun.Expand/Enforest.Roles.cs:148` | **P** | `enforest.ml:684` `error "not an infix operator: ~>"`; note the throw precedes the `continues` guard, so `1 + 2 ~> 3` throws where the prototype parses `(1+2) ~> 3` and type-errors (probe P5) |
| 16 | `Fun.Expand/Enforest.Roles.cs:338` | **P** | established; open item on [brackets-decide-grouping](brackets-decide-grouping.md) |
| 17 | `Fun.Expand/Enforest.Roles.cs:845` (`WithBody`) | **?** | a quoted-syntax block statement other than `let`/`rec`/`open`/`syntax`/`macro`; no program constructed |
| 18 | `Fun.Compiler/Unify.cs:199` | **U** | heads are exactly `HVar`/`HMeta`/`HPrim` |
| 19 | `Fun.Compiler/Unify.cs:204` | **RG** | residue: renaming `FDot`/`FRefGet`/`FRefSet`/`FMatch` frames; `elab-059` |
| 20 | `Fun.Compiler/Unify.cs:206` | **?** | `Rename` misses `VCont`/`VGlued`/`VModule`/`VStruct`(bindings)/`VTrait`/`VTraitDict`/`VSig`/`VRecord`/`VFix`; which can appear in a meta solution needs a ruling |
| 21 | `Fun.Compiler/Unify.Neutrals.cs:35` | **U** | every `Frame` kind is handled; the default is a lying catch-all |
| 22 | `Fun.Compiler/Reflection.cs:177` | **U** | a pattern head is `Var` or `OpenChoice` by grammar |
| 23 | `Fun.Compiler/Reflection.cs:282` | **U** | every `Syntax` form but `PatternSynonym`, which is reflected through `Binding.Let` (`:375`) |
| 24 | `Fun.Compiler/Reflection.cs:372` | **U** | all 10 `Pattern` variants handled |
| 25 | `Fun.Compiler/Reflection.cs:399` | **U** | all 15 `Binding` variants handled |
| 26 | `Fun.Compiler/Reflection.cs:603` | **RG** | prototype reads `UnitTok` (`macro_eval.ml:494`); a macro returning one is refused — see G3 |
| 27 | `Fun.Compiler/Reflection.cs:718`,`:922` | **RG** | prototype reads any number of trait params (`macro_eval.ml:603`,`:893`); the C# `TraitDef`/`Trait` models hardcode one — see G3 |
| 28 | `Fun.Compiler/Reflection.cs:726`,`:928` | **RG** | prototype reads any number of impl args (`macro_eval.ml:610`,`:898`) — see G3 |
| 29 | `Fun.Compiler/Reflection.cs:762` | **RG** | prototype reads `RawTypeDef` (`macro_eval.ml:593`); the ADT still has it (`std/stage1.fun:32`) — see G3 |
| 30 | `Fun.Compiler/Reflection.cs:850` | **RG** | prototype reads a param's bound paths (`macro_eval.ml`, `u_param`); a macro output with `[A : Trait]` is refused — see G3 |
| 31 | `Fun.Compiler/Nbe.cs:197` | **U** | all 39 `Term` variants handled |
| 32 | `Fun.Compiler/Nbe.cs:561` | **?** | all `Value` kinds handled but `VCont`; whether a continuation is read back as a term needs a ruling |
| 33 | `Fun.Compiler/Nbe.StuckMatch.cs:50` | **RG** | `ArmBinders` misses `CorePattern.SynonymParam`; the prototype's `pat_binder_count` handles it — see G2 |
| 34 | `Fun.Compiler/Nbe.StuckMatch.cs:54` | **U** | `InTree` returns non-null for every arm of a checked match |
| 35 | `Fun.Compiler/Nbe.Match.cs:87` | **?** | a known scrutinee with an unknown *part*: the prototype takes the decision tree's `default` (`nbe.ml:860-877`), the C# throws. See `ponytail:` at `Nbe.StuckMatch.cs:12` |
| 36 | `Fun.Compiler/Nbe.Effects.cs:106` | **RG** | residue: `core-102` |
| 37 | `Fun.Compiler/Elaborator.cs:304` | **?** | `Infer` has no case for `ProdTy`/`Stx`/`Block`/`Instantiate`/`MacroDef`/`OperatorUse`/`SyntaxDef`/`Elaborated`; most are consumed by the expander, but `Syntax.Stx` is deliberately left for the elaborator (`Expander.cs:209`). Needs a per-form ruling |
| 38 | `Fun.Compiler/Elaborator.cs:442` | **U** | after expansion a binding is only `Let`/`Open`/`Export`/`RecGroup`/`Effect`/`Trait`/`Impl` |
| 39 | `Fun.Compiler/Elaborator.cs:506` | **P** | `elab_resolve.ml:387` `raise (ElabError NotAModule)`; probe R2 |
| 40 | `Fun.Compiler/Elaborator.Export.cs:52` | **P** | `elab_infer.ml:150` `raise (ElabError NotAModule)` |
| 41 | `Fun.Compiler/Elaborator.Implicits.cs:50` | **RG** | prototype builds a meta `Pi` and unifies (`elab_apply.ml:158-175`) — see G1 |
| 42 | `Fun.Compiler/Elaborator.Imports.cs:16` | **U** | `Driver.Elaborate` always supplies a `Loader` |
| 43 | `Fun.Compiler/Elaborator.Macros.cs:25` | **U** | driver always supplies an `Expander` |
| 44 | `Fun.Compiler/Elaborator.Macros.cs:26` | **U** | driver always supplies a `Loader` |
| 45 | `Fun.Compiler/Elaborator.Generative.cs:52` | **?** | a generative nominal not bound as a module member; the prototype's `check_generative_escape` (`elab_effects.ml:298`) refuses the escape instead, so the two refuse different things |
| 46 | `Fun.Compiler/Elaborator.Patterns.cs:47` | **P** | `elab_patterns.ml:187` `raise (ElabError ApplyingNonFunction)` |
| 47 | `Fun.Compiler/Elaborator.Patterns.cs:72` | **?** | a synonym RHS that is not a constructor pattern; prototype accepts the *definition* (`elab_infer.ml:490`), probe Q2 (module form) → `ElabError(TupleLengthMismatch)` at use. The block form fails in the prototype for an unrelated, listed reason ([pattern-synonym-not-a-block-declaration](pattern-synonym-not-a-block-declaration.md)). Needs a ruling |
| 48 | `Fun.Compiler/Elaborator.Patterns.cs:76` | **?** | a synonym over a type-case pattern; prototype handles type-case patterns generally |
| 49 | `Fun.Compiler/Elaborator.Patterns.cs:88` | **?** | a synonym whose parameter types are not fixed |
| 50 | `Fun.Compiler/Elaborator.Patterns.cs:200` | **OWNED** | being fixed by [port-nominal-identity](port-nominal-identity.md) |
| 51 | `Fun.Compiler/Elaborator.RecTypes.cs:62` | **RG** | a recursive enum whose payload captures a variable its body does not name; the prototype predicts captures by evaluation — see G5 |
| 52 | `Fun.Compiler/Elaborator.Structs.cs:80` | **P** | `enforest.ml:1509` `unsupported "unsupported struct item"` |
| 53 | `Fun.Compiler/Elaborator.Structs.cs:209` | **P** | `elab_infer.ml:936` `raise (ElabError ApplyingNonFunction)` |
| 54 | `Fun.Compiler/Elaborator.Structs.cs:244` | **P** | `elab_type_expr.ml:74` `raise (ElabError ApplyingNonFunction)` |
| 55 | `Fun.Compiler/Elaborator.Structs.cs:290` | **OWNED** | being fixed by [port-stage2-residue](port-stage2-residue.md) (4 cases) |
| 56 | `Fun.Compiler/Elaborator.Enum.cs:295` | **RG** | free *names* a `Syntax` uses; the traversal misses `Sig`/`Struct`/`TraitDef`/`ImplDef`/`MacroCall`/`Quote`/… — see G2 |
| 57 | `Fun.Compiler/Elaborator.Enum.cs:314` | **RG** | same, over a `Binding` — see G2 |
| 58 | `Fun.Compiler/Elaborator.Enum.cs:352` | **RG** | free *levels* a `Value` mentions; misses `VEffect`/`VModule`/`VSig`/`VTrait`/`VStruct`/… — see G2 |
| 59 | `Fun.Compiler/Elaborator.Match.cs:93` | **OWNED** | constructor pattern head of a non-nominal (`elab-067`), fixed by [port-stage2-residue](port-stage2-residue.md) |
| 60 | `Fun.Compiler/Elaborator.Match.cs:161` | **OWNED** | same site, other branch |
| 61 | `Fun.Compiler/Elaborator.Macros.cs`/`Elaborator.cs` others | — | no further `NotImplementedException` found on reading |

### The 15 `ponytail:` stopgaps

Each already names its own reason; the verdict is one of *feature the prototype has*
(port it), *shared choice* (the prototype does the same), or *divergence* (which is
right? needs the user).

| Stopgap | Verdict |
|---|---|
| `Elaborator.cs:368`, `Elaborator.Enum.cs:48`, `Elaborator.Generative.cs:12` (no run-time stamp) | feature the prototype has → [port-nominal-identity](port-nominal-identity.md) |
| `Elaborator.Structs.cs:31` (method types read where the first method is elaborated) | shared choice (comment names the prototype's own restriction) |
| `Elaborator.Effects.cs:288`, `:304` (row read with a rigid argument; stand-in codomain) | shared choice |
| `Elaborator.PolyArrows.cs:60` (rank-1 higher-order binders) | shared choice |
| `Elaborator.Macros.cs:21` (a typed macro argument elaborates twice) | shared choice (prototype reuses the elaboration; observable only in cost) |
| `Elaborator.Refs.cs:122` (one pass over older metas per discharge site) | shared choice (comment says "as the prototype") |
| `Nbe.Patterns.cs:88` (nominal-head match nests one evaluation) | shared choice (no observable difference; document for uniformity) |
| `Nbe.Rec.cs:99` (read-back equality has no eta, does not unfold) | shared choice (prototype's equality is the same) |
| `Nbe.StuckMatch.cs:12` (only an unknown scrutinee waits) | **divergence** — hides row 35 / G6; needs a ruling |
| `Core.Rec.cs:34` (environment identity by reference) | shared choice (structural identity is the prototype's too) |
| `Budget.cs:11` (limit is a constant) | shared choice (no surface syntax to raise it in either) |
| `ScopeSet.cs:10` (`ImmutableSortedSet` vs sorted list) | shared choice (representation only) |
| `Fun.Conformance/Program.cs:53` (timed-out run's thread keeps spinning) | divergence (port-only; the OCaml runner has no timeout) — cosmetic, runner-only |

## Real gaps, in priority order

### G1. Implicit application not ported (2 programs, both verified)

The prototype handles two forms the port refuses; both came back with the value `1`.

- `Fun.Expand/Enforest.cs:387` — `f{e}` written in braces is an implicit argument
  (`enforest.ml:632-644`).
  ```fun
  { f = fn[A : Type](x : A) { x }; f{I64}(1) }
  ```
  Prototype: `1`. Port: `not ported yet: an implicit argument written f{ e }`.
- `Fun.Compiler/Elaborator.Implicits.cs:50` — applying `f[x]` to a value of unknown
  function type; the prototype synthesises an implicit `Pi` and unifies
  (`elab_apply.ml:158-175`). This is the decided rule from
  [deref-of-unknown-type-is-not-a-reference](deref-of-unknown-type-is-not-a-reference.md)
  ("applying a value of unknown type unifies it with a fresh arrow"), so it is a
  real gap, not a parity candidate.
  ```fun
  { h = fn(g) { g[I64] }; 1 }
  ```
  Prototype: `1`. Port: `not ported yet: applying a value of unknown function type`.

### G2. Traversals that miss a whole form (latent)

`map_subterms` is the prototype's one statement of every form's binder count
(`core.ml:618`), and the free-name/level walks (`Macro_eval`, `Expand`) have
equivalents. The port's mirrors are partial, so any term/syntax containing a missed
form cannot be walked at all — `Core.Shift.cs:67` (six `Term` kinds),
`Core.Shift.cs:107` (`BindingTerm.Impl`), `Elaborator.Enum.cs:295`/`:314`/`:352`
(free names/levels), `Unify.cs:206` (`Rename`), `Nbe.StuckMatch.cs:50`
(`SynonymParam`). No single small program was constructed; each is reached from a
different feature (generalisation, export, shut-up hygiene, meta solving, stuck-match
readback). One ticket, "traversals must cover every form", is the right shape.

### G3. The reflection reader refuses forms the prototype reads (latent)

A macro may return any constructor of the prelude's `Expr`/`Decl` ADTs
(`std/stage1.fun:32`). `Reflection.cs` reads most of them but throws on: a reflected
unit token (`:603`), a trait with other than one parameter (`:718`/`:922`), an impl
with other than one argument (`:726`/`:928`), `RawTypeDef` (`:762`), and a parameter
with trait bounds (`:850`). The prototype reads all five (`macro_eval.ml:494`,
`:593`, `:603`/`:893`, `:610`/`:898`, `u_param`). The multi-parameter trait/impl rows
are also a *model* divergence: the C# `Syntax.TraitDef`/`Binding.Trait` carry one
parameter where the prototype carries a list.

### G4. A type-aware operator macro (latent)

`Expander.Macros.cs:306`: an operator whose role calls a macro with a typed signature
(`: Expr(T)`) is refused; the prototype's `syntax_operator_arg` admits it. No program
constructed.

### G5. Recursive-enum captures predicted by name (latent)

`Elaborator.RecTypes.cs:62`: a recursive enum whose payload reaches a variable the
body does not name. The prototype predicts captures by evaluation, not by name.

### G6. A stuck match on a known scrutinee's unknown part

`Nbe.Match.cs:87` (hidden behind the `Nbe.StuckMatch.cs:12` stopgap): the prototype
takes the decision tree's `default` when a tested occurrence is neutral
(`nbe.ml:860-877`); the port throws. Recorded as **undecided** because the domain
model has not said which is right.

## Parity: the throw should become a `FunException`

Thirteen sites the prototype also refuses. Each must be deleted and replaced by the
language error, so a case expecting `error` can pass for the right reason.

| Site | Prototype reference |
|---|---|
| `Enforest.cs:220` | `enforest.ml:1445` |
| `Enforest.cs:291` | `enforest.ml:514` |
| `Enforest.cs:396` | `enforest_util.ml:342` |
| `Enforest.cs:412` | `enforest.ml:87` (established) |
| `Enforest.Traits.cs:143` | `enforest.ml:407` |
| `Enforest.Roles.cs:148` | `enforest.ml:684` (guard-order bug noted above) |
| `Enforest.Roles.cs:338` | open item (established) |
| `Elaborator.cs:506` | `elab_resolve.ml:387` |
| `Elaborator.Export.cs:52` | `elab_infer.ml:150` |
| `Elaborator.Patterns.cs:47` | `elab_patterns.ml:187` |
| `Elaborator.Structs.cs:80` | `enforest.ml:1509` |
| `Elaborator.Structs.cs:209` | `elab_infer.ml:936` |
| `Elaborator.Structs.cs:244` | `elab_type_expr.ml:74` |

**Integrator re-verified the sharpest one (2026-09-20).** `Enforest.Roles.cs:148`
throws *before* the `continues` guard, so the port refuses `{ 1 + 2 ~> 3 }` with
``NotImplementedException("not ported yet: the polymorphic arrow ~>")`` while the
prototype parses `(1 + 2) ~> 3` and type-errors. Reproduced in both runners (a case
expecting `error` fails in C# and passes in OCaml): the port's `error` cases do not
simply lack coverage here, the convention-2 violation is live and observable. Move
the `continues` check ahead of the throw.

## Undecided — needs the user

Each is a form the port refuses where the prototype's behaviour is either absent or
itself suspect; the ruling decides a real gap versus a `FunException`:

1. `Elaborator.cs:304` — `Syntax.Stx` (a typed macro argument marker) is left for the
   elaborator by the expander but has no `Infer` case. Program form: a typed macro
   whose argument is placed in the output.
2. `Nbe.Match.cs:87` — neutral sub-occurrence: default arm (prototype) or stuck?
3. `Unify.cs:206` — which `Value` kinds may appear in a meta solution.
4. `Nbe.cs:561` — can `VCont` be read back as a term?
5. `Elaborator.Patterns.cs:72` — a synonym whose RHS is a product pattern. **Independently
   re-probed by the integrator, and the audit's evidence direction was wrong:** the
   prototype does *not* accept the definition and fail at use — it rejects
   `module { pattern P(a, b) = (a, b); pub X = 1 }` **at elaboration** with
   `ElabError(TupleLengthMismatch)`, with no use in the program at all. So the
   prototype refuses too: by convention 2 this is either parity (the throw becomes
   the same language error) or a prototype defect (`P(a, b) = (a, b)` reads like a
   legal tuple-pattern synonym, and `TupleLengthMismatch` is a strange way to say
   otherwise). Which one needs a ruling. (A block `pattern` fails in the prototype
   for the unrelated reason on
   [pattern-synonym-not-a-block-declaration](pattern-synonym-not-a-block-declaration.md).)
6. `Elaborator.Patterns.cs:76` — a synonym over a type-case pattern.
7. `Elaborator.Patterns.cs:88` — a synonym whose parameter types are not fixed.
8. `Elaborator.Generative.cs:52` — sealing a generative nominal not bound as a member.
9. `Enforest.Roles.cs:845` — which quoted-syntax statements take a body.

## Internals parity (the third invisible delta)

The conformance suite cannot see syntax shapes, the unifier, the machine or budget
accounting. OCaml runs **613** Alcotest cases; C# runs 20 xUnit files. The OCaml
suites and their C# counterparts:

| OCaml suite | cases | C# counterpart |
|---|---|---|
| `test/syntax/test_enforest.ml` | 93 | **none dedicated**; `ExpandTests.cs` (4), `RolesTests.cs` (3), `ReaderTests.cs` (3) |
| `test/syntax/test_expand_compat.ml` | 17 | **none** |
| `test/syntax/test_macros.ml` | 5 | `MacroTests.cs` (5) |
| `test/syntax/test_parse_effects.ml` | 18 | `EffectTests.cs` (2) |
| `test/syntax/test_parse_patterns.ml` | 5 | **none dedicated** |
| `test/syntax/test_parse_smoke.ml` | 7 | partial (`ExpandTests`, `ReaderTests`) |
| `test/syntax/test_parse_traits_refs.ml` | 9 | `TraitTests.cs` (9), `RefsTests.cs` (6) |
| `test/syntax/test_scope_sets.ml` | 5 | **none** (no `ScopeSetTests.cs`) |
| `test/syntax/test_spec.ml` | 2 | **none** (parser combinators) |
| `test/syntax/test_line_counts.ml` | 1 | n/a (OCaml LoC cap) |
| `test/semantic/test_elaborate.ml` | 269 | partial (`UnifyTests` 6, `NominalTests` 3, `RecTypesTests` 2, …) |
| `test/backend/test_core.ml` | 172 | partial (`NbeTests` 3, `MatchCompileTests` 7, `RecTests` 3, `MacroTests`) |
| `test/backend/test_macro_driver_stage7.ml` | 10 | `InterleavingTests.cs` (2), `MacroTests.cs` |

Areas with no C# counterpart at all, and no conformance case either (the source-to-
result cases moved to conformance; these are the internals that did not):

- **Budget accounting** — `test_core.ml:1399`, `:1412`, `:2048` (expansion-budget
  overrun names the macro / at the operator / decl expansion) have no
  `BudgetTests.cs`. There is no shared conformance case that can observe a budget
  because the runner's expectations are value/`ok`/`error` only.
- **Enforest shapes and scope sets** — 93 + 17 + 7 + 9 + 5 OCaml cases assert the
  *tree* a parse produced (`Shape`, resolved names, open regions). The C# xUnit
  suite asserts a handful of these.
- **Parser-combinator spec** (`test_spec.ml`) and the `parse_*_shapes` families.
- **Unifier internals** — `test_elaborate.ml`'s shape/error-constructor cases are
  largely uncovered; `UnifyTests.cs` has 6 facts.

I cannot decide whether the missing shape/budget coverage must be mirrored: that is
a test-strategy ruling (mirror the OCaml suites case-for-case, or accept that the
port is behaviour-complete without shape tests). Flagged for the user.

## Prototype defects found

None reproduced. Probe P5 exposed a *port-side* ordering bug (`InfixRoleUse` throws
before the `continues` guard, so `1 + 2 ~> 3` throws where the prototype parses and
type-errors) — that is row 15, a parity conversion, not a prototype defect. No new
ticket filed; the integrator decides.
