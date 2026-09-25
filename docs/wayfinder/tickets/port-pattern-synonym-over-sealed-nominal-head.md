---
title: "Port: a pattern synonym over a sealed-nominal head"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-25
resolution: Closed 2026-09-25 - implemented by a fork (1d53a17, merged) and verified by the integrator - 755 cases 0 failed and 184/184 xUnit where the port was at 751, dune test green at 755/0 with 31 divergences. One deviation from this ticket's prescribed mechanism is recorded below, with the reason it was necessary.
assignee:
blocked_by:
---

# Port: a pattern synonym over a sealed-nominal head

> ## Resolution (2026-09-25) — closed, with a recorded deviation
>
> Implemented by a fork (`1d53a17`, merged) and verified by the integrator re-running both
> suites: port `755 cases, 0 failed` (was 751) and `184/184` xUnit; `dune test` green at
> `755 cases, 0 failed, 31 divergences`. All four new cases agree in **both** runners:
>
> | case | expect | OCaml | port |
> | --- | --- | --- | --- |
> | definition (`...-sealed-nominal-head`) | `1` | `VALUE 1` | `VALUE 1` |
> | use in a match (`...-match`) | `42` | `VALUE 42` | `VALUE 42` |
> | a second evaluation's `Symbol` (`...-stamp`) | `0` | `VALUE 0` | `VALUE 0` |
> | the former half (`...-former-head`) | `error` | `ELAB UnknownConstructor "Option2"` | `ELAB unknown constructor \`Option2\`` |
>
> **The deviation, recorded because this ticket prescribed the opposite mechanism.** It said the
> head is a definition-site term and must run under a **definition-site closure** (its environment
> and width). The fork implemented exactly that first, and it **fails**: inside the module the
> elaborated head term is `Dot(Var 1, Symbol)` whose slot is a `VVar` elaboration artifact, so
> under the recorded environment it stays a stuck `VNeutral`. What shipped is **prototype
> parity** — the head *term* is carried in the template and re-evaluated at each match under the
> **match's** environment, which is what `same_instance mc env …` does at `nbe.ml:716`. The
> property this ticket was actually protecting survives: no pre-applied `Value` (so no double
> application) and the head is re-evaluated per match rather than frozen. The stamp case is the
> proof that identity still separates two instances.
>
> **The hazard that mechanism shares with the prototype, recorded rather than guessed:** a
> use-site environment *wider* than the definition site's could resolve the head term's variable
> to a different binding than intended. **It fired on 2026-09-25.**
> [A pattern synonym's nominal head is captured by the use site's scope](pattern-synonym-nominal-head-captured-by-use-site.md)
> has three minimal reproducers — one extra name in scope around the `match` is enough
> (`fn(x : Type) { match (…) { M.S => 42, … } }`) — and they fail in **both** runners, so this is
> a language defect the parity route shipped rather than a theoretical exposure. The fix is the
> **definition-site closure this ticket originally prescribed**, with the `VVar` artifact above as
> the obstacle to get past, and it is that ticket's work.
>
> **The former half was split off**, as the ticket asked: `RejectFormerHeads` walks the surface
> right-hand side and a head naming a type former now throws `FunException("unknown constructor …")`
> where the port used to refuse — parity with the prototype's `UnknownConstructor`, covered as an
> ordinary `error` case. Do not "fix" that half; the prototype refuses it too.
>
> **Two smaller reach-ins:** `FillSynonymParams` now recurses into a `NominalHead`'s parameters,
> and `CollectSynonymMetas` walks a `VRef` cell's content (the `Symbol` nominal captures the
> table cell). The integrator also corrected three of the four new case comments, which described
> the prescribed definition-site closure rather than the shipped match-site re-evaluation — a
> comment a reader would have been right to trust and wrong to act on.

Split out of [the type-case right-hand side ticket](port-pattern-synonym-over-type-case-rhs.md)
when its *struct* half landed (2026-09-25). What remains is the half that needs a
**definition-site closure**, because a nominal type-case head carries a head *term*.

## The program, and both runners (integrator, verified on the `747/0` base)

```fun
{ SymbolTable = fn(u : Unit) { module {
    table = ref("");
    pub type Symbol = Sym(String);
    pub intern = fn(s : String) { table <- s; Sym(deref(table)) } } };
  st1 = SymbolTable(());
  M = module { pub pattern S = st1.Symbol };
  1 }
```

| runner | output |
|---|---|
| OCaml | `1` |
| port | `not ported yet: a pattern synonym over a nominal type-case pattern` (`Elaborator.Patterns.cs`) |

## Half of this refusal is parity, and it is worth keeping straight

A **former** head does not work in the prototype either: `pub pattern IsOpt(a) = Option(a)`
answers `UnknownConstructor "Option"` in OCaml, so the port's refusal there *agrees* with it.
Only the **zero-argument sealed-nominal** head — `st1.Symbol`, a nominal sealed by a generative
module's evaluation — is a gap. Do not "fix" the former half while passing through.

## What it needs

The head is a term from the **definition site**, so it must run under a definition-site
**closure** (its environment and width), not under the use site's environment. That is precisely
what the earlier sketch tried to sidestep by storing a `Value` instead of a `Term`, and why it
was rejected: the value is already applied to arity metas (a second application would
double-apply) and it discards the runtime-env re-evaluation E11's sealed projections need. The
next attempt should carry the closure, not the value.

## Tests

- The program above, an **ordinary** shared case once fixed (`expect` `1`; the prototype
  answers `1`, so nothing goes in `prototype-divergences.txt`).
- A companion that *uses* the synonym in a match, so the direct-match path runs.
- One where a **second evaluation's** same-named nominal must **not** match — the stamp — so the
  closure route proves it keeps identity rather than merging two instances.

## Reading

- the parent ticket's Resolution (why the struct half was cheap and this one is not)
- [the generative former's identity residue](port-generative-former-identity-residue.md) — the
  other remaining gap in the same identity area, and **queued ahead of this one** for that
  reason: both change how a nominal head is compared, so they must not be in flight together
- `Nbe.Generative.cs` (`MatchesNominalHead`, `SameInstance`), `Elaborator.Patterns.cs`
  (`ContainsNominalHead`)
