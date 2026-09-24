---
title: "Port: a pattern synonym over a sealed-nominal head"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: a pattern synonym over a sealed-nominal head

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
