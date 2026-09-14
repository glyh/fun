---
title: A qualified constructor nested in a pattern argument is unknown
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: The nesting was incidental. A type chain's payloads name their members with [Core.NomRef], and [eval] found that nominal by scanning the environment for its spelling, so a chain member spelled like a nominal already in scope (an outer [type E], the prelude's [Syntax.EffectRow]) resolved to that other type and the nested head was not among its constructors. [NomRef] now carries the nominal id and [eval] finds the nominal by id; [quote] and unification emit [NomRef] for every nominal (nullary ones used to become [Con name]), and [eval_con] no longer matches nominals at all.
closed_date: 2026-09-14
blocked_by:
---

# A qualified constructor nested in a pattern argument is unknown

## Defect

`match … do M.MkA(M.MkE(n)) -> …` fails with `UnknownConstructor "MkE"`: the
outer qualified head resolves, the nested one does not. Not investigated.

## Where to look

`lib/semantic/typecheck/elab_patterns.ml` / `elab_match.ml` — how a
sub-pattern's head is resolved once the argument's expected type is known
(the "constructor labels inside a scrutinee's known type" path that
[names-resolve-without-spelling](names-resolve-without-spelling.md) left as
member lookup) versus a qualified head resolved by entry. Instrument which
path the nested head takes before changing either.

## Found by

The EffectRow / `stx_` spelling change (2026-09-14), incidentally.

## Root cause (2026-09-14)

Instrumenting the payload type `elab_patterns` checks the nested head against
showed `MkA`'s payload body `NomRef(EffectRow, [])` evaluating to the
prelude's `Syntax.EffectRow`, not the chain member: the payload closure's
environment is `ctx.env @ placeholders`, and `Nbe.eval_con` scanned innermost
first *by name*, entering modules. Nested patterns were only where the wrong
type became visible. Regression tests: `path_heads` "a chain member spelled
like an outer type" / "… like a prelude type", plus nested qualified and
unqualified heads in both orders.

Still by spelling: `Con` for a quoted constructor (`VCon`) — out of scope here.
