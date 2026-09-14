---
title: A qualified constructor nested in a pattern argument is unknown
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
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
