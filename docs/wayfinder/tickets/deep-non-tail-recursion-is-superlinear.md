---
title: Deep non-tail recursion runs in superlinear time
parent: ../fun-design-map.md
labels:
  - wayfinder:research
status: open
assignee:
blocked_by:
---

# Deep non-tail recursion runs in superlinear time

## Observation

At run time (unbudgeted `Nbe.run`), a non-tail recursion 400k deep takes
~3.7 s, and time grows faster than depth. Not compared against the commit
before the checker budget (`ba0edd5`); the budget adds O(1) per call on that
path, so it is probably pre-existing.

## Question

Measure against `ba0edd5` first. If pre-existing, find the superlinear step —
suspects: list appends on spines (`sp @ [va]`, `params @ [va]` in
`Nbe.apply_result`), environment representation, or `Fun.protect`/exception
frames per call. A port would transliterate whichever it is.

## Found by

The checker-budget implementation's "running is not budgeted" test, which had
to use a shallow binary recursion instead (2026-09-14).
