---
title: One set literal; effect rows and bound sets are sets pinned by position
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# One set literal; effect rows and bound sets are sets pinned by position

Proposed by the user (2026-09-16), deferred: `{a, b}` is one first-class set
literal, `Set(A)`; a position only pins the element type.

```fun
s = {1, 2, 3}              // Set(I64)
A ->{Log, Exc} B           // position expects Set(Effect)
fn[T : {Eq, Show}](…)      // position expects Set(Trait)
IO = {Log, Exc}            // a named, computed row
```

Removes `{…}` meaning three things (block / effect row / bound set). To settle:

1. `{x}` alone: block or singleton set? Candidate: a set where a set is expected,
   a block elsewhere, `{x,}` forces a set.
2. Element equality: conversion at type level (as rows today); a runtime set needs
   `Eq`/`Ord` on its elements (prelude type backed by a trait).
3. Open sets `{Log | e}`: the row unification algorithm made generic over element
   type.
4. Spelling of a named row on an arrow (`->IO`, `->{..IO}`, …).

**For now** (effect-arrow-syntax, small-followups item 4): bounds `[T : {Eq, Show}]`
and rows `A ->{Log, Exc | e} B` are dedicated syntax in those positions only.
