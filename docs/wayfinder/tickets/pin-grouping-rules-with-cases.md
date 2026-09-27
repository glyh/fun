---
title: Pin the grouping rules with cases
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# Pin the grouping rules with cases

Opened 2026-09-27 while closing [brackets decide grouping](brackets-decide-grouping.md). Every
decision in that ticket is implemented and merged — this is the *coverage* left behind, not a
design question. Two rules were checked and found to have no case; the third is a re-check of one
that does.

## What to pin

1. **A non-last `Decl` hole takes one `{ … }` group.** The rule is
   [`brackets-decide-grouping.md`](brackets-decide-grouping.md)'s "Implementation notes": *"A
   non-trailing `Decl` hole is one brace group of items."* The example the ticket gives is
   `with_decls { x = 1; y = 2 } in x + y` — and `with_decls` appears **nowhere** in the repo, so
   this needs a form that takes a `Decl` hole in non-final position before a case can be written
   at all. Choosing that form is part of the work.
2. **A non-trailing hole matches exactly one term** — an atom or one bracket group. The accepted
   half is pinned (`macros/core-270`, `macros/core-275`: `choose (is_zero 0) then (wrap (inc 2))
   else (wrap 10)`). The refusal half is not: the ticket's own example is `choose x < n then a
   else 0` giving "`<` where `then` expected".
3. **A group through a module path is the same group** — believed already pinned twice
   (`macros/core-260.fun`, `imports/order-group-through-unit-path.fun`). Listed so the check is
   recorded; confirm rather than add.

## What was searched, and what that does not cover

`test/conformance/cases/` was searched for `with_decls`, and for case files whose names carry
`order` / `group` / `assoc` / `hole` / `bracket`, plus the three error cases `elaborate/` holds.
**The search was by filename and body text, not exhaustive** — a case pinning item 1 or 2 under a
name neither search reached would have been missed, and finding one is a fine outcome for this
ticket.

## Rule for whoever takes it

The expected result comes from the rule in
[`brackets-decide-grouping.md`](brackets-decide-grouping.md), **not** from what the runner prints.
If the runner disagrees with the rule, **stop and report it** — that is a bug, and a case written
from the runner's output would freeze the bug into the suite.
