---
title: Pin the grouping rules with cases
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: "Closed 2026-09-27 by the `grouping-cases` fork (79fccd3, merged into `main`). Both unpinned rules now have conformance cases and the third was confirmed already pinned, so nothing on this ticket is left. Verified on the merge: `dotnet build` 0 errors, xUnit 186/186, `conformance: 792 cases, 0 failed` (790 + 2)."
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

## Closed 2026-09-27

Two cases, both in `test/conformance/cases/macros/`:

- **`non-trailing-decl-hole-takes-one-group.fun` → `3`.** The form had to be written, because
  `with_decls` existed nowhere in the repo: `syntax with_decls { with_decls $(ds : List(Decl)) in
  $(body : Expr) => { r = module { $ds; pub value = $body }; r.value } }`, used as the ticket's own
  `with_decls { x = 1; y = 2 } in x + y`. It discriminates: a hole that did not stop at the `}`
  would swallow `in x + y` and the form would not match at all.
- **`non-trailing-hole-matches-one-term.fun` → `error`**, written from the ticket's own example
  with the vocabulary bound, so a greedy `$cond` would answer `5` instead. The refusal's wording
  is `no matching branch for syntax choose`, not the ticket's illustrative *"`<` where `then`
  expected"* — the `.expect` convention is error-or-not, with wording
  implementation-specific (`test/conformance/cases/README.md`), so the case is written against the
  outcome, not the message.
- **Item 3 confirmed already pinned** (`macros/core-260.fun`,
  `imports/order-group-through-unit-path.fun`); no case added.

The runner agreed with the ruled rules in both cases — neither was written from the runner's
output, and neither needed the stop-and-report rule. **Not exhaustive anyway**: the searches were
by filename and body text, so a case pinning item 1 or 2 under a name neither reached could still
be hiding; finding one now costs nothing.
