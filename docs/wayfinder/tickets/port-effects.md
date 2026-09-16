---
title: "Port: algebraic effects"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: algebraic effects

Wave 2 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `effect E(params) { op : A -> B }` declarations, `perform E.op(arg)`, handlers
  (`match` effect branches, 5 cases), `resume`, and effect rows on arrows:
  `A ->{E, F} B`, open `->{E | e}`, inferred `->{_}`, `~>` polymorphic arrows, rows
  on `fn … ->{E} T { … }` definitions and methods (82 cases blocked on `effect`).
- Handlers are deep and one-shot; a captured continuation is a slice of the
  machine's frame stack (decided), and tunneling skips lexical handlers by
  instance (E5). The evaluator stays a loop: performing and resuming never recurse
  on the native stack.
- **Sequencing guard:** add a row to `Term.Pi`/`Value.VPi` as an *optional* member
  that defaults to the empty (pure) row, so no other fork's `Pi` construction
  changes. The rec fork treats every arrow as pure today (see
  [port-recursive-definitions](port-recursive-definitions.md) follow-ups):
  purity must now come from the row.
- Refs (`ref`, `deref`, `<-`, `Mutate`) are **out of scope** (next wave).

## Decided rules to read first

`docs/wayfinder/topics/core-tt-domain-model-effects.md` and
`algebraic-effects.md`; [bare-arrow-is-pure](bare-arrow-is-pure.md),
[effect-arrow-syntax](effect-arrow-syntax.md),
[multi-tail-effect-rows](multi-tail-effect-rows.md),
[handlers-tunnel-callback-effects](handlers-tunnel-callback-effects.md). The
glossary's effects section follows the decided model; where the prototype
departs, the prototype is the defect.

## Progress (2026-09-16)

Merged from `port/effects` (head `d33325e`). Effect families in blocks and modules;
`perform E.op(arg)` through module and import paths; deep, lexical, one-shot
handlers with `resume`; rows on arrows (closed, open, `->{| r}`, `->{_}`) as an
optional, pure-by-default member of `Pi`. A lambda body's effects become its row
and a checked lambda is held to its declared row; a call emits its callee's row
and tunnels through an open one; a value that performs is never evaluated at check
time and types must be pure; a program or unit entry leaves no effect unhandled
and every `_` row is solved; a closure performing an effect its enclosing match
handles may not escape it (E6); rec purity reads the row. The machine holds a
handler as a frame: a request pops frames to its handler, they become the
continuation, `resume` pushes them back (pinned a million frames deep, and
one-shot). Newly passing: errors elab-200–202, 206, 207, 210, 212, 214, 221, 229,
232, 240–242, pure-colon-body, unsolved-row-field, escape-module-member; ok
elab-199, 204, 205, 208, 209, 213, 215, 216, 220, 230, 231, 243; values core-044–046,
062, 064, 100, 101, elab-003, imports core-184, 185. Merge fixes: `~>` recognised
through its base role; `Syntax.Map` covers effect forms (fixed core-237).
C# 203/647; xUnit 90.

**Not done (ticket stays open):** `~>` elaboration (6 cases), rows on method
results (5), a module binding that performs (generative nominals, E11), rows that
name their own parameter (refs). Most remaining effect cases need prelude
operators or `if`.
