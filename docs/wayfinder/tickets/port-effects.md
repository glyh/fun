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
