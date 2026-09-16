---
title: "Port: recursive value definitions"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: recursive value definitions

Wave 1 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- `rec f = fn(n) { … f(…) … }` in blocks and module items, and recursive groups
  `rec f = … and g = …`: `Fix` terms and fixpoint values, the checker's
  evaluation budget, and lazy unfolding of pure calls (`VGlued`) as the prototype
  decides it.
- `fn name(params) { … }` declarations that recurse.
- Recursive **type** definitions (`rec T = enum`, recursive records) are **out of
  scope**: stop at them.

## Decided rules to read first

- [recursive definitions stuck on open arguments](recursive-definitions-stuck-on-open-arguments.md),
  [checker evaluation budget](checker-evaluation-budget.md),
  [deep non-tail recursion is superlinear](deep-non-tail-recursion-is-superlinear.md)
  (why the machine exists: recursion depth is bounded by memory, never by the
  CLR stack, and a fixpoint call must go through `Kont` frames).

## Target

Failing cases that use `rec` on a value and need nothing else unported; many
also need operators or `if` from the prelude, so expect few to turn green here.
Pin the machine property with an xUnit test: a deep non-tail recursive fixpoint
does not overflow.

## Other forks

structs, match-enums, implicits, imports run concurrently.

## Resolution (2026-09-16)

Merged from `port/rec` (head `76f96ff`). `rec` and `rec … and …` in blocks and
module items build one `Fix` group; calls unfold through machine frames (a
million-deep non-tail recursion runs without the native stack, pinned in
`RecTests`). While checking, every call and unification step spends from one
budget request, and divergence is an error naming the fixpoint; running a program
has no limit. A pure fixpoint's call is deferred while checking (lazy delta), so
two calls of the same fixpoint on convertible arguments are equal unfolded or not.
`fn name(…)` measures adjacency from the name (the imports fork's report). Applying
a value of unknown function type unifies it with a fresh arrow. 8 new shared cases,
each agreeing with the prototype: `elaborate/rec-divergent-type-budget`,
`rec-divergent-open-argument-budget`, `rec-lazy-delta-same-fixpoint`,
`rec-distinct-fixpoints-unfold`, `values/rec-unannotated-unused`, `rec-group-unused`,
`rec-module-members`, `rec-returns-closure`. C# 69/618; xUnit 65. A first run looped:
forcing a deferred call opened a fresh budget request per unfold; the forcing loop
is now one request.

**Follow-ups:**
- **Purity comes from rows.** A call is treated as known pure whenever its type is
  an arrow, which holds only because effect rows are not ported (every arrow is a
  bare, pure arrow). Revisit when rows land.
- **Budget error site.** The divergence error does not name the source position
  the checker was at.
- **Recursive types by type-name string.** `Elaborator.Rec.cs` detects
  `rec T = enum/struct` with `value.GetType().Name is "Enum" or "Struct"`, because
  the enum node was in another fork. Replace with type patterns once match-enums
  merges (handed to that fork).
- Most existing `rec` cases still need `if`, `match` or arithmetic from the prelude.
