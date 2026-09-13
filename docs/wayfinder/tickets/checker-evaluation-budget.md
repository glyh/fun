---
title: The checker evaluates under a budget, not a termination check
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# The checker evaluates under a budget, not a termination check

## Decision

Zig's model. Surface syntax is deliberately left open.

- **No termination checking.** Any pure function may be evaluated while type
  checking; divergence is not an effect and never appears in a type.
- **An evaluation budget.** Evaluating a closed term counts function calls and
  loop iterations. Exceeding the budget is a compile error naming the call and
  how to raise the budget for that evaluation.
  **Extended by the macro domain-model pass**
  ([core-tt-domain-model-macros](../topics/core-tt-domain-model-macros.md),
  M5): one budget counts every evaluation the checker performs — **macro
  applications included, which are calls**. The macro-expansion depth fuel
  retires into it: a depth guard cannot catch breadth blowup (each output
  spawning two sibling calls at bounded depth), and the count, being a
  property of the program, is stable across compiler versions. Expansion
  failures — budget exhaustion included — become error values, retiring the
  `failwith`s at the macro application sites.
- **Only closed terms evaluate.** A call mentioning an unknown variable stays
  stuck and costs nothing: `fn(n : I64, y : loop(n)) -> …` is fine.
- **What cannot run at check time is an error, not a stuck term.** An extern
  call or a call performing effects in a position the checker must evaluate
  reports that, rather than surfacing later as a type mismatch.

Vocabulary: **Evaluation budget**, **Pure** in [`CONTEXT.md`](../../../CONTEXT.md).

## Evidence

```
do rec loop : I64 -> Type = fn(n) -> loop(n); g = fn(y : loop(0)) -> 1; 2 end
```

hangs the checker (killed after 10 s). Under this decision it is a budget error.

## Why not the alternatives

- **Divergence as an effect (Koka's `div`)** — every recursive helper used in
  types carries it in its signature; too heavy for real code and C FFI.
- **Totality checking (Idris 2, Lean 4, Agda)** — definitions not proven
  terminating stop reducing in types, which forbids the complex compile-time
  computation this language wants, and makes the checker's strength part of
  the semantics.
- **Internal reduction depth (GHC)** — the count follows implementation
  minutiae; GHC warns the required depth "may change between minor GHC
  releases". Count calls and iterations, which are properties of the program.

## Footguns kept in view

1. The checker evaluates implicitly — whenever conversion needs a normal form —
   so a budget error can arise where no compile-time evaluation was written.
   Limited to closed terms; the error must say which conversion demanded it.
2. The budget counts semantic steps only, so it is stable across compiler
   versions and optimisations.
3. ~~The existing macro-expansion fuel is a separate guard with a separate
  name.~~ Superseded by the extension above: one budget, macro applications
  counted with everything else; *fuel* is its retired name.

## Sources

- [Zig language reference](https://ziglang.org/documentation/master/) —
  "evaluation exceeded 1000 backwards branches", `@setEvalBranchQuota`,
  "comptime call of extern function".
- [GHC User's Guide: type families](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html)
- [Idris 2 totality](https://idris2.readthedocs.io/en/latest/tutorial/typesfuns.html)
