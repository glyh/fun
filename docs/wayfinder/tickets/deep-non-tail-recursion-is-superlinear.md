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

## Findings (2026-09-14)

Benchmark: `do rec t : I64 -> I64 = fn(n) -> if n == 0 do 0 else t(n - 1) + 1 end; t(N) end`,
elaborated once, then timed through `Ctx.run` (`Ctx.eval` on the baseline).
OCaml 5.3.0, default GC settings.

| N | `728f719` (budget) | `ba0edd5` (before the budget) |
|---|---|---|
| 50 000 | 0.26 s | 0.24 s |
| 100 000 | 0.84 s | 0.82 s |
| 200 000 | 3.32 s | 3.28 s |
| 400 000 | 14.40 s | 14.18 s |

**Pre-existing, and quadratic** (×4 per doubling). The budget is not the cause.

**Root cause.** `perf` on N = 200 000: 50 % of cycles in `caml_scan_stack`
(plus `oldify_one`, `caml_find_frame_descr`). Every object-language call adds
native OCaml frames (`eval_result` → `bind_result` → `apply_result` → …), so the
native stack grows with recursion depth. OCaml 5's minor GC scans the *whole*
stack at every minor collection, and a collection happens every
fixed-number-of-words allocated — so total GC work is O(depth × allocations)
= O(N²). Confirmed by the minor heap size: `OCAMLRUNPARAM=s=4M` brings N =
400 000 to 2.07 s, `s=32M` to 1.45 s — a constant factor, still superlinear.

**Why it stays open.** No clean local fix. A larger minor heap
(`Gc.set` in `bin/main.ml`) only divides the constant. The fix is
architectural: object-language calls must not consume native stack — an
explicit continuation/stack machine for `Nbe.eval_result` (it already has
`result`/`bind_result` for effects, which a defunctionalised evaluator would
subsume). That is a decision for the port's evaluator design, not a patch to
the prototype; a port that transliterates `eval` recursively inherits it on any
runtime whose GC scans stacks.
