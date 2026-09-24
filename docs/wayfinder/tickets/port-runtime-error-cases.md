---
title: "Port: the runtime-error variants no case covers"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: the runtime-error variants no case covers

Found by the coverage sweep (2026-09-24). The shared suite pins two members of the I64
runtime-error family and none of the rest:

| behaviour | shared case |
|---|---|
| `+` overflow | `values/runtime-i64-overflow` |
| division by zero | `values/runtime-division-by-zero` |
| **multiplication overflow** | none |
| **subtraction overflow** | none |
| **remainder by zero** | none |
| **`min_int / -1`** | none |

The prototype tests the whole family (`test/backend/test_core.ml`'s `check_overflow`
cluster, ~line 2203+), and all of them go through the same checked-arithmetic path in the
port — so parity is *likely* and completely unverified. "Likely" is the reason to write them:
these are one-line cases, and a case is what turns a shared code path into a fact both
runners agree on.

## What to do

Add the four as `values/` cases with `expect` `error` (the shared suite's coarse form; the
*class* of error is not pinned there by design). Probe each in **both** runners before
committing it:

- if both refuse → ordinary `error` cases, nothing added to `prototype-divergences.txt`;
- if the port answers where the prototype errors, or the other way round, say so in your
  report with both outputs rather than committing a case that only one runner passes.

`min_int / -1` is worth checking separately from the other three: it is the one that is
*not* obviously an overflow even though it is one, and `min_int`'s spelling may need the
case's first line to say what it is doing.

## Reading

- `test/conformance/cases/values/runtime-i64-overflow.fun` and
  `runtime-division-by-zero.fun` — the two that exist, for the shape to copy
- `test/backend/test_core.ml` — `check_overflow`
- `dotnet/src/Fun.Compiler/Nbe_prim.cs` (or the primitives table) and the prototype's
  `nbe_prim.ml` for the checked-arithmetic declarations

## Resolution (2026-09-25) — closed

All four cases landed, each probed in both runners first, and **both runners agree on every
one** — so they are ordinary `error` cases and `prototype-divergences.txt` is untouched at 27:

| case | OCaml | port |
|---|---|---|
| `runtime-multiplication-overflow` | `integer overflow in *` | `integer overflow in *` |
| `runtime-subtraction-overflow` | `integer overflow in -` | `integer overflow in -` |
| `runtime-remainder-by-zero` | `division by zero` | `division by zero` |
| `runtime-min-int-div-neg-one` | `integer overflow in /` | `integer overflow in /` |

`min_int / -1` is the one whose case says so in its first line: it is an overflow that does
not look like one, and the prototype calls it `integer overflow in /` rather than division by
zero — worth having in writing, since the obvious guess is the other answer.

**Verified by the integrator after merging:** conformance **736 → 740, 0 failed** in both
runners; xUnit 182 (this ticket adds no xUnit); `dune test` green; divergences 27.
