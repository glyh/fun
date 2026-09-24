---
title: "Port: the runtime-error variants no case covers"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
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
