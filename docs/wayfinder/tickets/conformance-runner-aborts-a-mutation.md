---
title: A mutation aborts the conformance run instead of failing cases
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-10-01
resolution: "Closed 2026-10-01. Filed from the wide sweep's own last finding, reproduced and fixed by the `runner-invariant` fork (008720b, merged 6053ea3), re-measured by the integrator: mutation A → `954 cases, 9 failed` exit 1, mutation B → `954 cases, 1 failed` exit 1, restored → `954 cases, 0 failed` exit 0, xUnit 209/209. The throw sites are unreachable by a real program, so the runner was the bug, not the engine."
assignee:
blocked_by: []
---

# A mutation aborts the conformance run instead of failing cases

Filed 2026-10-01 from the last paragraph of
the [redundancy sweep's round two](suite-redundancy-measured.md), which had recorded the
observation without a ticket:

> removing the match non-exhaustiveness check and the rec-group mix check made the suite crash
> rather than report a failing case — the runner has an invariant-failure path for four exception
> types, and something escapes it.

That matters because a mutation run *is* the coverage measurement: a mutation that aborts the run
discards every other case's result, so one unclassified exception type quietly voids the sweep.

## What actually escaped (reproduced 2026-10-01)

Both refusals were located by probing all 231 `error` cases through the `--file` runner, rather than
by trusting the sweep's names:

| mutation | what the removed guard was hiding | the escaping throw |
|---|---|---|
| remove `Elaborator.Match.cs:72` (`non-exhaustive match: {missing} is not matched`) | `MatchCompile.Compile` returns `(null, missing)`, `:75` stores the null tree | `NullReferenceException` at `Nbe.Match.cs:126` — the `default` arm's `$"{tree.GetType().Name}"` NREs before its own `InvalidOperationException` can be built. Nine cases exercise the refusal (`elab-169/170/171/174/177/182/183/184/185`). |
| remove `Elaborator.RecTypes.cs:46-47` (`GroupKind`'s mix refusal) | a `Struct` reaches `Shape`'s hard cast | `InvalidCastException` at `Elaborator.RecTypes.cs:139` (`(Syntax.Enum)value`). `elab-164` reaches it. |

Both throws sit on paths **no real program reaches** — the null tree exists only once the
non-exhaustiveness refusal is gone, and a `Struct` reaches `Shape` only once the group-kind refusal
is gone. So the engine is not the bug; the runner is.

## The bug, by name

`test/Fun.Conformance/Program.cs` had two hand-kept exception lists that had **drifted**:

- the elaboration path rethrew anything outside `{InvalidOperationException, IndexOutOfRangeException,
  ArgumentException, UnifyException}` (after `NotImplementedException`/`FunException` were handled),
  so an unlisted type killed the process;
- the evaluation path's filtered `catch` (missing `IndexOutOfRangeException` and `ArgumentException`)
  let the task fault, and `run.Wait` then threw `AggregateException` out of `Program.cs:86`: exit
  **134**, with every other case's result lost.

## The fix (`008720b`)

One classifier, `CaseJudge.Failure`, used by both paths, plus a catch-all on the evaluation path.
An unexpected type becomes that case's **`hard failure (T): msg`** — the FAIL line names the type,
the message and the case path; the run completes; the exit code is 1. Every previous wording is
preserved, and the `("error", _) => "expected an error"` ordering is kept, so a value can still never
satisfy a case expecting `error`. The rethrow is gone because per-case hard-failure reporting is the
non-laundering behaviour that survives a full run — an abort is strictly weaker, not stricter.

Verified on the merge (`6053ea3`):

- mutation A → `conformance: 954 cases, 9 failed`, exit 1, all nine the `hard failure
  (NullReferenceException)` above;
- mutation B → `conformance: 954 cases, 1 failed`, exit 1, `elaborate/elab-164.fun: hard failure
  (InvalidCastException): Unable to cast object of type 'Struct' to type 'Enum'.`;
- restored → `conformance: 954 cases, 0 failed`, exit 0; `dotnet build` 0 errors; xUnit **209/209**
  (208 unchanged + `ConformanceJudgeTests`, which pins the mapping and needed a
  `Fun.Tests → Fun.Conformance` reference).

## Left behind, deliberately

- **`--file`'s `DescribeFailure` keeps its own list.** It never crashed (it has a catch-all and a
  different probe protocol), and its wording is what probes read, so it was left alone.
- **The sweep's table over-credits mutation B.** `elab-109` is not discriminating under it: with the
  refusal removed it lands on a different `FunException` ("type mismatch: cannot unify `VPi` with
  `VU`"), which still satisfies its `error` expect. Confirmed here — mutation B fails `elab-164`
  alone.
- The engine's two unguarded paths (`Nbe.Match.cs:126`'s format string, `RecTypes.cs:139`'s cast) are
  left as they are: unreachable by a program, and a defensive rewrite would be code with no caller.
