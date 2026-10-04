---
title: "The conformance runner does not timebox elaboration, so a hang cannot fail it"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
closed_date: 2026-09-27
resolution: Closed 2026-09-27 - implemented by a fork (00fa8f7, merged) and verified by the integrator by reintroducing a non-advancing step in a scratch worktree. RunCase and RunFile now bound Driver.Elaborate with Task.Run + Wait(60s); a timeout is a named failure, never a pass, and never satisfies a case expecting error. Constructed evidence: the suite terminated by itself in 128s with two named FAILs and `conformance: 776 cases, 2 failed` - the two error-expecting reader cases failed rather than passing - and --file printed HANG at 60s and exited 0. Reverted, suite 776 cases 0 failed, xUnit 185/185.
assignee:
blocked_by:
---

# The conformance runner does not timebox elaboration, so a hang cannot fail it

Reported by the fork that fixed [the reader's non-advancing statement step](port-reader-loops-on-struct-field-comma.md)
(2026-09-25) as an open question in its report, and filed here because it is the *other half* of
that guard: the fix removes the loops that exist, and this is what would catch one that does not.

## The gap

`dotnet/test/Quill.Conformance/Program.cs`'s `RunCase` and `RunFile` both call
`Driver.Elaborate(...)` with **no time bound**; only `Driver.Run` is timeboxed. So a regression in
the reader (or anywhere in elaboration) does not make the suite print `FAIL` — the suite simply
**never returns**. The only thing that fails it is the `timeout 300` in the documented command:

```sh
cd dotnet && timeout 300 dotnet run --project test/Quill.Conformance --no-build | tail -1
```

Run it without that wrapper — which is the obvious thing to do when debugging a *single* case —
and you get a wedged process and no message.

## Why this is worth fixing now

- **The failure mode is the bug.** `port-reader-loops-on-struct-field-comma.md` was a hang: no
  output, no position, 100 % CPU. Its four new cases have `.expect error`, so they fail only if
  something turns "never finishes" into "fails" — today that is an external `timeout` the runner
  knows nothing about.
- **The port is the only implementation**, so "the suite hangs" now has no second runner to
  compare against and no owner who would notice from the other side.
- The compiler is not the place for this: `Driver.Run` already demonstrates the shape (a time
  bound at the driver's edge), and this is the same thing one layer up, in the test runner.

## What to do

1. **Bound elaboration in the runner** and report it as a failure with a name, e.g.
   `FAIL <area>/<name>: elaboration did not finish in 60s` — the same way a timeout is reported
   for a run. Generous by default so a loaded machine does not flake the suite; the bound is a
   hang detector, not a performance test.
2. **Keep the honest accounting**: a timeout is a failure, never a pass, and never satisfies a
   case whose `.expect` is `error` (the reader cases above would otherwise be satisfied by *any*
   non-termination, which is exactly backwards).
3. **Say what it covers**: per case, and covering both the whole-suite walk and `--file`, since
   the single-program probe is how a suspected bug is checked *before* a case exists.
4. **No new conformance case can test this** (a hang is not a program result), so the evidence is
   by construction: temporarily reintroduce a non-advancing step in the reader, watch the runner
   fail with the message and a path rather than wedging, then revert. Say in the report that you
   did that, with the message pasted.

## Closed 2026-09-27 — the suite can no longer wedge

A fork (`wright`, `00fa8f7`, merged) bounded `Driver.Elaborate` in both entry points with the same
shape the run already had one layer up — `Task.Run` + `Wait(60s)`, per case, `--file` included. A
timeout returns `elaboration did not finish within 60s` as the case's failure; `--file` prints
`HANG elaboration did not finish within 60s`. Exception accounting is unchanged: an unported path
still fails a case and never passes an `error` one, a `FunException` still passes only an `error`
case, and an invariant failure is still reported as this case's failure rather than crashing the
run.

**Verified by construction, by the integrator, in a throwaway worktree** — not taken on the fork's
word. `RequireAdvance` (`src/Quill.Expand/Enforest.cs:576`) was made to never fire, and:

```
FAIL elaborate/struct-field-comma.qll: elaboration did not finish within 60s
FAIL elaborate/struct-field-trailing-comma.qll: elaboration did not finish within 60s
conformance: 776 cases, 2 failed
```

It terminated **by itself** in 128s rather than wedging, and both cases failed — which is the
point: their `.expect` is `error`, and a timeout must not satisfy one. `--file` on the same program
printed `HANG elaboration did not finish within 60s` at 60s and exited 0 (`--file` exits 0 for every
diagnostic, as before). The reader change was reverted; the worktree and its branch are gone, and
the suite is 776 cases 0 failed with xUnit 185/185.

**Known ceiling, kept:** the timed-out elaboration thread spins until process exit — the same thing
the run timeout already accepts, and now noted in the code. This is a hang detector, not a
preemption: it makes a hang *visible and named*, which is all the ticket asked for.

## Reading

- `dotnet/test/Quill.Conformance/Program.cs` — `RunCase`, `RunFile`, and the `Driver.Run` call that
  already has the shape to copy
- [the reader loop](port-reader-loops-on-struct-field-comma.md) — the bug whose signature is a
  hang, and the four cases that depend on termination being enforced by something
