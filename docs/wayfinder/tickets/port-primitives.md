---
title: "Port: primitives"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
assignee:
blocked_by:
---

# Port: primitives

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- The base context's primitives: one declaration per primitive (name, type,
  evaluation), as decided in
  [unify-primitive-declaration](unify-primitive-declaration.md) — the prototype's
  table is `lib/backend/interp/nbe_prim.ml` (`declarations`) and is bound into the
  base context by `Elab_entry.stage1_ctx`.
- Each primitive is a defined entry whose value is a neutral headed by the
  primitive; applying it to atom arguments reduces (the prototype's
  `try_prim_reduce`), otherwise it stays stuck. I64 arithmetic is **checked**:
  overflow is a genuine error, as the ticket writes down.
- Everything the prelude's two stages call (`dotnet/std/stage1.fun`,
  `stage2.fun`: `eq_string`, `panic`, arithmetic and comparison, char and string
  operations) except the ones that need the macro runtime (`expand_block`,
  `expand_decls`): those raise "not ported yet".
- The Driver's prelude rule keeps holding: a name the base context now has is
  found; a prelude name is still "not ported yet".

## Target

Base-context names reachable from programs, and whatever conformance cases call
primitives directly; mostly this unblocks the prelude slice. xUnit for reduction,
stuck applications and overflow.

## Resolution (2026-09-16)

Merged from `port/primitives` (`5f16665`). `Primitives.cs` is the one table: name,
type, reducer. Checked I64 `+ - * / %` (overflow and division by zero are genuine
errors; `MinValue % -1` is 0), comparisons on I64/Char/Unit/String returning I64 1
or 0, `panic` once its message is a string atom, `Tuple`/`tuple_arity`;
`expand_block`/`expand_decls` are "not ported yet". Base-context entries, so no
width is hard-coded; one hook in `Nbe.ApplyStuck` reduces on atoms. Also fixed: the
unifier had no case for two neutrals (`Unify.Neutrals.cs` compares head then
frames). `elaborate/elab-238` now passes on its genuine error. C# 207/650; xUnit 108.

**Follow-ups:**
- **Runners do not run `error` cases.** `cases/README.md` defines `error` as failing
  "at expansion, elaboration or evaluation", but both runners only elaborate an
  `error` case, so a run-time failure (overflow, division by zero, `panic`) cannot
  be a shared case. The README states the intent; the runners are wrong. Queued as
  a fix to both runners (overflow is pinned in xUnit until then).
- **Deviation:** `panic` whose message is not yet a known string stays stuck ("reduces
  on atoms"); the prototype fails with the literal message `panic`. No case
  exercises it.
