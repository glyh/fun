---
title: Every elaborator error names the form it was at
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by: []
---

# Every elaborator error names the form it was at

Decided 2026-10-01, from the fog item "diagnostics polish boundary" in
[the map](../fun-design-map.md), whose measurement had said "no consumer needs a position yet". The
user's answer was the tie-breaker: `--file` **is** the consumer — it is how a probe is read — and a
message without a position makes the reader hunt for the form.

## What is already there

The position machinery exists and one path already prints it:

- `Budget._site` (`src/Fun.Compiler/Budget.cs:23`) is a `(string Mode, SourceSpan Span)?` set by
  `Budget.At` and restored on unwind; `Where()` (`:50`) renders it as `" while {Mode} at {Span}"`.
- `Elaborator.At` (`Elaborator.cs:180`) wraps `Infer`/`Check`/`ReadType` and already appends
  `Where()` — but **only** for `EvaluationFailed`:
  `throw new FunException(e.Message + ctx.Metas.Budget.Where() + " (while type checking)")`.
- `BudgetTests.cs:56` asserts the rendered position for the budget error, so the format is pinned.

## The change

`Elaborator.At` appends `Where()` to **every** `FunException` that passes through it, not just
`EvaluationFailed`. The naive version double-appends: an error thrown deep inside nested `At` frames
is caught by the innermost frame, and the rebuilt `FunException` is then caught again by each
enclosing frame, so the suffix would repeat once per nesting level.

The existing code avoids this by type — `EvaluationFailed` converts to `FunException`, which the
enclosing frames do not catch. Keep that trick:

1. Unseal `FunException` (`Driver.cs:6`: `public sealed class FunException(string message) :
   Exception(message);` → drop `sealed`) so the located form can be its subtype.
2. Add a private marker beside `At`, e.g. `private sealed class Located(string message) :
   FunException(message);` — a located error *is* a language error, so the driver, the runner and
   every `catch (FunException)` keep working unchanged.
3. `At`'s body becomes, in this order:
   - `catch (Located) { throw; }` — pass an already-located error through untouched;
   - `catch (EvaluationFailed e)` — unchanged except that it now throws `Located`;
   - `catch (FunException e) when (ctx.Metas.Budget.Where() is { Length: > 0 } where)` —
     `throw new Located(e.Message + where);`

A `FunException` thrown where no `At` frame is active (the reader, the expander, the loader) gains
nothing, which is correct: those have no form to name, and their messages are already pinned
(`ReaderTests.cs:65`, `ExpandTests.cs:104`). Expansion-side positions are a separate question — the
funnel that discards them is `Driver.cs:38-46`, recorded in the map's fog.

## Acceptance

- A program with an unbound variable reports its position through `--file`, e.g.
  `ELAB unbound variable: x while inferring the form at …`.
- The suffix appears **once**, with the **innermost** form's position, however deeply the error is
  nested — prove it with a nested program whose error is two or more `At` frames deep.
- The conformance suite is unchanged (`958 cases, 0 failed`): `.expect` compares a value or the
  literal `error`, never a message.
- The xUnit assertions that compare an exact message are updated **only where the message actually
  changes**, from the real output. The measurement counted nine candidates
  (`PrimitivesTests.cs:46,69,89`, `EffectTests.cs:50`, `PreludeTests.cs:40`,
  `InterleavingTests.cs:19`, `RecTests.cs:57`, `ReaderTests.cs:65`, `ExpandTests.cs:104`), but the
  last two are reader/expander errors and should **not** change — do not append a suffix to an
  assertion before seeing it fail, and say in the commit which ones actually moved.

## Cost

Measured before the ruling: the position already exists; the cheap move is at one catch; the real
cost is the exact-message assertions, not the line.
