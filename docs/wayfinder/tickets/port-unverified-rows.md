---
title: "Port: settle the rows nobody could decide by probing them"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: settle the undecided rows by probing them

[The audit](port-unported-path-audit.md) left rows it could not classify, and two later
forks added two more: sites whose reachability **no program has been shown to reach**, so
nobody can say whether the port is refusing a form the prototype accepts (a real gap),
refusing something the prototype refuses too (parity), or guarding a path that cannot
occur (a lying catch-all worth deleting).

This is a **probing** ticket: the work is to construct the program, or to establish that
none exists. It needs no design until a probe succeeds — and a successful probe inherits
whatever ruling the form then needs.

## The rule for this ticket

For each row: build the smallest program that reaches the site.
- **It fails in C# and passes in the prototype** → a real gap. Port it (and get a ruling
  first if the *reason* the prototype accepts it is a semantic choice the domain model
  does not already answer).
- **It fails in both** → parity: the refusal must become the same `FunException` the
  prototype gives (see [the parity conversions](port-parity-conversions.md) for the
  pattern, and note the throw-kind lesson there).
- **No program reaches it after an honest attempt** → say so, with what you tried, and
  make it an assertion rather than a "not ported yet" — a catch-all that claims a path
  is unported when it is unreachable misleads every future reader.

Do **not** resolve a row by reading the prototype's code and inferring. Two rows in this
family were already resolved that way and both inferences were wrong (the dotted
order-group path was called parity and is a real gap; `elab-062`'s stated cause was not
its cause). Probe.

## The rows

**Added by the latent gaps fork** — it reported these rather than guessing:

1. **G4, a type-aware operator macro** (`dotnet/src/Fun.Expand/Expander.Macros.cs:306`).
   It refuses an operator entry whose `Signature is not null`, which the prototype's
   `syntax_operator_arg` admits. The prelude in `dotnet/std/stage2.fun` declares
   operators, so check whether a *typed* operator macro exists in the prototype at all
   before porting — if only the untyped form is exercised anywhere, this may be parity.
2. **G5, recursive-enum captures predicted by name**
   (`dotnet/src/Fun.Compiler/Elaborator.RecTypes.cs:62`). **Checked and not subsumed by
   the landed E11 work**: `PredictCaptures` still predicts from `NamedLevels` alone while
   `InferEnum` adds payload `FreeLevels`, so a name-based prediction sits beside a
   use-based one — which both over- and under-captures. But a block-local probe passed in
   *both* runners, so reachability is unverified. Find the program or close it.

**Left undecided by the audit** (its "Undecided — needs the user" list, minus the two
since settled: the parametric nominal in a generative module, and the neutral
sub-occurrence, which the user ruled on — [the match waits](port-stuck-match-sub-occurrence.md)):

3. `dotnet/src/Fun.Compiler/Elaborator.cs:304` — `Syntax.Stx`, the typed-macro-argument
   marker the expander leaves for the elaborator, has no `Infer` case. Program shape: a
   typed macro whose argument is placed in its output. Note the port elaborates a typed
   argument twice (a `ponytail:` stopgap), so the marker may indicate a real hole rather
   than an unreachable one.
4. `dotnet/src/Fun.Compiler/Unify.cs:206` — which `Value` kinds may appear in a meta
   solution. `Rename` (now complete, see [the latent gaps](port-latent-form-gaps.md))
   hands this its inputs, so start from what `Rename` can produce.
5. `dotnet/src/Fun.Compiler/Nbe.cs:561` — can `VCont` be read back as a term? A
   continuation escapes a handler's body only through `resume`; relate it to the E6 rule
   on [handlers tunnelling callback effects](handlers-tunnel-callback-effects.md) (closed).
6. ~~**The pattern-synonym cluster**~~ — **the product/binder half is settled (user
   ruling, 2026-09-21): do not re-probe it.** A synonym is *checked* at its declaration,
   and the types it cannot determine are *generalized* like a generic function's,
   instantiated at the use — so `pattern Two(a, b) = (a, b)` and `pattern Id(x) = x`
   are legal; the work is
   [a pattern synonym is checked, and generalizes where its type is unknown](port-pattern-synonym-generalizes.md),
   and the prototype's `ElabError(TupleLengthMismatch)` (product) and
   `ElabError(NotANominalType)` (bare binder) are prototype defects. **Evidence already
   gathered — do not repeat it:** a zero-parameter type-case synonym
   (`pub pattern IsI64 = I64`) works in **both** runners, so a type-case right-hand side
   is not part of this gap.
   **Still yours to probe** (current lines in `dotnet/src/Fun.Compiler/Elaborator.Patterns.cs`):
   `:75` `NeedsDirectMatch` — a right-hand side that needs the direct-match machinery;
   and `:87` — a parameter type that stays a meta, which under the ruling is *the thing
   that gets generalized*, so it should dissolve rather than need a verdict of its own.
7. `dotnet/src/Fun.Expand/Enforest.Roles.cs:845` — which quoted-syntax statements take a
   body (`WithBody`). Program shape: `quote { … }` containing a statement other than
   `let`/`rec`/`open`/`syntax`/`macro`.

## Report

Per row: the program (or what you tried and why you stopped), the verdict, and what the
verdict implies. Two or three rows settled is a fine outcome; a row resolved by
inference is not.

## Paused (2026-09-24) — resume here

The probing fork was killed by a provider usage limit (resets 2026-09-24 18:33:48)
after ~94 tool calls, mid-report. It left **no branch**, which is correct: it added no
case files, so its worktree was clean.

What did reach the integrator before it died, as fragments — enough to shorten the next
run, not enough to settle any row:

- **Row 1 (G4, a typed operator macro): a probe came back `1` in both runners →
  *parity, not a gap*** (its words: "Both runners give `1` on r1 — parity, not a gap
  (surprising)"). A variant it called `r1b` "**throws** the gap", i.e. a related shape
  *does* reach the refusal, so row 1 may split into a parity row plus a narrower real
  gap. Both need re-probing to be written down.
- **Row 3 (`Syntax.Stx`, the typed-macro-argument marker): its prototype counterpart is
  an assertion** at `lib/semantic/typecheck/elab_infer.ml:1418` — so the prototype is not
  quietly handling it either; the fork was about to check whether the prelude exposes an
  `stx` builder a macro could use to inject one.
- **Rows 2, 4, 5, the row-6 remainder and 7: nothing recorded.**

Resume with `resume: "unverified-rows"` (its context holds the probes and their output).
Its full transcript is at
`/tmp/pi-subagents-1000/home-lyh-pullground-fun/01a0d1e2-8670-75e3-a2ef-72dadaf596b5/tasks/d0b516c6-c060-43f.output`
— under `/tmp`, so a fresh fork should be told this section rather than the path. If it
is respawned rather than resumed, give it the corrected row-6 wording above and the two
lines (site numbers, not the older `:72/:76/:88`) already fixed in this file.
