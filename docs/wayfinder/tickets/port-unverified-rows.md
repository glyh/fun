---
title: "Port: settle the rows nobody could decide by probing them"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: closed
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

## Verdicts (2026-09-24) — closed

Probed by a fresh fork on `32aa27e` (the `deepseek` run; the earlier attempt died on a
provider rate limit), 5 of 7 rows reached, every verdict re-verified by the integrator in
both runners before a ticket was opened. Line numbers below are as they are at `32aa27e`
— several of the ticket's originals had drifted (the table at the end of this section).

| row | site | verdict | follow-up |
|---|---|---|---|
| 1 — typed operator macro (G4) | `Expander.Macros.cs:306` | **split**: an *untyped* operator macro is **parity** (both runners answer `1`); a **typed** one is neither implementation's feature — the port refuses and the prototype **hangs** (exit 124 at 25 s, no output) | [a type-aware operator macro](port-typed-operator-macro.md) — **ruled 2026-09-24: deferred to after the port**, because parity is "neither has it"; the port keeps its refusal |
| 2 — recursive-enum captures (G5) | `Elaborator.RecTypes.cs:62`, `:132` | **real gap, and a crash**: the prototype accepts, the port throws an **unhandled** `UnifyException` and kills the whole run | [captures from payload values](port-enum-captures-from-payload-values.md) |
| 3 — `Syntax.Stx` | `Elaborator.cs:304` | **parity**: both refuse (the prototype's `elab_infer.ml:1418` is an assertion) | [the conversions](port-probed-row-conversions.md) §1 |
| 4 — `Value` kinds in a meta solution | `Unify.cs:204`, `:222` | **unreachable** — no program put a `VRef`/`VCont`/`VPatternSynonym` into a solution | [the conversions](port-probed-row-conversions.md) §2 |
| 5 — quoting a `VCont` | `Nbe.cs:577` | **unreachable** — no quoting of a continuation was reachable four different ways | [the conversions](port-probed-row-conversions.md) §3 |
| 6 — a synonym over a type-case pattern | `Elaborator.Patterns.cs:75` | **real gap**: the prototype answers `1`, the port refuses. The `:87` half dissolves under the generalization ruling (evidence in the ticket) | [a pattern synonym over a type-case pattern](port-pattern-synonym-over-type-case-rhs.md) |
| 7 — statements in a quoted block | `Enforest.Roles.cs:867` | **real gap**: `effect` and `trait` statements answer `1` in the prototype, refuse in the port; `impl` untested | [quoted block statements](port-quoted-block-statements.md) |

The reachable shape matters for row 7: it is a quoted **block expression**,
`quote( { … } )`. `quote { … }` goes through `ReadItemsNow`/`ParseModuleStatement` and
never reaches `WithBody` — a case written that way proves nothing.

Corrected line numbers at `32aa27e` (the ticket's originals in parentheses):
`Elaborator.RecTypes.cs:62` is the `CompletePending` throw, message on `:63` (`:54`);
`Enforest.Roles.cs:845` is `ReadBlock` and the throw is `WithBody` `:867` (`:845`);
`Nbe.cs:561` is inside `Quote` and the `VCont` catch-all is `:577` (`:561`);
`Unify.cs:204` is the value-kind refusal, `:206` the `VNeutral` case, and the catch-all
is `:222` (`:206`/`:223`). `Elaborator.Patterns.cs:75`/`:87` matched as written.

Resumable transcripts of both probe attempts are under
`/tmp/pi-subagents-1000/home-lyh-pullground-fun/01a0d1e2-8670-75e3-a2ef-72dadaf596b5/tasks/`
(`d0b516c6…` the first, `1abe1ca5…` the second) — convenience only; this section is the
record.
