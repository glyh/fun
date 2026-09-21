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
6. **The pattern-synonym cluster** — `Elaborator.Patterns.cs:72` (a synonym whose RHS is
   a product pattern), `:76` (a synonym over a type-case pattern), `:88` (a synonym whose
   parameter types are not fixed). **Known result: the prototype rejects the product case
   too**, at elaboration, with `ElabError(TupleLengthMismatch)` with no use in the program
   (re-probed by the integrator 2026-09-20). That refusal looks like a defect rather than
   a decision — a synonym `pattern P(a, b) = (a, b)` reads like a legal tuple pattern — so
   **ask for a ruling before treating `TupleLengthMismatch` as the answer here**, and
   probe the other two the same way.
7. `dotnet/src/Fun.Expand/Enforest.Roles.cs:845` — which quoted-syntax statements take a
   body (`WithBody`). Program shape: `quote { … }` containing a statement other than
   `let`/`rec`/`open`/`syntax`/`macro`.

## Report

Per row: the program (or what you tried and why you stopped), the verdict, and what the
verdict implies. Two or three rows settled is a fine outcome; a row resolved by
inference is not.
