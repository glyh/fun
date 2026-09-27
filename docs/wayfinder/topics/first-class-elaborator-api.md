# First-class compiler API

A fog-stage direction, broadened: expose as much of the compiler as it is honest
to expose to **downstream users** — macros at compile time, and tools, libraries
and frontends out of process. The elaborator and unifier are the core of it, not
the whole of it.

Recorded here so the ambition and its constraints are written down before anyone
starts widening the interface.

## What the compiler knows, and who might want it

`fun` already has, in one place or another:

| facility | where |
|---|---|
| bidirectional elaboration (`infer` / `check`) | `src/Fun.Compiler/Elaborator*.cs` |
| unification, metavariables, Miller-pattern solving | `src/Fun.Compiler/Unify.cs` |
| normalization by evaluation, conversion, quoting | `src/Fun.Compiler/Nbe.cs` |
| effect-row computation, handler checking | `Elaborator.Effects.cs`, `Nbe.Effects.cs` |
| pattern compilation and exhaustiveness | `Core.Match.cs`, `Nbe.Match.cs` (`Elaborator.Match.cs`) |
| the shared evaluation budget | `src/Fun.Compiler/Budget.cs` |
| reflection over `Expr` / `Decl` / `Pattern` | `src/Fun.Expand/Reflection.cs` |
| driver: source → checked term → value | `src/Fun.Compiler/Driver.cs` |
| the loader and its per-unit caches | `src/Fun.Compiler/Loader.cs` |

The full stretch goal is: a downstream user can hold a `Context`, ask what is in
scope, infer and check terms, unify, solve a goal, normalize a value, compile a
match, and see the diagnostics — at compile time from a macro, and out of process
from a tool. One API serving both, so a language server, a REPL and a macro do
not each re-implement the elaborator.

## Two consumers, one surface

**Compile time.** A macro that can ask the elaboration questions — what is the
expected type here, does `A` unify with `B`, solve this hole — is a tactic.
Lean 4's `Elab` is the reference: a monad whose primitives are exactly those
questions, with errors carried as values.

**Out of process.** `Driver.Elaborate` / `Run` / `Describe` is nearly a public
API already; its own doc comment says "the conformance runner and the CLI are its
only callers". A supported version returns structured results (term, type,
context, diagnostics with spans) instead of throwing `FunException`, and lets a
caller hold a live `Context` and elaborate incrementally rather than one string
at a time.

The reason to want both from one surface is the precedent: Unison's
[UCM](https://www.unison-lang.org/) is an out-of-process codebase manager over a
compiler that is a library, and its editor integration is another client of the
same thing. Two surfaces would drift.

## Where the seam goes

This is an interface-widening decision, not a feature.

**`Fun.Expand` cannot reference `Fun.Compiler`** (`CLAUDE.md`, "The three-project
split is load-bearing"). Expansion is parsing interleaved with macro execution,
and the elaborator is its consumer — that direction is what keeps hygiene from
depending on elaboration. Today exactly one adapter crosses the line:
`IMacroRuntime` (`src/Fun.Expand/MacroRuntime.cs`), whose whole surface is
`LoadSyntax`, `Advance`, `CompileMacro`, `CompileSignature`, `ApplyExpr`,
`ApplyDecls`.

The encouraging half: the *vocabulary* an API speaks in already lives below both
— `Fun.Kernel` holds `Atom`, `Core` (terms, values, patterns, decision trees),
`Syntax`, `ScopeSet`, `SourceSpan`. Only the *operations* need the compiler. So
the shape is a capability interface declared in a layer both can see, implemented
where the elaborator lives, handed to expansion at construction — the pattern
`IMacroRuntime` already sets, extended.

The three ways to widen it, for the ticket to choose between:

1. **Grow `IMacroRuntime`.** Cheapest; the risk is that it is also the hygiene
   boundary, and it should not become a way for a macro to mint scope sets or
   observe metas in a shape that breaks the one hygiene contract.
2. **A capability interface in `Fun.Kernel`, implementation in `Fun.Compiler`.**
   Keeps the operation set explicit and lets out-of-process callers use it too.
3. **A fourth assembly (`Fun.Api`) depending on all three**, used by the CLI,
   the LSP and the REPL. This serves out-of-process consumers cleanly but does
   *not* by itself let a macro reach the elaborator — macros run inside
   `Fun.Expand`, so a compile-time capability still has to cross the boundary
   through (1) or (2).

## Guardrails

- **It is a stability commitment.** Every exposed internal becomes a thing a
  refactor must preserve. The domain-model docs
  ([elaborate ↔ evaluate](core-tt-domain-model.md),
  [surface and enforestation](core-tt-domain-model-surface.md),
  [macros](core-tt-domain-model-macros.md),
  [effects](core-tt-domain-model-effects.md)) are what make it possible to expose
  a *model* rather than whatever the code happens to do today.
- **Errors must be values with spans.** An API that reports failure by throwing
  an exception whose message may or may not name a location is not an API a tool
  can build on. The elaborator's errors still carry no source location (a fog
  item on the map) — that is a prerequisite, not a follow-up.
- **The hygiene contract holds at the boundary.** A macro's output is hygienic
  syntax; an API that hands a macro raw scope sets or unsolved metas has to say
  how the contract is preserved.
- **Budget.** Every question that evaluates is a checker request and spends from
  the one evaluation budget ([macros](core-tt-domain-model-macros.md)).
- **Identity outside a run.** Nominals can be run-time generative
  (`Elaborator.Generative.cs`). An exposed nominal id needs a documented meaning
  for a caller that is not inside the current run.

## Staging

An ordered ticket would look like this — each step usable on its own:

1. Finish reflection (the `Match` ADT and friends) — the cheapest path to
   compile-time program generation, and it needs no boundary crossing.
2. A result-returning driver API with spans on diagnostics, replacing the
   throw-only entry point; the CLI and the conformance runner become its first
   callers.
3. Read-only elaboration queries reachable from macros: current goal type, local
   context, `infer`.
4. Actions: `unify`, `solve`, `check` — the point at which a tactic is possible.
5. Build the LSP, the REPL and the CLI on that one API rather than beside it.
6. An out-of-process/server surface over the same capability — which is where
   this direction meets [content-addressed-codebase](content-addressed-codebase.md).

## Why this is fog, not a ticket

No downstream consumer exists yet. Which subset to expose should be chosen *by* a
consumer — an API guessed in the abstract exposes too much and is expensive to
retract, and the design priority is Consistency > Flexibility, not surface area.

There is also a cheaper competitor worth measuring first: most of the value may
be reachable as a *library over reflection* (the `Expr`/`Decl`/`Pattern` ADTs,
`quote`, type-aware signatures) without crossing the project boundary at all.

**Sharpens when** the first real consumer appears — an editor feature that wants
elaboration, a tool that wants to hold a `Context`, or a `derive`/tactic that
must inspect the goal its expansion is checked against — and "can this be a
library over reflection instead?" has a measured no.
