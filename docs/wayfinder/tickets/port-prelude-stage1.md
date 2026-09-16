---
title: "Port: prelude stage 1"
parent: port-core-tt-to-dotnet.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Port: prelude stage 1

Wave 3 fork. Follow the porting conventions in
[port-core-tt-to-dotnet](port-core-tt-to-dotnet.md#porting-conventions-2026-09-16).

## Scope

- Elaborate `dotnet/std/stage1.fun` (the port's copy of the prelude's stage 1:
  `Bool`, `if`, `i64_to_bool`, `Option`, `List`, the `Syntax` module with its
  enums, pattern synonyms and builders) as a compilation unit against the base
  context, once per process, and bind it in the base context as `stdlib` (glossary
  **Base context**, **Prelude**: `stdlib` is *bound*, not opened).
- `import "std"` resolves to it; its syntax exports (the `if` form) and members
  reach a program through the open that brings them. A program read as an
  expression is elaborated inside `open (import "std")`, as today's Driver
  assumes: make that open real, through the macro runtime interface.
- **Stage 2 is not ported** (it defines the operators, `Eq` and `type`, and needs
  procedural macros). So a name or role the stage-1 prelude does not supply must
  still be "not ported yet", never "unbound": narrow the Driver's two blanket rules
  ("`x` from the prelude", "prelude syntax roles") to what stage 2 could still
  supply, and keep them honest.
- Whatever stage 1 needs that is not ported yet (e.g. primitives that need the
  macro runtime, `expand_block`/`expand_decls`, only called inside function bodies)
  stays "not ported yet" at run time; stage 1 must still *elaborate*.

## Decided rules to read first

Domain model I5 ("What the base context holds"), glossary **Base context**,
**Prelude**, **Compilation unit**; the port ticket's prelude decision
(`dotnet/std/`); `STATUS.md` "Staged prelude; `type` is a std macro; `export`";
the prototype's `Elab_entry.stage1_ctx` and `Macro_driver.std_syntax` as supporting
material. The prelude's `Syntax` module is what procedural macros reflect over
next: keep its nominals reachable.

## Target

Cases whose only blocker is a stage-1 name or form (`Bool`, `True`/`False`, `if`,
`Option`, `List`, `Some`, `Nil`, `Cons`, …). Most prelude-blocked cases also need
stage 2's operators; report how many turn green and what blocks the rest.
