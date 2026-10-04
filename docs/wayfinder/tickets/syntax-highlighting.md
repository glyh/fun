---
title: Syntax highlighting for quill
parent: ../quill-design-map.md
labels:
  - wayfinder:fog
status: open
assignee:
blocked_by: []
---

# Syntax highlighting for quill

## Desire

The user wants to see proper syntax highlighting for `quill` source soon. This is a
visibility/ergonomics goal, not a language-semantics one — but the surface makes it
unusually hard.

## Why treesitter is a doubt

Tree-sitter is the default choice for editor highlighting, and it is a **context-free
parser**. `quill`'s surface is not context-free in the ways that matter for
highlighting:

1. **Enforestation.** Parsing is interleaved with macro expansion. The token stream
   is not the syntax tree — a macro application produces syntax that was never
   written. A CF parser over the raw text sees only the macro call, not its output.
2. **Operator demotion.** Operators are not keywords; they are `pub infix` /
   `pub prefix` declarations in `std`. Whether `+` is an operator or a variable
   depends on whether `std` is opened at that point. A static grammar cannot know.
3. **Scope-aware binding.** A name's role (operator, variable, type, constructor)
   is resolved through the binding table, not by position or spelling. The same
   identifier can be an operator in one unit and a variable in another.
4. **Hygiene.** Macro-generated syntax carries scope sets that affect resolution but
   are invisible in the source text.

The result: a hand-written or generated CF grammar will **always** mis-colour some
programs, and the mis-colourings will be exactly the interesting ones (operators,
macro uses, generated code).

## What the existing analysis says

`docs/ideas/tooling-and-diagnostics.md` §"The highlighting grammar is a second
artefact, and for an enforested surface it must be a query" covers this in detail.
Its conclusion: a correct highlighter has to *elaborate bindings per unit*, which
makes it a client of the first-class compiler API — not a standalone grammar.

## Why this is fog, not a ticket

- The first-class compiler API is itself a fog item on the map. A correct
  highlighter depends on it.
- The surface syntax may still change (the "surface syntax after the macro model
  settles" fog item). A grammar written today may need rewriting.
- No editor integration exists yet — no LSP, no CLI --highlight mode, nothing that
  would consume a highlighter. The consumer is hypothetical.
- The cheapest useful thing (a TextMate grammar for the common case, accepting
  that operators and macro uses will be wrong) is a few hours of work and drifts
  immediately. Whether that trade-off is worth it is a user decision, not a
  technical one.

## What would sharpen it

- A decision on whether "good enough now" (a static grammar that handles keywords,
  delimiters, and comments but not operators or macros) is worth shipping, or
  whether to wait for the first-class API.
- A decision on the consumer: editor extension? LSP? CLI pretty-printer? Each has
  a different cost and a different correctness ceiling.
- The first-class API fog item moving to a ticket — a highlighter is one of its
  natural first consumers.
