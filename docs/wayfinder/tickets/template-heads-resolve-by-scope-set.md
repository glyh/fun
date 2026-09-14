---
title: Template and operator heads resolve by scope set
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
decided: 2026-09-14 (grilled)
assignee:
blocked_by:
---

# Template and operator heads resolve by scope set

## Decision (macro model M7, pass one I4c; revised by grilling 2026-09-14)

Fixity, precedence and "this name is a syntax form" are a binder's
**syntactic role**, resolved by scope set. ~~A later binder of the same name
takes the role away~~ — superseded: **a syntactic role never mixes with any
other binder of its name** (see below).

## Grilled decisions (2026-09-14)

1. **Arrangement: the expander drives the enforester (Honu).** Raw tokens
   carry scope sets. A definition context (unit, module, block) is enforested
   one form at a time; each form is expanded — its binders, roles and macro
   output registered — before the next form's tokens are enforested. Definition
   contexts are sequential (a module cannot forward-reference), so there is no
   two-pass partial expansion. This is what lets generated syntax reach later
   forms once templates are macros (M9 blocker 1).
2. **Every binder's region is reader-delimited.** Bodies are brace groups; see
   [surface-syntax-braces](surface-syntax-braces.md). No add-parse-strip
   mechanism, and no macro can move where a body ends.
3. **Mixing is an error.** A name with a syntactic role (macro, template,
   operator) cannot also be bound as anything else where both are visible:
   ```fun
   syntax answer { | answer => 42 }
   do { answer = 7; 1 }       # error at the binder, even if unused
   fn(answer) { 1 }           # error
   open M;                    # error at the open once M's members are known (elaboration)
   ```
   - **Fixity attaches, it does not bind.** `pub not = fn…; pub prefix (not) 30`
     is one binding with a role; `(not)` is the value, `not x` the prefix form.
   - **Reported at the binder**, not at a later use.
   - **Decided by scope set, not spelling.** A macro-introduced binder carrying
     its own intro scope does not conflict (hygiene, M12).
4. **Syntax shadows syntax** by scope set: an inner or later `syntax` / `infix`
   of the same name wins ("7I generated syntax later-wins" keeps passing).
5. **A captured `{…}` block is captured unparsed** (`$b:block`) and enforested
   only where the output places it — embedded DSLs can take raw tokens.
6. **Generated syntax is hygienic, exactly like values** (decided 2026-09-14,
   during implementation). A `syntax` / `infix` / `prefix` declaration a
   template or macro writes names its role with an id carrying the
   application's intro scope, so user code cannot see it. To generate callable
   syntax the name comes from the use site — a captured id, which may name a
   generated declaration and head its rules:
   ```fun
   syntax make_inc { | make_inc $(n: ident) => multi { syntax $n { | $n $x => $x + 1 } } };
   make_inc inc;
   pub result = inc 5          # 6
   ```
   or a macro building an `Id` with a received id's scopes (M11). **Grill Q1's
   example 3 was wrong as stated:** it showed `make_inc; inc 5` working with a
   name written inside the template, which decision 3's hygiene note (and M2)
   rule out.

## Rejected

- Enforester-tracked binders without scopes (misses macro output and M9).
- Add-scope / parse / strip over undelimited bodies, and binding declarations in
  template patterns (`$body:expr in $x`) — unnecessary once bodies are groups.
- `do … end` / `fn … end` keyword groups (Elixir-like) — replaced by braces.
- An open's value silently taking a role away, or never taking it away.

## Today

`Binding.find_operator` is string-keyed and newest-wins, because enforestation
runs before expansion and no scope sets exist yet. Its comment defers
scope-keyed resolution to "the interleaving driver". The prefix-template and
operator paths in `enforest.ml` consult it by written name.

## Notes

This needs scopes at parse time, which is the fixed-passes defect the surface
pass named (enforestation entirely before expansion). It likely needs Honu's
arrangement: body forms are enforested lazily as expansion reaches them, after
the enclosing binders have added their scopes. Grilled: see above.

Sequencing: lands with, or after, [surface-syntax-braces](surface-syntax-braces.md)
(the reader groups must exist before enforestation can be driven form by form).
