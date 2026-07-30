---
title: Unify procedural macro call syntax with function calls
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: Implemented in commit 0441c9a — `@` removed, procedural macros invoked with ordinary `f(args)` application syntax; macros promoted into the scope-aware binding table with a Value/Macro kind tag (one namespace, innermost-lexical shadowing), replacing the save/restore hack. Internal MacroCall node is now compiler-derived; the type-aware driver path is unchanged.
closed_date: 2026-07-30
implemented_date: 2026-07-21
blocked_by:
---

# Unify procedural macro call syntax with function calls

## Question

Make **procedural macros** (`macro f(stx) -> …`) callable with ordinary
application syntax `f(args)` — the same as functions — and **remove the `@`
macro-call marker** entirely. Scope: procedural macros only; syntax templates and
operators (`if`, `&&`, `+`) keep their own parse-time syntax and are out of scope.

## Decided design (from a grilling session)

- **Uniform surface.** Drop `@`. `f(args)` invokes a macro when `f`'s binding is a
  macro, and is a normal call otherwise. `f @ (args)` is removed; there is a single
  calling convention. The mid-session rationale: the language already invokes
  templates/operators (`if`, `&&`) with native syntax, so requiring `@` only for
  procedural macros is the odd one out; and `if` refactoring a fn↔macro shouldn't
  churn call sites.
- **(a) Expander derives the marker (not the user).** When the expander meets a
  `Var`-headed application spine `f(a, b, …)` and `f` resolves to a macro binding,
  it reclassifies it into the existing internal `MacroCall` node (gathering the
  curried `Ap` spine back into an arg list). Users write `f(args)` uniformly; the
  `MacroCall` node becomes compiler-derived. **The type-aware interleaving driver
  is untouched** — type-aware macros still leave the derived `MacroCall` node to
  survive lowering into the elaborator exactly as today. Chosen over the
  alternative (elaborator resolves macro-ness at every application head) because it
  is cheaper (a hash probe in the lighter expansion pass, not the hot interleaving
  loop) and lower blast-radius.
- **(i) One namespace, innermost lexical binding wins.** A name resolves to exactly
  one binding whose *kind* (macro or value) decides expand-vs-call; there is no
  "both at once", one shadows the other by scope. This is the Racket model.

## Why (i) is a real refactor, not a flag

The code today is the **rejected** model (separate namespaces):
- Value bindings live in the **scope-aware `binding_table`** with hygienic
  `resolved_name` renaming (`expand.ml` `Let` → `extend_at_fresh`).
- Macros live in a **string-keyed `macro_table`** (`expand.ml:340`,
  `register_macro ~name:name.name` — raw surface name, no `binding_table` entry),
  and shadowing is faked with a **save/restore** around body expansion
  (`expand.ml:347-355`), which is dynamic-scope-ish, not lexical.

Faithful (i) means **promoting macros into the scope-aware `binding_table`**: a
macro definition `extend`s the binding table with a `resolved_name` and a **kind
tag** (`Macro`/`Value`), so name resolution returns one binding and its kind
dispatches. Bonus: macro scoping becomes genuinely **lexical/hygienic**, replacing
the save/restore hack — macros stop being quasi-global-by-string.

## Implementation sketch

- Add a kind tag to the binding info (`Binding` / `Expand_ctx`), so a resolved
  binding says whether it is a macro or a value.
- Macro registration (`MacroDef` at `expand.ml:333-356`, and the struct-level
  `MacroBinding`) `extend`s the `binding_table` with a `resolved_name` + `Macro`
  kind, carrying the macro entry (fn value, nominals, macro kind). Replace the
  save/restore shadowing with ordinary lexical scoping.
- Application head (`expand.ml:260` `Ap (f, e, a)`, currently just recurses): walk
  the left spine to the head; resolve it via binding resolution; if it is a macro
  binding, gather the spine args and run the existing macro-expansion / type-aware
  deferral logic (today under the `MacroCall` case, `expand.ml:360-405`); else keep
  it as application.
- Remove `@` parsing (`enforest.ml` `At`-postfix ~657 producing `Syntax.MacroCall`)
  and the surface `MacroCall`/`MacroCallBinding` *entry points*. Keep the internal
  `MacroCall` Surface/Syntax node (now compiler-derived) so the elaborator interface
  and type-aware path are unchanged.
- Update tests using `@`-call syntax to `f(args)`.

## Risks / watch

- Macro lookup moves from string-keyed to scope-resolved; verify hygiene renaming
  interacts correctly (a macro's `resolved_name` vs its `macro_table`/entry keying).
- Curried-spine gathering must reconstruct the exact arg list the old `MacroCall`
  carried (arity, explicitness).
- Provisional-registration / fuel and mutually-recursive-macro edge cases
  (already deferred in macro STATUS) should not regress.

## Resolution

Implemented in commit `0441c9a` ("Demote Bool/if to library; unify macro call
syntax; chart Stage 11"), exactly per the decided design:

- `@` macro-call marker removed; procedural macros are invoked with ordinary
  application syntax `f(args)`.
- Macros live in the scope-aware `binding_table` with a Value/Macro kind tag —
  one namespace, innermost-lexical shadowing (model (i)), replacing the old
  save/restore shadowing hack. Macro scoping is now genuinely lexical/hygienic.
- The expander derives the internal `MacroCall` node (design (a)); the
  type-aware interleaving driver path is unchanged.
