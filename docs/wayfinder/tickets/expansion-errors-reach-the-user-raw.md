---
title: Expansion budget errors reach the user raw, and body errors lost the operator span
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: closed
assignee: glyh
resolution: A macro application installs its own error on the evaluation budget (Eval_budget.t.exceeded, restored when it returns), so an overrun inside it raises Expand_error BudgetExceeded naming the innermost macro, with the syntax operator's site when there is one - raised with its site, not re-caught. The driver's direct elaboration (macro-body compilation and per-binding advancement) now reports through Elab_entry.reporting_budget, the checker's existing translation point. Left - evaluator errors (EvalError from panic, field-not-found, division by zero) raised inside a macro body still carry no application site; attaching it as data needs the raise sites in Nbe to read the budget's current application.
closed_date: 2026-09-14
blocked_by:
---

# Expansion budget errors reach the user raw, and body errors lost the operator span

## Defect

After [macro-fuel-is-the-evaluation-budget](macro-fuel-is-the-evaluation-budget.md):

- A budget overrun during *expansion* (outside `Elab_entry`) escapes as the
  raw `Eval_budget.Exceeded` rather than an `Expand_error` / `ElabError`
  value. Only overruns inside `Elab_entry.on_expr*` are translated.
- `with_syntax_operator_context`, a catch-all that re-wrapped any exception
  from a macro body as a string prefixed with the operator's use and
  declaration spans, was deleted (it was exceptions as control flow). An error
  raised inside a macro body now reports without the operator's spans.

## Direction

One translation point for `Eval_budget.Exceeded` at the expander's entry
(the loader / `Parse_expand` driver), producing `Expand_error` with its
`site`. For body errors, carry the application `site` as data on the
evaluation request (the budget request already scopes one application) so
the error attaches it where it is raised, rather than re-catching.

## Found by

The macro-fuel retirement (2026-09-14).
