---
title: Templates desugar to macros
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Templates desugar to macros

## Decision (macro model M9, M10)

A template is sugar for a macro. It keeps one job, the parse (patterns,
fixity, what each hole captures); the rest is a macro whose parameters are the
captures and whose body is the replacement as quoted syntax, parsed where it is
written. Hole kinds are the reflection types (`Expr`, `Pattern`, `Decl`,
`Id`), so `binder`/`ident` collapse into `Id` and pattern position gains a
kind.

## Today

Templates instantiate during enforestation, from a `Raw_syntax.t list`
replacement re-parsed at every use site (`Enforest_template`). Correctness no
longer depends on this ticket: template literals resolve at the definition
(template-literals-resolve-at-use-site, closed), through fresh resolved names,
region-aware scope addition for template-introduced ids (`Expand.add_id_scope_if`),
and the defining unit's open as a candidate. What remains is structural: one
instantiation path instead of two, with parse-at-definition for replacements.

## Notes

- The region rule in `Expand.add_id_scope_if` exists only because templates
  instantiate before scopes are added. Once they instantiate during expansion,
  it goes and a binder's scope reaches its whole body.
- Nested template declarations inside a replacement, `multi` declaration
  templates, and inherited captures all ride on token-level rewriting today, and
  must be reconsidered under parse-at-definition.
- Likely sequenced with, or after,
  [template-heads-resolve-by-scope-set](template-heads-resolve-by-scope-set.md).
