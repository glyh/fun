---
title: Path heads, traits and effects resolve without spelling
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Path heads, traits and effects resolve without spelling

## Question

Finish M12 ("no name is found by its spelling alone",
[macro model](../topics/core-tt-domain-model-macros.md)). Bare-name
expressions are done: expansion resolves each to a binder's resolved name or an
**open choice**, and the elaborator never looks one up by spelling among the
locals. The rest of the tier is still spelling-based:

- **Path heads**: `M.x`, qualified pattern heads `M.Ctor(..)`, record-pattern
  types, `perform E.op`, `impl M.Trait(..)`. `Expand.expand_path` resolves the
  head to a binder but produces no open choice. The elaborator splits the
  `Syntax.path` back into strings (`Syntax.path_split`) and locates the head
  with `Ctx.lookup` / `resolve_path_value*`.
- **Trait names**: `ctx.traits` is a `NameMap` keyed by written name
  (`lookup_trait`, the `Trait.method` case in `elab_infer`, trait-bound sugar
  via `Syntax.written_name`).
- **Unqualified pattern heads and nominals**: `find_nominal_for_constructor` and
  `find_nominal_template_opt` scan the environment for a `VNominal` by name.
- **Effect names**: resolved through the same path helpers.

About 27 lookup sites (`grep Ctx.lookup\|resolve_path_value\|lookup_trait`).

## Direction

Give a path head the same resolution a bare name has: expansion produces the
binder's resolved name or an open choice, and the elaborator locates the head
through `Ctx.lookup_choice` before following members. Traits and nominals are
then located through the context entry the head resolved to, not through a
name-keyed side table or an environment scan. Since delete-surface-ir the
elaborator reads `Syntax.path` directly, so the head can carry that choice.

## Found by

The open-choices change (2026-09-14): `do x = 1; x__0 end` and an open member
spelled like a generated name were fixed for bare names only.
