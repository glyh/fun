---
title: Constructor lookup matches the type name, not the constructor name
parent: ../fun-design-map.md
labels:
  - wayfinder:task
status: open
assignee:
blocked_by:
---

# Constructor lookup matches the type name, not the constructor name

## Question

`Elab_resolve.find_nominal_template_opt`'s env scan compares the candidate's
**type** name against the name being looked up:

```ocaml
| VNominal n when String.equal n.name name -> Some (VNominal n)
```

So resolving a *constructor* by name never succeeds on that path. Promoted from
the design map's fog list ("known deferred bug — nested-module ADT constructor
resolution"), where it was salvaged from an old handover note; confirmed still
live in `lib/semantic/typecheck/elab_resolve.ml`.

## Symptom

`pub pattern PatWild = RawPatWild(_)` inside a module fails: the pattern
synonym's right-hand side names a constructor, the path scan falls through to
`scan_env`, and `scan_env` only matches type names. Affects pattern matching in
macro bodies over module-scoped ADTs.

`Elab_resolve.find_nominal_for_constructor` right above it does the correct
thing (scans `n.constructors`), so the fix is likely to route the fallback
through that rather than to write new logic.

## Why it is a pre-rewrite item

It is a plain semantic bug in resolution, not an OCaml artifact — a port
reproduces it faithfully, and it will look deliberate to whoever transcribes
it.

## Sketch of the work

1. Decide whether `find_nominal_template_opt`'s fallback should match type names
   only, constructor names only, or both with a stated precedence.
2. Reuse `find_nominal_for_constructor` for the constructor half.
3. Regression: a module-scoped ADT with a `pub pattern` synonym over one of its
   constructors.

## Resolution

_Unresolved._
