---
title: Imported modules elaborate in the importer's context
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
status: open
assignee:
blocked_by:
---

# Imported modules elaborate in the importer's context

## Question

An imported `.fun` module is elaborated in the **importing expression's**
elaboration context, not in a fresh base context. Should it be? Today this means
a module sees whatever the importer happened to have in scope — in particular
the prelude, which the top-level entry points open by default.

## Context — what exists today

- `Elab_infer`'s `Import path` case calls
  `Core_loader.load_elaborated loader path ~elaborate:(fun imported _ -> ops.infer ctx imported)`,
  where `ctx` is the context *at the import site*. The imported module's
  bindings therefore resolve against the importer's namespace.
- [Module-level open (strict imported modules)](module-level-open-strict-imported-modules.md)
  made modules strict on the **syntax** side: `Enforest.parse_module` no longer
  has an `?open_prelude` flag, so a module that uses `+` must write
  `open (import "std")`. But the **value** side is unaffected: a module with
  `pub y = Some(1)` and no open still elaborates, because `Some` is in the
  importer's context.
- So the strict phase rule is currently half-enforced for modules: prelude
  *syntax* requires the open, prelude *values* do not.

## Why it is not simply a bug fix

- `Core_loader` caches elaborated modules by resolved path
  (`runtime_elab_cache`). If a module's meaning depended on the importer's
  context, that cache would be wrong; the fact that it is *shared* is an
  argument that modules should elaborate in a context that does not depend on
  the importer at all.
- Elaborating in a bare base context instead would make every module strict —
  which is the consistent answer — but the prelude is currently reachable from a
  module only through the importer's scope for anything the module does not
  itself open. Whether a module in a base context can still resolve
  `import "std"` (it can — the path is reserved and `stdlib` is bound by
  `init_ctx`) and what else the base context must carry (loader, macro table,
  syntax nominals) needs checking before committing.
- Interacts with the deferred **private type visibility** work: what an importer
  can see of a module is the same question from the other side.

## Sketch of the work

1. Decide the intended rule: a module's meaning depends only on its own source
   plus what it imports/opens (the consistent answer), or the current
   inherit-from-importer behaviour is deliberate.
2. If the former: elaborate imported modules from `Elaborate.init_ctx ()`
   (plus loader/macro plumbing) rather than the import-site `ctx`, and audit the
   caches that assume path-keyed identity.
3. Audit and fix the module fixtures that turn out to be relying on the leak —
   they should write their own `open (import "std")`.

## Resolution

_Unresolved._
