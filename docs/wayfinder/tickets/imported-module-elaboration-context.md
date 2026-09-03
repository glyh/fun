---
title: Imported modules elaborate in the importer's context
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
  - severity:soundness
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

## It is a crash, not only a leak

Importing the same module **twice** crashes whenever the module's body mentions
any name it did not bind itself:

```
("m", "pub v = Some(1)")
do A = import "m"; B = import "m"; match B.v do Some(k) -> k | None -> 0 end end
  => EvalError("bd mask length mismatch")

("m", "open (import \"std\")\npub v = Some(1)")   (* strict module, writes its own open *)
do A = import "m"; B = import "m"; ... end
  => EvalError("open of non-module")
```

Three facts compose into it:

1. the module elaborates in the *importer's* context (above);
2. so any name it did not bind itself — a leaked prelude value, or the `Var`
   that `import "std"` itself elaborates to — becomes a **free variable** whose
   index is relative to that context;
3. `Core_loader.runtime_elab_cache` is keyed by resolved path, so the second
   import splices that same core term in at a different binder depth.

Controls that isolate it: two *distinct* files with identical content at those
same depths work; a self-contained module (`pub type T = C(I64); pub v = C(1)`)
at differing depths works; a closed module (`pub x = 21`) works. Only a
path-cached term with free variables breaks.

Note that writing `open (import "std")` does **not** avoid it — `import "std"`
is itself a `Var` into the importer's context. Strict modules are equally
affected.

## Why the test suite never caught it

Every existing repeated-import test uses a closed module (`pub x = 21`). Worth
adding a non-closed double-import case regardless of how this is resolved.

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
