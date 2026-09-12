---
title: Imported modules elaborate in the importer's context
parent: ../fun-design-map.md
labels:
  - wayfinder:grilling
  - severity:soundness
status: closed
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

**Rule decided; implementation not started.** Settled while building the
[domain model](../topics/core-tt-domain-model.md), which named the concepts this
ticket needed.

**The rule.** A compilation unit's meaning depends only on its own source plus
what it imports and opens. It elaborates against a base scope, not the
importer's. One rule for values, operators, macros and syntax — today only syntax
obeys it.

**The base scope** holds the atom types, the primitives, and `stdlib` *bound as a
name, not opened*. So `pub v = stdlib.Some(1)` needs no open (it already works
today — `init_ctx` binds `stdlib`), while bare `Some(1)` and `1 + 2` require the
unit to write `open (import "std")`. That keeps
[module-level-open](module-level-open-strict-imported-modules.md) intact rather
than restoring the blanket prelude open it deleted.

**The invariant is base-anchored, not closed.** A unit's term keeps free indices
— `import "std"` stays a `Var` — and is safe only because every importer shares
the same base. Consequence worth writing down: the base scope's width is
implicitly part of every cached term. Safe today because the cache lives in a
per-run loader and is never persisted; a port that persisted it would have to key
on the base.

### Correction to this ticket

The leak is wider than "the prelude". A unit resolves **any** name the importer
happened to bind:

```
importer:  do outer_val = 9; U = import "u"; … end
unit u:    pub v = outer_val          -- resolves today
```

A unit's meaning currently depends on the importer's choice of local variable
names. Reconfirmed against the current tree, along with the double-import crash
and its controls.

### Remaining work

1. Elaborate imported units from `Elaborate.init_ctx ()` plus loader/macro
   plumbing, rather than the import-site context.
2. Make macros strict, matching operators. **Measured, and worse than the value
   leak**: a bare `import` (no open) injects a unit's public macros into the
   importer's namespace, and two units exporting the same macro name overwrite
   each other silently, last import wins.

   ```
   import m1; import m2   ->  answer(0) = 2
   import m2; import m1   ->  answer(0) = 1
   ```

   Cause: expression-level macros are keyed by a freshly uniquified *resolved*
   name, so the elaborator's flat `macro_table` is sound for them; module-level
   macros register with `~resolved_name:binding_name`, the **written** name
   (`expand.ml`, the `extend_at_kinded` call), and so share one unscoped
   string-keyed table across every imported unit. Hygiene holds within a unit and
   is absent between units.

   Sharper still: macros resolve on a *different axis* from every other name.
   `open (import "m1"); answer(0)` does **not** work, and `M.answer(0)` does not
   either. Binding the import is the only form that delivers a macro, and it does
   so as a side effect — `M` is never used.

   **Decided** (see [core-tt-domain-model](../topics/core-tt-domain-model.md)
   I4d): macros become members. `M.answer(0)` expands; `M.answer` stays an error
   since a macro is not a runtime value; macros arrive bare through `open`; and
   binding an import stops injecting them. No new naming mechanism needed —
   unlike impls, a macro call is written at the use site, so qualification gives
   disambiguation for free.
3. Audit fixtures relying on the leak — they write their own
   `open (import "std")`.
4. Add the non-closed double-import regression this ticket notes is missing.

## Closed

All four remaining items are built.

**A unit elaborates against the base context.** `Elab_ctx.Ctx` carries the base
it grew out of, set once by `init_ctx`; the import site elaborates the unit
against that, with the live loader, macro table and expander state, sharing the
meta context. A unit no longer sees the importer's locals, and bare prelude names
now need the unit's own `open`. Reaching the prelude qualified through `stdlib`
still needs no open, as decided.

**The double-import crash is gone, by not transporting the term at all.** A unit's
term is base-anchored and was being spliced into the importer at a different
binder depth. The new `Core.Imported` carries the unit's *value* to the import
site instead; a value carries its own environment, so moving it is always sound.
Both repeated-import cases the ticket records now pass, and so does the differing-
depth variant.

**Macros are members.** A unit's macros are filed per unit rather than in one flat
string-keyed table, so two units exporting the same macro name no longer
overwrite each other and the answer no longer depends on import order. A bare
`import` injects nothing; `M.answer(0)` expands through a bound import; `open`
delivers them bare, as ordinary scope-aware bindings, so an open inside a `do`
block does not leak and a local name shadows normally. `M.answer` on its own
stays an error.

Operators are the deliberate exception: a syntactic role is keyed by written name
a phase before any binding exists. An operator's body is therefore not resolved
through the binding table - it is taken from the unit whose declaration won the
fixity, which the use node now carries.

**One fixture was relying on the leak** and now writes its own
`open (import "std")`. That was the whole audit.

## Found and fixed while closing this

**No macro call inside a `.fun` unit expanded, its own macros included.** Not a
regression - pristine `HEAD` behaved the same - and not something any of the
fifteen rules named, but it made "macros are members" untestable one file down.

Every unit was expanded twice. `Macro_driver.visit_macros` ran the full driver on
it, which interleaves expansion and elaboration and so does compile the macros
the file defines; it kept only `macro_exports` and discarded `output.surface`.
The loader then re-parsed and re-expanded the same file through
`parse_module_with_ctx` with no `elaborate` callback, so `MacroBinding` took its
inert branch and compiled nothing - and *that* surface was the one elaborated.
A unit could hand its macros out but could not use them.

The loader now caches and reuses the driver's surface, which also removes a
redundant parse and expansion per imported unit.

Two smaller gaps went with it: a module-level `I = import "m"` did not register
as a unit handle (only the expression-level `Let` did), and a unit-valued member
was not itself a handle, so `M.I.answer(0)` did not resolve. Both are covered by
regressions now.

## Operator bodies and fixity had separate resolution

Found while auditing the above, and worse than the macro-name collision it was
next to.

`find_operator` resolves fixity **last-wins**, deliberately: `add_operator`
prepends so a user operator overrides a builtin. The duplicate-export check runs
per unit, over one file's export list, so two units each exporting `~` never trip
it. The body, meanwhile, was found by scanning loaded units for the written name,
which neither import order nor definition order decided - the unit path's hash
did. Measured: swapping the two units' return values flipped the answer while the
paths stayed put, and renaming unrelated paths changed the winner again.

So a program could take one unit's precedence and the other unit's body, silently.
With `opa` and `opb` both exporting `~`, importing b then a gave opa's fixity and
opb's body.

`Binding.operator_info` now carries the unit it was imported from, stamped at the
import site - the only place that knows the written path - and
`Syntax.SyntaxOperatorUse` carries it through to the expander. The body comes from
the declaration that won the fixity, so the two cannot disagree by construction,
the name scan is gone, and last-wins holds for both halves.
