---
name: de-bruijn-debugging
description: |
  Use when debugging de Bruijn index mismatches between elaboration and NBE evaluation.
  Trigger when tests produce wrong values (VNeutral, wrong primitive), List.nth failures,
  or "unbound constructor/type" errors. Project-specific for this OCaml compiler.
---

# De Bruijn Index Debugging

When elaboration builds a Ctx and NBE evaluates in a parallel env, they must match
entry-for-entry. A mismatch of one entry causes Var ix to resolve to the wrong value.

## Reusable utilities

`env_value_label` and `dump_env` are already implemented at
`lib/backend/interp/nbe_support.ml`. Call them from both elaboration and NBE:

```ocaml
Nbe_support.dump_env "elab ctx" ctx.Ctx.env
Nbe_support.dump_env "nbe env" (List.length env) env
```

## Diagnostic pattern

1. **Print both sides at key points.** Add `dump_env` calls at the start of
   `go` (in `elab_infer.ml`, Module case) and `eval_binds` (in `nbe.ml`).

2. **Narrow the gap.** `Printf.eprintf` the env size after each binding in both
   elaboration and NBE. The mismatch appears at a specific binding.

3. **Check specific indices.** When a test fails, print what Var ix resolves to:

   In `elab_infer.ml` `Var` case:
   ```ocaml
   Printf.eprintf "Elab: %s -> Var[%d] (ctx=%d)\n%!" name ix (List.length ctx.Ctx.env)
   ```

   In `nbe.ml` `Var ix` case:
   ```ocaml
   Printf.eprintf "NBE: Var[%d] = %s (env=%d)\n%!" ix
     (Nbe_support.env_value_label v) (List.length env)
   ```

## Common causes

### NBE doesn't push parameter entries

Elaboration creates VRigid entries for type parameters. NBE `eval_binds` must
also push dummy entries. Fixed at `nbe.ml` TypeBind case — match on
`VNominal { num_params; _ }` and push `num_params` dummy entries before ctors.

### Multiple Ctx.define for the same name

`Ctx.define` always ADDS an entry (never overwrites). Defining a type name
multiple times (placeholder + overwrite) adds multiple env entries. NBE pushes it
once. Fix in `elab_infer.ml` nullary TypeBinding: use a manual temp context
(`{ ctx with env = placeholder :: ctx.env; lvl = ctx.lvl + 1; ... }`) for
self-referential payload elaboration, then `Ctx.define` only once at the end.

### Anonymous entries

`Ctx.define_anonymous` / `Ctx.bind` adds env entries without name_table entries.
ImplBinding uses these. Verify NBE pushes corresponding entries.

### Bindings skipped in elaboration

Some bindings (MacroBinding) are skipped in the elaboration `go` fold. Verify
both sides skip the same bindings.

## Quick fixes

### Prim detection

When a variable resolves to a primitive (VNeutral with HPrim head), emit `Prim`
instead of `Var ix`. This makes the term env-independent. See the `stx_` prefix
check in `elab_infer.ml` `Var` case.

### nominal_value embedding

When name-based lookup (`eval_con`) fails because the nominal is nested in a
sub-module (not in flat env), embed the value directly in the term. See `Ctor`
in `core.ml` — add a `nominal_value : value` field, set it in `build_ctor`,
use `force mc nominal_value` instead of `eval_con env nominal_name` in `nbe.ml`.
