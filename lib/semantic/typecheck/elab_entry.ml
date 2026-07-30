open Core
include Elab_error
open Elab_common
open Elab_prelude

module Ctx = Elab_ctx.Ctx

open Elab_resolve

(** Build the initial elaboration context with built-in types ([I64],
    [Bool], [Unit], [Char], [Type]) and primitive operators. *)
let init_ctx () : Ctx.t =
  let ctx = Ctx.empty () in
  let add_type ctx name v = Ctx.define ctx name VU v in
  let ctx = add_type ctx Compiler_names.Type_name.i64 (VAtomTy Atom_ty.TI64) in
  (* [Bool] is no longer a builtin atom type — it is the nominal ADT defined in
     the prelude ([type Bool = False | True]). *)
  let ctx = add_type ctx Compiler_names.Type_name.unit (VAtomTy Atom_ty.TUnit) in
  let ctx = add_type ctx Compiler_names.Type_name.char (VAtomTy Atom_ty.TChar) in
  let ctx = add_type ctx Compiler_names.Type_name.string (VAtomTy Atom_ty.TString) in
  let ctx = add_type ctx Compiler_names.Type_name.absurd (VAtomTy Atom_ty.TAbsurd) in
  let ctx = Ctx.define ctx Compiler_names.Type_name.type_ VU VU in
  let ctx = Ctx.define ctx Compiler_names.Type_name.effect_row VU VEffectRowTy in
  let ref_ty = VPi { explicitness = Explicit; domain = VU; effects = effect_row_closure ctx.env empty_effect_row; codomain = { env = ctx.env; body = U } } in
  let ctx = Ctx.define ctx Compiler_names.Type_name.ref_ ref_ty (VLam { body = { env = ctx.env; body = RefTy (Var 0) } }) in
  let ctx =
    NameMap.fold
      (fun name ty ctx ->
        Ctx.define ctx name ty (VNeutral { ty; neutral = { head = HPrim name; frames = [] } }))
      prims ctx
  in
  let stdlib_core, stdlib_ty = Elab_driver.infer ctx (Lazy.force parsed_stdlib) in
  let stdlib_value = Ctx.eval ctx stdlib_core in
  let ctx = Ctx.hide_names ctx syntax_primitive_names in
  Ctx.define ctx Compiler_names.Module_name.stdlib stdlib_ty stdlib_value

let open_stdlib ctx =
  let ix, ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
  let value = Ctx.eval ctx (Var ix) in
  (ix, open_module_value ctx ty value)

let resolve_stdlib (ctx : Ctx.t) (path : string list) : value =
  Elab_stdlib.resolve ctx path

(* The prelude is opened for user code by the same [open (import "std")]
   construct a program could write itself, rather than a bespoke implicit-open
   path: wrap the body in [Surface.Open (Import "std", body)] and elaborate that.
   [import "std"] is the reserved prelude (see [elab_infer]); the [Open] case
   checks it is a module and opens it. The operator side of this synthetic open
   is delivered at parse time via [?builtin_syntax] (the exports the caller
   injects), so [+]/[if]/… are in scope while the body parses. *)
let open_prelude (expr : Surface.t) : Surface.t =
  Surface.Open (Surface.Import Compiler_names.Module_name.std_import_path, expr)

(** Entry point: elaborate a surface expression in the given context. *)
let on_expr ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  Elab_driver.infer ctx (open_prelude expr)

let on_expr_effects ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value * Elab_effects.expr_effects =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  let wrapped = open_prelude expr in
  let core, ty = Elab_driver.infer ctx wrapped in
  (core, ty, Elab_driver.collect_effects ctx wrapped)
