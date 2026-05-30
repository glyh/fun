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
  let ctx = add_type ctx "I64" (VAtomTy TI64) in
  let ctx = add_type ctx "Bool" (VAtomTy TBool) in
  let ctx = add_type ctx "Unit" (VAtomTy TUnit) in
  let ctx = add_type ctx "Char" (VAtomTy TChar) in
  let ctx = add_type ctx "String" (VAtomTy TString) in
  let ctx = add_type ctx "Absurd" (VAtomTy TAbsurd) in
  let ctx = Ctx.define ctx "Type" VU VU in
  let ctx = Ctx.define ctx "EffectRow" VU VEffectRowTy in
  let ref_ty = VPi { explicitness = Explicit; domain = VU; effects = effect_row_closure ctx.env empty_effect_row; codomain = { env = ctx.env; body = U } } in
  let ctx = Ctx.define ctx "Ref" ref_ty (VLam { body = { env = ctx.env; body = RefTy (Var 0) } }) in
  let ctx =
    NameMap.fold
      (fun name ty ctx ->
        Ctx.define ctx name ty (VNeutral { ty; neutral = { head = HPrim name; frames = [] } }))
      prims ctx
  in
  let stdlib_core, stdlib_ty = Elab_driver.infer ctx (Lazy.force parsed_stdlib) in
  let stdlib_value = Ctx.eval ctx stdlib_core in
  Ctx.define ctx "stdlib" stdlib_ty stdlib_value

let open_stdlib ctx =
  let ix, ty = Ctx.lookup ctx "stdlib" in
  let value = Ctx.eval ctx (Var ix) in
  (ix, open_module_value ctx ty value)

(** Entry point: elaborate a surface expression in the given context. *)
let on_expr ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  let stdlib_ix, ctx = open_stdlib ctx in
  let core, ty = Elab_driver.infer ctx expr in
  (Open (Var stdlib_ix, core), ty)

let on_expr_effects ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value * Elab_effects.expr_effects =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  let stdlib_ix, ctx = open_stdlib ctx in
  let core, ty = Elab_driver.infer ctx expr in
  (Open (Var stdlib_ix, core), ty, Elab_driver.collect_effects ctx expr)
