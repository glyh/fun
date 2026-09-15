open Core
include Elab_error
open Elab_common
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_resolve
open Elab_refine
open Elab_ops

(* Applying a function performs its latent row, instantiated at the argument.
   The argument is evaluated only when the row mentions it: evaluating it at
   check time would run what it performs. *)
let emit_latent ctx (latent : effect_row_closure) arg_core =
  match latent.effects, latent.tail with
  | [], None -> ()
  | _ ->
      let terms = latent.effects @ Option.to_list latent.tail in
      let arg =
        if List.exists (term_mentions_var 0) terms then Ctx.eval ctx arg_core
        else VRigid { lvl = ctx.Ctx.lvl; spine = [] }
      in
      emit ctx (expr_effects_of_row_values ctx (effect_row_values ctx latent arg))

(** Application inference.
    Loops to insert fresh metas for implicit VPi domains before consuming
    the user's explicit argument. *)
let infer_resume ops (ctx : Ctx.t) (arg : Syntax.t) : term * value =
  let cont_ix, cont_ty = Ctx.lookup_resume ctx in
  match Nbe.force ctx.metas cont_ty with
  | VPi { explicitness = Explicit; domain; codomain; _ } ->
      let arg_core = ops.check ctx arg domain in
      let arg_value = Ctx.eval ctx arg_core in
      (Ap (Var cont_ix, Explicit, arg_core), Nbe.closure_apply ctx.metas codomain arg_value)
  | _ -> raise (ElabError ApplyingNonFunction)

let infer_ap ops (ctx : Ctx.t) (f : Syntax.t) (a : Syntax.t) : term * value =
  let f_core, f_ty = ops.infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  (* Insert leading implicit arguments before consuming the user's explicit argument. *)
  let f_core, f_ty = insert_implicit_args ctx f_core f_ty in
  let rec pending_trait_dicts ty pending =
    match Nbe.force ctx.metas ty with
    | VPi { explicitness = Implicit; domain; codomain; _ } -> (
        match resolve_trait_dict_ty ctx domain with
        | Some (trait_info, args, _) -> (
            match resolve_trait_evidence_opt ctx trait_info args with
            | Ok (Some _) -> None
            | Ok None ->
                let dict_placeholder = Ctx.raw_meta ctx in
                pending_trait_dicts
                  (Nbe.closure_apply ctx.metas codomain dict_placeholder)
                  (pending @ [ (trait_info, args) ])
            | Error err -> raise (ElabError err))
        | None -> None)
    | ty -> if pending = [] then None else Some (pending, ty)
  in
  let unresolved_trait_arg args =
    List.exists
      (fun arg ->
        match Nbe.force ctx.metas arg with
        | VFlex _ | VRigid _ | VNeutral _ -> true
        | _ -> false)
      args
  in
  let apply_pending_trait_dicts pending core =
    List.fold_left
      (fun core (trait_info, args) ->
        let evidence_core =
          match resolve_trait_evidence_opt ctx trait_info args with
          | Ok (Some (evidence_core, _)) -> evidence_core
          | Ok None when unresolved_trait_arg args -> Ctx.fresh_meta ctx
          | Ok None -> raise (ElabError (MissingTraitImplementation (trait_key trait_info.trait_name args)))
          | Error err -> raise (ElabError err)
        in
        Ap (core, Implicit, evidence_core))
      core pending
  in
  match f_ty with
  | VPi { explicitness = Explicit; domain = a_ty; effects; codomain = b_clo } ->
      let a_core = ops.check ctx a a_ty in
      emit_latent ctx effects a_core;
      let ret_ty =
        if term_mentions_var 0 b_clo.body then
          let a_val = Ctx.eval ctx a_core in
          Nbe.closure_apply ctx.metas b_clo a_val
        else Nbe.closure_apply ctx.metas b_clo (VRigid { lvl = ctx.lvl; spine = [] })
      in
      (Ap (f_core, Explicit, a_core), Nbe.force ctx.metas ret_ty)
  | _ -> (
      match pending_trait_dicts f_ty [] with
      | Some (pending, VPi { explicitness = Explicit; domain = a_ty; effects; codomain = b_clo }) ->
          let a_core = ops.check ctx a a_ty in
          emit_latent ctx effects a_core;
          let f_core = apply_pending_trait_dicts pending f_core in
          let ret_ty =
            if term_mentions_var 0 b_clo.body then
              let a_val = Ctx.eval ctx a_core in
              Nbe.closure_apply ctx.metas b_clo a_val
            else Nbe.closure_apply ctx.metas b_clo (VRigid { lvl = ctx.lvl; spine = [] })
          in
          (Ap (f_core, Explicit, a_core), Nbe.force ctx.metas ret_ty)
      | _ -> (
          match f_ty with
          | VFlex _ | VRigid _ | VNeutral _ ->
              let a_ty = Ctx.raw_meta ctx in
              let a_core = ops.check ctx a a_ty in
              let a_val = Ctx.eval ctx a_core in
              let ret_ctx, _ = Ctx.bind_anonymous ctx a_ty in
              let ret_meta = Ctx.raw_meta ret_ctx in
              let ret_body = Ctx.quote ret_ctx ret_meta in
              let expected_f_ty =
                VPi
                  { explicitness = Explicit;
                    domain = a_ty;
                    effects = effect_row_closure ctx.env empty_effect_row;
                    codomain = { env = ctx.env; body = ret_body } }
              in
              Ctx.unify ctx f_ty expected_f_ty;
              let ret_ty =
                Nbe.closure_apply ctx.metas
                  { env = ctx.env; body = ret_body }
                  a_val
              in
              (Ap (f_core, Explicit, a_core), ret_ty)
          | _ -> raise (ElabError ApplyingNonFunction)))

(** Explicit implicit application ([f {arg}]). *)
let infer_ap_implicit ops (ctx : Ctx.t) (f : Syntax.t) (a : Syntax.t) : term * value =
  let f_core, f_ty = ops.infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  match f_ty with
  | VPi { explicitness = Implicit; domain = a_ty; effects; codomain = b_clo } ->
      let a_core = ops.check ctx a a_ty in
      emit_latent ctx effects a_core;
      let a_val = Ctx.eval ctx a_core in
      let ret_ty = Nbe.closure_apply ctx.metas b_clo a_val in
      (Ap (f_core, Implicit, a_core), Nbe.force ctx.metas ret_ty)
  | VPi { explicitness = Explicit; _ } ->
      raise (ElabError ApplyingNonFunction)
  | VFlex _ | VRigid _ | VNeutral _ ->
      let a_ty = Ctx.raw_meta ctx in
      let a_core = ops.check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_ctx, _ = Ctx.bind_anonymous ctx a_ty in
      let ret_meta = Ctx.raw_meta ret_ctx in
      let ret_body = Ctx.quote ret_ctx ret_meta in
      let expected_f_ty =
        VPi
          { explicitness = Implicit;
            domain = a_ty;
            effects = effect_row_closure ctx.env empty_effect_row;
            codomain = { env = ctx.env; body = ret_body } }
      in
      Ctx.unify ctx f_ty expected_f_ty;
      let ret_ty =
        Nbe.closure_apply ctx.metas
          { env = ctx.env; body = ret_body }
          a_val
      in
      (Ap (f_core, Implicit, a_core), ret_ty)
  | _ -> raise (ElabError ApplyingNonFunction)

(** Lambda inference. *)
let infer_lam ops (ctx : Ctx.t) (param : Syntax.param) (body : Syntax.t) :
    term * value =
  let a_ty =
    match param.type_ with
    | Some ty_expr ->
        let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
        ty_val
    | None ->
        Ctx.raw_meta ctx
  in
  let ctx' = Ctx.bind ctx param.name.name a_ty in
  let (body_core, body_ty), body_effects = collecting ctx' (fun ctx' -> ops.infer ctx' body) in
  let body_ty_term = Ctx.quote ctx' body_ty in
  let pi_ty = VPi { explicitness = expl_of_syntax param.explicitness; domain = a_ty; effects = effect_row_closure ctx.env (effect_row_of_expr_effects ctx' body_effects); codomain = { env = ctx.env; body = body_ty_term } } in
  (Lam body_core, pi_ty)
