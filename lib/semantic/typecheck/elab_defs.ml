open Core
include Elab_error
open Elab_common
open Elab_validate

module Ctx = Elab_ctx.Ctx

open Elab_resolve
open Elab_ops

let elaborate_trait ops ctx name params fields =
  check_duplicate_trait_fields fields;
  let param_ctx =
    List.fold_left
      (fun ctx param_name ->
        Ctx.define ctx param_name VU (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
      ctx params
  in
  let field_terms =
    List.map
      (fun (field_name, ty_expr) ->
        let ty_core, ty_ty = ops.infer param_ctx ty_expr in
        check_type_like param_ctx ty_ty (Ctx.eval param_ctx ty_core);
        (field_name, ty_core))
      fields
  in
  let trait_info =
    { trait_id = fresh_trait_id ();
      trait_name = name;
      trait_params = params;
      trait_fields = List.map (fun (field_name, ty_core) -> (field_name, { env = ctx.Ctx.env; body = ty_core })) field_terms }
  in
  Hashtbl.replace trait_registry trait_info.trait_id trait_info;
  let trait_ty = VTrait { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name } in
  (trait_info, trait_ty)

(* What an impl contributes, worked out without touching the context: the
   dictionary type and value that occupy its single entry, plus the trait
   identity the evidence needs. Separated from installing it so a binding fold
   can extend the context from [Core.binding_slots] like every other binding,
   rather than receiving a context someone else extended.
   See docs/wayfinder/tickets/bring-impls-and-traits-into-the-slot-list.md. *)
type impl_contribution = {
  impl_effects : Elab_effects.expr_effects list;
  impl_dict_ty : value;
  impl_core : term;
  impl_value : value;
  impl_trait_id : int;
  impl_trait_name : string;
  impl_args : value list;
}

let elaborate_impl_contribution ops ctx trait_path args fields =
  let trait_info = lookup_trait ctx trait_path in
  let trait_name = trait_info.trait_name in
  let arg_cores =
    List.map
      (fun arg ->
        let arg_core, arg_ty = ops.infer ctx arg in
        check_type_like ctx arg_ty (Ctx.eval ctx arg_core);
        arg_core)
      args
  in
  let arg_values = List.map (Ctx.eval ctx) arg_cores in
  let expected_fields = eval_trait_fields ctx trait_info arg_values in
  let expected_dict_ty = trait_dict_ty ~trait_id:trait_info.trait_id trait_name arg_values expected_fields in
  check_duplicate_names (List.map fst fields);
  List.iter
    (fun (name, _) ->
      if Option.is_none (List.assoc_opt name expected_fields) then
        raise (ElabError (UnknownTraitMethod name)))
    fields;
  List.iter
    (fun (name, _) ->
      if Option.is_none (List.assoc_opt name fields) then raise (ElabError (MissingTraitField name)))
    expected_fields;
  let impl_effects = List.map (fun (_, value) -> ops.collect_effects ctx value) fields in
  let field_cores =
    List.map
      (fun (name, value) ->
        let field_ty =
          match List.assoc_opt name expected_fields with
          | Some ty -> ty
          | None -> raise (ElabError (UnknownTraitMethod name))
        in
        (name, ops.check ctx value field_ty))
      fields
  in
  let impl_core = Struct { con_fields = []; bindings = List.map (fun (name, value) -> LetBind (name, Public, value)) field_cores; partial = false } in
  { impl_effects;
    impl_dict_ty = expected_dict_ty;
    impl_core;
    impl_value = Ctx.eval ctx impl_core;
    impl_trait_id = trait_info.trait_id;
    impl_trait_name = trait_name;
    impl_args = arg_values }

(* The furniture that rides along with an impl's entry but adds no entry of its
   own: the evidence resolution searches, and the name an [impl NAME : …] is
   reachable by. [impl_name] names the entry the impl already occupies rather
   than adding one, so the impl is reachable as a member without changing what
   the binding contributes. See docs/wayfinder/topics/impl-visibility.md. *)
let install_impl_evidence ?impl_name ctx (c : impl_contribution) ~level =
  let evidence =
    { evidence_trait_id = c.impl_trait_id;
      evidence_trait_name = c.impl_trait_name;
      evidence_args = c.impl_args;
      evidence_level = level;
      evidence_ty = c.impl_dict_ty }
  in
  let ctx = Ctx.add_trait_evidence ctx evidence in
  let ctx =
    match impl_name with
    | Some n -> Ctx.alias ctx n { level; ty = c.impl_dict_ty }
    | None -> ctx
  in
  (ctx, evidence)

(* An impl in expression position, where there is no binding term and so no slot
   list: contribute, take the entry, install. *)
let elaborate_impl ?impl_name ops ctx trait_path args fields =
  let c = elaborate_impl_contribution ops ctx trait_path args fields in
  let ctx', entry = Ctx.define_anonymous ctx c.impl_dict_ty c.impl_value in
  let ctx', evidence = install_impl_evidence ?impl_name ctx' c ~level:entry.level in
  (ctx', c.impl_effects, evidence, c.impl_dict_ty, c.impl_core)


let elaborate_eff_family ops (ctx : Ctx.t) (name : string) (params : string list)
    (effect_ops : Syntax.effect_op list) : effect_id * value * value * (string * term * term) list =
  check_duplicate_eff_ops effect_ops;
  let param_ctx =
    List.fold_left
      (fun ctx param_name ->
        Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
      ctx params
  in
  let effect_id = EffectId.fresh () in
  let elaborated_ops =
    List.map
      (fun (op : Syntax.effect_op) ->
        let input_core, _input_ty, _input_val = ops.type_value_of_expr param_ctx op.input in
        let output_core, _output_ty, _output_val = ops.type_value_of_expr param_ctx op.output in
        (op.name, input_core, output_core))
      effect_ops
  in
  let operations =
    List.map
      (fun (op_name, input_core, output_core) ->
        (op_name, { env = ctx.env; body = input_core }, { env = ctx.env; body = output_core }))
      elaborated_ops
  in
  let eff = VEffect { id = effect_id; name; params = []; operations } in
  let eff_ty =
    List.fold_right
      (fun _ acc ->
        VPi
          { explicitness = Explicit;
            domain = VU;
            effects = effect_row_closure ctx.env empty_effect_row;
            codomain = { env = ctx.env; body = Nbe.quote ctx.metas ctx.lvl acc } })
      params VU
  in
  (effect_id, eff, eff_ty, elaborated_ops)

(* A term transformer cannot rewrite indices under a subterm whose binder count
   only evaluation reveals ([Core.map_subterms] gives it [None]: an [open]'s
   body, bindings after an [OpenBind]); a guessed count would yield a silently
   wrong value rather than a type error, so refuse it loudly.
   See docs/wayfinder/tickets/core-traversals-count-binders-separately.md. *)
let reject_unknown_binder_count where =
  failwith (where ^ ": subterm under a binder count known only by evaluation (an open)")

let rec shift_term amount cutoff term =
  match term with
  | Var ix when ix >= cutoff -> Var (ix + amount)
  | _ ->
      map_subterms
        (fun under sub ->
          match under with
          | Some n -> shift_term amount (cutoff + n) sub
          | None -> reject_unknown_binder_count "shift_term")
        term

(** Build a constructor's VLam chain + VPi type from type params and payloads.
    Payload closures have bodies whose de Bruijn indices 0..num_params-1
    reference type params. Constructor comma payloads are separate arguments;
    tuple payloads are represented as one [ProdTy] payload. *)
let build_ctor (mc : MetaContext.t) (env : env) (nominal_name : string) (ctor_name : string)
    (num_params : int) (payload_clos : closure list)
    : value * value =
  let payload_count = List.length payload_clos in
  let total_args = num_params + payload_count in
  let param_vars = List.init num_params (fun i -> Var (total_args - 1 - i)) in
  let payload_vars = List.init payload_count (fun i -> Var (payload_count - 1 - i)) in
  let all_spine_vars = param_vars @ payload_vars in
  let body_term =
    Ctor { name = ctor_name; spine = all_spine_vars;
           nominal_name; nominal_spine = param_vars;
           nominal_value = List.hd env }
  in
  let core_term =
    let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
    wrap total_args body_term
  in
  let ctor_val = Nbe.eval mc env core_term in
  let depth = List.length env in
  let type_term =
    let nom_ret_vars = List.init num_params (fun i -> Var (payload_count + num_params - 1 - i)) in
    let ret_term =
      match List.hd env with
      | VNominal { id; _ } -> NomRef { id; name = nominal_name; params = nom_ret_vars }
      | _ -> failwith "build_ctor: the environment's head is not the nominal"
    in
    let param_rigids = List.init num_params (fun i -> VRigid { lvl = depth + i; spine = [] }) in
    let payload_terms =
      List.mapi
        (fun i payload_clo ->
          let payload_val = Nbe.eval mc (List.rev param_rigids @ payload_clo.env) payload_clo.body in
          Nbe.quote mc (depth + num_params) payload_val |> shift_term i 0)
        payload_clos
    in
    let inner = List.fold_right (fun payload acc -> payload ^->> acc) payload_terms ret_term in
    let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (U ^=>> t) in
    wrap_pi num_params inner
  in
  let ret_type = Nbe.eval mc env type_term in
  (ctor_val, ret_type)
