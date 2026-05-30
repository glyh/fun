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

let elaborate_impl ops ctx trait_name args fields =
  let trait_info = lookup_trait ctx trait_name in
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
  let impl_value = Ctx.eval ctx impl_core in
  let ctx', entry = Ctx.define_anonymous ctx expected_dict_ty impl_value in
  let evidence =
    { evidence_trait_id = trait_info.trait_id;
      evidence_trait_name = trait_name;
      evidence_args = arg_values;
      evidence_level = entry.level;
      evidence_ty = expected_dict_ty }
  in
  (Ctx.add_trait_evidence ctx' evidence, impl_effects, evidence, expected_dict_ty, impl_core)


let elaborate_eff_family ops (ctx : Ctx.t) (name : string) (params : string list)
    (effect_ops : Surface.effect_op list) : effect_id * value * value * (string * term * term) list =
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
      (fun (op : Surface.effect_op) ->
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

(** Build a constructor's VLam chain + VPi type from type params and optional payload.
    [payload_clo_opt] is a closure whose body is the payload type term with
    de Bruijn indices 0..num_params-1 referencing type params. *)
let build_ctor (mc : MetaContext.t) (env : env) (nominal_name : string) (ctor_name : string)
    (num_params : int) (payload_clo_opt : closure option)
    : value * value =
  let has_payload = Option.is_some payload_clo_opt in
  let total_args = num_params + (if has_payload then 1 else 0) in
  let param_vars = List.init num_params (fun i -> Var (total_args - 1 - i)) in
  let payload_var = if has_payload then [ Var 0 ] else [] in
  let all_spine_vars = param_vars @ payload_var in
  let body_term =
    Ctor { name = ctor_name; spine = all_spine_vars;
           nominal_name; nominal_spine = param_vars }
  in
  let core_term =
    let with_payload = if has_payload then Lam body_term else body_term in
    let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
    wrap num_params with_payload
  in
  let ctor_val = Nbe.eval mc env core_term in
  let depth = List.length env in
  let type_term =
    let nom_ret_vars =
      if has_payload then
        List.init num_params (fun i -> Var (num_params - i))
      else
        List.init num_params (fun i -> Var (num_params - 1 - i))
    in
    let ret_term = NomRef (nominal_name, nom_ret_vars) in
    match payload_clo_opt with
    | Some payload_clo ->
        (* The payload closure body has indices relative to [payload_clo.env]
           with num_params extra bindings prepended. Inside the implicit Pi chain,
           param i is at level depth+i. Evaluate with rigid vars to get the
           payload value, then quote at the right depth. *)
        let param_rigids = List.init num_params (fun i ->
          VRigid { lvl = depth + i; spine = [] }) in
        let payload_val =
          Nbe.eval mc (List.rev param_rigids @ payload_clo.env) payload_clo.body in
        let payload_term = Nbe.quote mc (depth + num_params) payload_val in
        let inner = payload_term ^->> ret_term in
        let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (U ^=>> t) in
        wrap_pi num_params inner
    | None ->
        let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (U ^=>> t) in
        wrap_pi num_params ret_term
  in
  let ret_type = Nbe.eval mc env type_term in
  (ctor_val, ret_type)
