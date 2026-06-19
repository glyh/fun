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

let rec shift_term amount cutoff term =
  let shift = shift_term amount in
  match term with
  | Var ix when ix >= cutoff -> Var (ix + amount)
  | Var _ | Atom _ | AtomTy _ | U | Prim _ | Meta _ | InsertedMeta _ | Con _ | TraitRef _ | Stx _ -> term
  | Lam body -> Lam (shift (cutoff + 1) body)
  | Ap (f, expl, a) -> Ap (shift cutoff f, expl, shift cutoff a)
  | Let (ty, def, body) -> Let (shift cutoff ty, shift cutoff def, shift (cutoff + 1) body)
  | Pi { explicitness; domain; effects; codomain } ->
      Pi
        { explicitness;
          domain = shift cutoff domain;
          effects = { effects = List.map (shift (cutoff + 1)) effects.effects; tail = Option.map (shift (cutoff + 1)) effects.tail };
          codomain = shift (cutoff + 1) codomain }
  | If (cond, then_, else_) -> If (shift cutoff cond, shift cutoff then_, shift cutoff else_)
  | Prod elems -> Prod (List.map (shift cutoff) elems)
  | ProdTy elems -> ProdTy (List.map (shift cutoff) elems)
  | EffectRowTy -> EffectRowTy
  | EffectRowLit row -> EffectRowLit { effects = List.map (shift cutoff) row.effects; tail = Option.map (shift cutoff) row.tail }
  | RefTy a -> RefTy (shift cutoff a)
  | RefNew a -> RefNew (shift cutoff a)
  | RefGet a -> RefGet (shift cutoff a)
  | RefSet (r, e) -> RefSet (shift cutoff r, shift cutoff e)
  | Proj (e, i) -> Proj (shift cutoff e, i)
  | Dot (e, field) -> Dot (shift cutoff e, field)
  | RecordConstruct { typ; fields } ->
      RecordConstruct { typ = shift cutoff typ; fields = List.map (fun (field, value) -> (field, shift cutoff value)) fields }
  | Open (s, body) -> Open (shift cutoff s, shift cutoff body)
  | Fix body -> Fix (shift (cutoff + 1) body)
  | NomRef (name, params) -> NomRef (name, List.map (shift cutoff) params)
  | EffectRef (name, params) -> EffectRef (name, List.map (shift cutoff) params)
  | TraitDictTy { trait_id; trait_name; args; fields } ->
      TraitDictTy { trait_id; trait_name; args = List.map (shift cutoff) args; fields = List.map (fun (name, value) -> (name, shift cutoff value)) fields }
  | SelfTypeRef args -> SelfTypeRef (List.map (shift cutoff) args)
  | Ctor { name; spine; nominal_name; nominal_spine } ->
      Ctor { name; spine = List.map (shift cutoff) spine; nominal_name; nominal_spine = List.map (shift cutoff) nominal_spine }
  | Match (scrut, branches) ->
      let branch = function
        | ValueBranch (pat, body) -> ValueBranch (pat, shift cutoff body)
        | EffectBranch { eff; op; arg_pat; body } -> EffectBranch { eff; op; arg_pat; body = shift cutoff body }
      in
      Match (shift cutoff scrut, List.map branch branches)
  | NominalDef { id; name; num_params; ctors; body } ->
      NominalDef { id; name; num_params; ctors = List.map (fun (ctor, payloads) -> (ctor, List.map (shift cutoff) payloads)) ctors; body = shift cutoff body }
  | EffectDef { id; name; num_params; ops; body } ->
      EffectDef { id; name; num_params; ops = List.map (fun (op, input, output) -> (op, shift cutoff input, shift cutoff output)) ops; body = shift cutoff body }
  | Perform { eff; op; arg } -> Perform { eff = shift cutoff eff; op; arg = shift cutoff arg }
  | Module { bindings } ->
      let binding = function
        | LetBind (field, kind, value) -> LetBind (field, kind, shift cutoff value)
        | ImplBind (kind, value, ty) -> ImplBind (kind, shift cutoff value, ty)
        | TypeBind _ | EffectBind _ | PatternSynBind _ as binding -> binding
      in
      Module { bindings = List.map binding bindings }
  | Struct { con_fields; bindings; partial } ->
      let binding = function
        | LetBind (field, kind, value) -> LetBind (field, kind, shift cutoff value)
        | ImplBind (kind, value, ty) -> ImplBind (kind, shift cutoff value, ty)
        | TypeBind _ | EffectBind _ | PatternSynBind _ as binding -> binding
      in
      Struct { con_fields = List.map (fun (field, ty) -> (field, shift cutoff ty)) con_fields; bindings = List.map binding bindings; partial }

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
           nominal_name; nominal_spine = param_vars }
  in
  let core_term =
    let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
    wrap total_args body_term
  in
  let ctor_val = Nbe.eval mc env core_term in
  let depth = List.length env in
  let type_term =
    let nom_ret_vars = List.init num_params (fun i -> Var (payload_count + num_params - 1 - i)) in
    let ret_term = NomRef (nominal_name, nom_ret_vars) in
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
