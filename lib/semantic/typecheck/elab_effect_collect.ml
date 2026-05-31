open Core
include Elab_error
open Elab_common
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_surface_rewrite
open Elab_resolve
open Elab_refine
open Elab_patterns
open Elab_match
open Elab_defs
open Elab_generalize
open Elab_ops

let rec compile_time_safe (expr : Surface.t) : bool =
  match expr with
  | Surface.RefNew _ | Surface.RefGet _ | Surface.RefSet _ -> false
  | Surface.Atom _ | Surface.Var _ | Surface.Self | Surface.SelfType | Surface.Import _ -> true
  | Surface.Ap (f, _, a) -> compile_time_safe f && compile_time_safe a
  | Surface.Lam (_, body) -> compile_time_safe body
  | Surface.Let { type_; value; body; _ } ->
      Option.fold ~none:true ~some:compile_time_safe type_ && compile_time_safe value && compile_time_safe body
  | Surface.If { cond; then_; else_ } ->
      compile_time_safe cond && compile_time_safe then_ && compile_time_safe else_
  | Surface.Annotated { inner; typ } -> compile_time_safe inner && compile_time_safe typ
  | Surface.Prod elems | Surface.ProdTy elems -> List.for_all compile_time_safe elems
  | Surface.Arrow (_, _, a, row, b) ->
      let row_safe =
        match row with
        | None -> true
        | Some (row : Surface.effect_row) ->
            List.for_all compile_time_safe row.effects && Option.fold ~none:true ~some:compile_time_safe row.tail
      in
      compile_time_safe a && row_safe && compile_time_safe b
  | Surface.FieldAccess (e, _) | Surface.Proj (e, _) -> compile_time_safe e
  | Surface.RecordConstruct { typ; fields } ->
      compile_time_safe typ && List.for_all (fun (_, value) -> compile_time_safe value) fields
  | Surface.Struct { con_fields; bindings } ->
      List.for_all (fun (_, ty) -> compile_time_safe ty) con_fields
      && List.for_all compile_time_safe_struct_binding bindings
  | Surface.Module { bindings } -> List.for_all compile_time_safe_struct_binding bindings
  | Surface.Open (_, body) -> compile_time_safe body
  | Surface.RecordTypeDef { fields; body; _ } ->
      List.for_all (fun (_, ty) -> compile_time_safe ty) fields && compile_time_safe body
  | Surface.TypeDef { ctors; body; _ } ->
      List.for_all (fun (_, payload) -> Option.fold ~none:true ~some:compile_time_safe payload) ctors && compile_time_safe body
  | Surface.EffectDef { ops; body; _ } ->
      List.for_all (fun (op : Surface.effect_op) -> compile_time_safe op.input && compile_time_safe op.output) ops && compile_time_safe body
  | Surface.TraitDef { fields; body; _ } ->
      List.for_all (fun (_, ty) -> compile_time_safe ty) fields && compile_time_safe body
  | Surface.ImplDef { args; fields; body; _ } ->
      List.for_all compile_time_safe args && List.for_all (fun (_, value) -> compile_time_safe value) fields && compile_time_safe body
  | Surface.Perform _ | Surface.Resume _ | Surface.Match _ -> false
  | Surface.MacroDef _ | Surface.MacroCall _ -> failwith "MacroDef/MacroCall should not reach elaboration"

and compile_time_safe_struct_binding = function
  | Surface.LetBinding { value; _ } -> compile_time_safe value
  | Surface.MethodBinding { body; _ } -> compile_time_safe body
  | Surface.TypeBinding { ctors; _ } ->
      List.for_all (fun (_, payload) -> Option.fold ~none:true ~some:compile_time_safe payload) ctors
  | Surface.RecordTypeBinding { fields; _ } -> List.for_all (fun (_, ty) -> compile_time_safe ty) fields
  | Surface.EffectBinding { ops; _ } ->
      List.for_all (fun (op : Surface.effect_op) -> compile_time_safe op.input && compile_time_safe op.output) ops
  | Surface.TraitBinding { fields; _ } -> List.for_all (fun (_, ty) -> compile_time_safe ty) fields
  | Surface.ImplBinding { args; fields; _ } ->
      List.for_all compile_time_safe args && List.for_all (fun (_, value) -> compile_time_safe value) fields

let collect_effects ops (ctx : Ctx.t) (expr : Surface.t) : expr_effects =
  match expr with
  | Surface.Perform { effect_path; op; arg } ->
      let effect_core, effect_value, input_ty, _output_ty = resolve_perform_operation ctx ~effect_path ~op in
      let _arg_core = ops.check ctx arg input_ty in
      union_expr_effects ctx (ops.collect_effects ctx arg) (singleton_expr_effect effect_core effect_value)
  | Surface.Resume arg -> ops.collect_effects ctx arg
  | Surface.RefNew e | Surface.RefGet e -> ops.collect_effects ctx e
  | Surface.RefSet (r, e) -> union_expr_effects ctx (ops.collect_effects ctx r) (ops.collect_effects ctx e)
  | Surface.Ap (f, Explicitness.Explicit, a) ->
      let f_core, f_ty = ops.infer ctx f in
      let _f_core, f_ty = insert_implicit_args ctx f_core f_ty in
      let f_ty = Nbe.force ctx.Ctx.metas f_ty in
      let latent, _arg_core =
        match f_ty with
        | VPi { explicitness = Explicit; domain = a_ty; effects; _ } ->
            let arg_core = ops.check ctx a a_ty in
            let latent =
              match effects.effects, effects.tail with
              | [], None -> empty_expr_effects
              | _ ->
                  let arg_val = Ctx.eval ctx arg_core in
                  expr_effects_of_row_values ctx (effect_row_values ctx effects arg_val)
            in
            (latent, arg_core)
        | _ -> (empty_expr_effects, Atom Atom.Unit)
      in
      union_many_expr_effects ctx [ ops.collect_effects ctx f; ops.collect_effects ctx a; latent ]
  | Surface.Ap (f, Explicitness.Implicit, a) ->
      let _f_core, f_ty = ops.infer ctx f in
      let f_ty = Nbe.force ctx.Ctx.metas f_ty in
      let latent, _arg_core =
        match f_ty with
        | VPi { explicitness = Implicit; domain = a_ty; effects; _ } ->
            let arg_core = ops.check ctx a a_ty in
            let latent =
              match effects.effects, effects.tail with
              | [], None -> empty_expr_effects
              | _ ->
                  let arg_val = Ctx.eval ctx arg_core in
                  expr_effects_of_row_values ctx (effect_row_values ctx effects arg_val)
            in
            (latent, arg_core)
        | _ -> (empty_expr_effects, Atom Atom.Unit)
      in
      union_many_expr_effects ctx [ ops.collect_effects ctx f; ops.collect_effects ctx a; latent ]
  | Surface.Lam _ -> empty_expr_effects
  | Surface.Let { name; type_; value; body; recursive = false } ->
      let value_effects = ops.collect_effects ctx value in
      let value_core, value_ty =
        match type_ with
        | Some ty_expr ->
            require_empty_effects ctx (ops.collect_effects ctx ty_expr);
            let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
            (ops.check ctx value ty_val, ty_val)
        | None -> ops.infer ctx value
      in
      let gen_value_core, gen_value_ty = generalize ctx value_core value_ty in
      let body_ctx =
        if is_empty_expr_effects value_effects && compile_time_safe value then Ctx.define ctx name gen_value_ty (Ctx.eval ctx gen_value_core)
        else Ctx.bind ctx name gen_value_ty
      in
      let body_effects = ops.collect_effects body_ctx body in
      union_expr_effects ctx value_effects body_effects
  | Surface.Let { name; type_; value; body; recursive = true } ->
      let rec_ty =
        match type_ with
        | Some ty_expr ->
            require_empty_effects ctx (ops.collect_effects ctx ty_expr);
            let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
            ty_val
        | None -> Ctx.raw_meta ctx
      in
      let fix_core = Fix (ops.check (Ctx.bind ctx name rec_ty) value rec_ty) in
      let fix_val = Ctx.eval ctx fix_core in
      union_expr_effects ctx (ops.collect_effects (Ctx.bind ctx name rec_ty) value) (ops.collect_effects (Ctx.define ctx name rec_ty fix_val) body)
  | Surface.If { cond; then_; else_ } ->
      union_many_expr_effects ctx [ ops.collect_effects ctx cond; ops.collect_effects ctx then_; ops.collect_effects ctx else_ ]
  | Surface.Annotated { inner; typ } ->
      require_empty_effects ctx (ops.collect_effects ctx typ);
      ops.collect_effects ctx inner
  | Surface.Prod elems | Surface.ProdTy elems -> union_many_expr_effects ctx (List.map (ops.collect_effects ctx) elems)
  | Surface.Arrow (Explicitness.Implicit, Some name, a, row, b) -> (
      match surface_trait_bound_names a with
      | Some trait_names when List.for_all (fun trait_name -> NameMap.mem trait_name ctx.Ctx.traits) trait_names ->
          let type_ctx = Ctx.bind ctx name VU in
          let arg = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
          let dict_ctx =
            List.fold_left
              (fun c trait_name ->
                let trait_info = lookup_trait ctx trait_name in
                let dict_ty = trait_dict_ty ~trait_id:trait_info.trait_id trait_name [ arg ] (eval_trait_fields ctx trait_info [ arg ]) in
                let c', entry = Ctx.bind_anonymous c dict_ty in
                let evidence =
                  { evidence_trait_id = trait_info.trait_id;
                    evidence_trait_name = trait_name;
                    evidence_args = [ arg ];
                    evidence_level = entry.level;
                    evidence_ty = dict_ty }
                in
                Ctx.add_trait_evidence c' evidence)
              type_ctx trait_names
          in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects dict_ctx (ops.collect_effects dict_ctx eff)) row.effects; Option.iter (fun tail -> require_empty_effects dict_ctx (ops.collect_effects dict_ctx tail)) row.tail) row;
          require_empty_effects dict_ctx (ops.collect_effects dict_ctx b)
      | _ ->
          require_empty_effects ctx (ops.collect_effects ctx a);
          let _a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
          let ctx' = Ctx.bind ctx name a_val in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) row;
          require_empty_effects ctx' (ops.collect_effects ctx' b));
      empty_expr_effects
  | Surface.Arrow (_, name, a, row, b) ->
      require_empty_effects ctx (ops.collect_effects ctx a);
      let _a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
      let ctx' = Ctx.bind ctx (Option.value name ~default:"_") a_val in
      Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) row;
      require_empty_effects ctx' (ops.collect_effects ctx' b);
      empty_expr_effects
  | Surface.FieldAccess (e, _) | Surface.Proj (e, _) -> ops.collect_effects ctx e
  | Surface.RecordConstruct { typ; fields } ->
      union_many_expr_effects ctx (ops.collect_effects ctx typ :: List.map (fun (_, value) -> ops.collect_effects ctx value) fields)
  | Surface.Module _ | Surface.Struct _ ->
      let core, _ty = ops.infer ctx expr in
      let value = Ctx.eval ctx core in
      (match Nbe.force ctx.Ctx.metas value with
      | VModule { entries; partial = _ } ->
          module_entry_fields entries
          |> List.filter_map
               (fun (_, _, value) ->
                 match Nbe.force ctx.Ctx.metas value with
                 | VPi { effects; _ } -> Some (expr_effects_of_row_values ctx (effect_row_values ctx effects (VRigid { lvl = ctx.Ctx.lvl; spine = [] })))
                 | _ -> None)
          |> union_many_expr_effects ctx
      | VStruct { entries; _ } ->
          struct_entry_fields entries
          |> List.filter_map
               (fun (_, _, value) ->
                 match Nbe.force ctx.Ctx.metas value with
                 | VPi { effects; _ } -> Some (expr_effects_of_row_values ctx (effect_row_values ctx effects (VRigid { lvl = ctx.Ctx.lvl; spine = [] })))
                 | _ -> None)
          |> union_many_expr_effects ctx
      | _ -> empty_expr_effects)
  | Surface.Open (name, body) ->
      let ix, ty = Ctx.lookup ctx name in
      let value = Ctx.eval ctx (Var ix) in
      ops.collect_effects (open_module_value ctx ty value) body
  | Surface.RecordTypeDef { fields; body; _ } ->
      union_many_expr_effects ctx (List.map (fun (_, ty) -> ops.collect_effects ctx ty) fields @ [ ops.collect_effects ctx body ])
  | Surface.TypeDef { ctors; body; _ } ->
      union_many_expr_effects ctx (List.filter_map (fun (_, payload) -> Option.map (ops.collect_effects ctx) payload) ctors @ [ ops.collect_effects ctx body ])
  | Surface.EffectDef { name; params; ops = eff_ops; body } ->
      let _effect_id, eff, eff_ty, _elaborated_ops = elaborate_eff_family ops ctx name params eff_ops in
      let op_effects = List.concat_map (fun (op : Surface.effect_op) -> [ ops.collect_effects ctx op.input; ops.collect_effects ctx op.output ]) eff_ops in
      union_many_expr_effects ctx (op_effects @ [ ops.collect_effects (Ctx.define ctx name eff_ty eff) body ])
  | Surface.TraitDef { name; params; fields; body } ->
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let body_ctx = Ctx.add_trait (Ctx.define ctx name VU trait_ty) trait_info in
      union_many_expr_effects ctx (List.map (fun (_, ty) -> ops.collect_effects ctx ty) fields @ [ ops.collect_effects body_ctx body ])
  | Surface.ImplDef { trait_path = []; trait_name; args; fields; body } ->
      let ctx', impl_effects, _impl_name, _impl_ty, _impl_core = elaborate_impl ops ctx trait_name args fields in
      union_many_expr_effects ctx (impl_effects @ [ ops.collect_effects ctx' body ])
  | Surface.ImplDef { trait_path = _ :: _; trait_name; _ } ->
      raise (ElabError (UnknownTrait trait_name))
  | Surface.Match (scrutinee, branches) ->
      let value_branches = surface_value_branches branches in
      let effect_branches = surface_effect_branches branches in
      let scrut_core, scrut_ty = ops.infer ctx scrutinee in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let scrutinee_effects = ops.collect_effects ctx scrutinee in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let ret_ty = Ctx.raw_meta ctx in
      let value_branch_effects =
        List.map
          (fun (pat, body) ->
            let branch_ctx = refine_branch_context ctx refinement_target pat in
            let _core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
            ops.collect_effects ctx' body)
          value_branches
      in
      let effect_branch_effects =
        List.map
          (fun branch ->
            let _effect_core, _effect_value, input_ty, output_ty =
              resolve_effect_branch_operation ctx scrutinee_effects branch
            in
            let _core_pat, arg_ctx = elaborate_pat ctx branch.arg_pat input_ty in
            let cont_ty =
              VPi
                { explicitness = Explicit;
                  domain = output_ty;
                  effects = effect_row_closure_of_expr_effects arg_ctx residual;
                  codomain = { env = arg_ctx.env; body = Ctx.quote arg_ctx ret_ty } }
            in
            let branch_ctx, resume_entry = Ctx.bind_anonymous arg_ctx cont_ty in
            ops.collect_effects { branch_ctx with Ctx.resume_entry = Some resume_entry } branch.body)
          effect_branches
      in
      union_many_expr_effects ctx (residual :: value_branch_effects @ effect_branch_effects)
  | Atom _ | Var _ | Self | SelfType | Import _ -> empty_expr_effects
  | MacroDef _ | MacroCall _ -> failwith "MacroDef/MacroCall should not reach elaboration"

