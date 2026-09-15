open Core
include Elab_error
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_resolve
open Elab_refine
open Elab_patterns
open Elab_match
open Elab_defs
open Elab_generalize
open Elab_ops

let rec compile_time_safe (expr : Syntax.t) : bool =
  match expr.kind with
  | Syntax.RefNew _ | Syntax.RefGet _ | Syntax.RefSet _ -> false
  | Syntax.Atom _ | Syntax.Var _ | Syntax.Self | Syntax.SelfType | Syntax.Stx _ | Syntax.Import _ -> true
  | Syntax.Quote { holes; _ } | Syntax.QuoteDecls { holes; _ } -> List.for_all (fun (_, h) -> compile_time_safe h) holes
  | Syntax.Elaborated { form; _ } -> compile_time_safe form
  | Syntax.Ap (f, _, a) -> compile_time_safe f && compile_time_safe a
  | Syntax.Lam (_, body) -> compile_time_safe body
  | Syntax.Let { type_; value; body; _ } ->
      Option.fold ~none:true ~some:compile_time_safe type_ && compile_time_safe value && compile_time_safe body
  | Syntax.Annotated { inner; typ } -> compile_time_safe inner && compile_time_safe typ
  | Syntax.Prod elems | Syntax.ProdTy elems -> List.for_all compile_time_safe elems
  | Syntax.Arrow (_, _, a, row, b) ->
      let row_safe =
        match row with
        | None -> true
        | Some (row : Syntax.effect_row) ->
            List.for_all compile_time_safe row.effects && Option.fold ~none:true ~some:compile_time_safe row.tail
      in
      compile_time_safe a && row_safe && compile_time_safe b
  | Syntax.FieldAccess (e, _) | Syntax.Proj (e, _) -> compile_time_safe e
  | Syntax.RecordConstruct { typ; fields } ->
      compile_time_safe typ && List.for_all (fun (_, value) -> compile_time_safe value) fields
  | Syntax.Struct { bindings } | Syntax.Module { bindings } -> List.for_all compile_time_safe_struct_binding bindings
  | Syntax.Open (m, body, _) -> compile_time_safe m && compile_time_safe body
  | Syntax.OpenChoice _ -> true
  | Syntax.RecordTypeDef { fields; body; _ } ->
      List.for_all (fun (_, ty) -> compile_time_safe ty) fields && compile_time_safe body
  | Syntax.TypeDef { ctors; body; _ } ->
      List.for_all (fun (_, payloads) -> List.for_all compile_time_safe payloads) ctors && compile_time_safe body
  | Syntax.EffectDef { ops; body; _ } ->
      List.for_all (fun (op : Syntax.effect_op) -> compile_time_safe op.input && compile_time_safe op.output) ops && compile_time_safe body
  | Syntax.TraitDef { fields; body; _ } ->
      List.for_all (fun (_, ty) -> compile_time_safe ty) fields && compile_time_safe body
  | Syntax.ImplDef { args; fields; body; _ } ->
      List.for_all compile_time_safe args && List.for_all (fun (_, value) -> compile_time_safe value) fields && compile_time_safe body
  | Syntax.Perform _ | Syntax.Resume _ | Syntax.Match _ -> false
  | Syntax.MacroDef _ | Syntax.SyntaxDef _ | Syntax.MacroCall _ | Syntax.SyntaxOperatorUse _ | Syntax.Block _ | Syntax.Instantiate _ ->
      failwith "macro-only syntax should not reach elaboration"

and compile_time_safe_struct_binding = function
  | Syntax.LetBinding { value; _ } -> compile_time_safe value
  | Syntax.MethodBinding { body; _ } -> compile_time_safe body
  | Syntax.TypeBinding { members; _ } ->
      List.for_all
        (fun (m : Syntax.type_decl) -> List.for_all (fun (_, payloads) -> List.for_all compile_time_safe payloads) m.ctors)
        members
  | Syntax.RecordTypeBinding { fields; _ } -> List.for_all (fun (_, ty) -> compile_time_safe ty) fields
  | Syntax.EffectBinding { ops; _ } ->
      List.for_all (fun (op : Syntax.effect_op) -> compile_time_safe op.input && compile_time_safe op.output) ops
  | Syntax.TraitBinding { fields; _ } -> List.for_all (fun (_, ty) -> compile_time_safe ty) fields
  | Syntax.ImplBinding { args; fields; _ } ->
      List.for_all compile_time_safe args && List.for_all (fun (_, value) -> compile_time_safe value) fields
  | Syntax.MacroBinding _ | Syntax.SyntaxBinding _ | Syntax.HoleBinding _ | Syntax.Items _ | Syntax.InstantiateBinding _ -> true
  | Syntax.MacroCallBinding _ -> true
  | Syntax.PatternSynBinding _ -> true
  | Syntax.FieldBinding { type_; _ } -> compile_time_safe type_
  | Syntax.OpenBinding (m, _) -> compile_time_safe m

let collect_effects ops (ctx : Ctx.t) (expr : Syntax.t) : expr_effects =
  match expr.kind with
  | Syntax.Perform { op = op_path; arg } ->
      let effect_core, effect_value, input_ty, _output_ty = resolve_perform_operation ctx op_path in
      let _arg_core = ops.check ctx arg input_ty in
      union_expr_effects ctx (ops.collect_effects ctx arg) (singleton_expr_effect effect_core effect_value)
  | Syntax.Resume arg -> ops.collect_effects ctx arg
  | Syntax.RefNew e | Syntax.RefGet e -> ops.collect_effects ctx e
  | Syntax.RefSet (r, e) -> union_expr_effects ctx (ops.collect_effects ctx r) (ops.collect_effects ctx e)
  | Syntax.Ap (f, Explicitness.Explicit, a) ->
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
  | Syntax.Ap (f, Explicitness.Implicit, a) ->
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
  | Syntax.Lam _ -> empty_expr_effects
  | Syntax.Let { name = { name; _ }; type_; value; body; recursive = false } ->
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
  | Syntax.Let { name = { name; _ }; type_; value; body; recursive = true } ->
      let rec_ty =
        match type_ with
        | Some ty_expr ->
            require_empty_effects ctx (ops.collect_effects ctx ty_expr);
            let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
            ty_val
        | None -> Ctx.raw_meta ctx
      in
      let fix_body = ops.check (Ctx.bind ctx name rec_ty) value rec_ty in
      let fix_core = Fix (name, Ctx.pure_call ctx rec_ty, fix_body) in
      let fix_val = Ctx.eval ctx fix_core in
      union_expr_effects ctx (ops.collect_effects (Ctx.bind ctx name rec_ty) value) (ops.collect_effects (Ctx.define ctx name rec_ty fix_val) body)
  | Syntax.Annotated { inner; typ } ->
      require_empty_effects ctx (ops.collect_effects ctx typ);
      ops.collect_effects ctx inner
  | Syntax.Prod elems | Syntax.ProdTy elems -> union_many_expr_effects ctx (List.map (ops.collect_effects ctx) elems)
  | Syntax.Arrow (Explicitness.Implicit, Some { name; _ }, a, row, b) -> (
      match trait_bounds_opt ctx a with
      | Some trait_infos ->
          let dict_ctx, _dict_layers = bind_trait_bound_dicts ctx name trait_infos in
          Option.iter (fun (row : Syntax.effect_row) -> List.iter (fun eff -> require_empty_effects dict_ctx (ops.collect_effects dict_ctx eff)) row.effects; Option.iter (fun tail -> require_empty_effects dict_ctx (ops.collect_effects dict_ctx tail)) row.tail) row;
          require_empty_effects dict_ctx (ops.collect_effects dict_ctx b)
      | _ ->
          require_empty_effects ctx (ops.collect_effects ctx a);
          let _a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
          let ctx' = Ctx.bind ctx name a_val in
          Option.iter (fun (row : Syntax.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) row;
          require_empty_effects ctx' (ops.collect_effects ctx' b));
      empty_expr_effects
  | Syntax.Arrow (_, name, a, row, b) ->
      require_empty_effects ctx (ops.collect_effects ctx a);
      let _a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
      let ctx' = Ctx.bind ctx (Option.fold ~none:"_" ~some:(fun (i : Syntax.id) -> i.name) name) a_val in
      Option.iter (fun (row : Syntax.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) row;
      require_empty_effects ctx' (ops.collect_effects ctx' b);
      empty_expr_effects
  | Syntax.FieldAccess (e, _) | Syntax.Proj (e, _) -> ops.collect_effects ctx e
  | Syntax.RecordConstruct { typ; fields } ->
      union_many_expr_effects ctx (ops.collect_effects ctx typ :: List.map (fun (_, value) -> ops.collect_effects ctx value) fields)
  | Syntax.Module _ | Syntax.Struct _ ->
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
  | Syntax.Open (mod_expr, body, label) ->
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      let mod_value = Ctx.eval ctx mod_core in
      (match (Nbe.force ctx.Ctx.metas mod_ty, Nbe.force ctx.Ctx.metas mod_value) with
       | VModule _, VModule _ ->
           union_many_expr_effects ctx
             [ ops.collect_effects ctx mod_expr;
               ops.collect_effects (open_module_value ~label ctx mod_ty mod_value) body ]
       | _ -> ops.collect_effects ctx body)
  | Syntax.RecordTypeDef { fields; body; _ } ->
      union_many_expr_effects ctx (List.map (fun (_, ty) -> ops.collect_effects ctx ty) fields @ [ ops.collect_effects ctx body ])
  | Syntax.TypeDef { ctors; body; _ } ->
      union_many_expr_effects ctx (List.concat_map (fun (_, payloads) -> List.map (ops.collect_effects ctx) payloads) ctors @ [ ops.collect_effects ctx body ])
  | Syntax.EffectDef { name = { name; _ }; params; ops = eff_ops; body } ->
      let params = Syntax.names params in
      let _effect_id, eff, eff_ty, _elaborated_ops = elaborate_eff_family ops ctx name params eff_ops in
      let op_effects = List.concat_map (fun (op : Syntax.effect_op) -> [ ops.collect_effects ctx op.input; ops.collect_effects ctx op.output ]) eff_ops in
      union_many_expr_effects ctx (op_effects @ [ ops.collect_effects (Ctx.define ctx name eff_ty eff) body ])
  | Syntax.TraitDef { name = { name; _ }; params; fields; body } ->
      let params = Syntax.names params in
      let _trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let body_ctx = Ctx.define ctx name VU trait_ty in
      union_many_expr_effects ctx (List.map (fun (_, ty) -> ops.collect_effects ctx ty) fields @ [ ops.collect_effects body_ctx body ])
  | Syntax.ImplDef { name; trait; args; fields; body } ->
      let impl_name = Option.map (fun (i : Syntax.id) -> i.name) name in
      let ctx', impl_effects, _evidence, _impl_ty, _impl_core =
        elaborate_impl ?impl_name ops ctx trait args fields in
      union_many_expr_effects ctx (impl_effects @ [ ops.collect_effects ctx' body ])
  | Syntax.Match (scrutinee, branches) ->
      let value_branches = value_branches_of branches in
      let effect_branches = effect_branches_of branches in
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
  | Atom _ | Var _ | OpenChoice _ | Self | SelfType | Stx _ | Import _ -> empty_expr_effects
  | Elaborated { form; _ } -> ops.collect_effects ctx form
  | Quote { holes; _ } | QuoteDecls { holes; _ } -> union_many_expr_effects ctx (List.map (fun (_, h) -> ops.collect_effects ctx h) holes)
  | MacroDef _ | SyntaxDef _ | MacroCall _ | SyntaxOperatorUse _ | Block _ | Instantiate _ -> failwith "macro-only syntax should not reach elaboration"
