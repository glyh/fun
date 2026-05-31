open Core
include Elab_error
open Elab_common
open Elab_validate
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_resolve
open Elab_refine
open Elab_patterns
open Elab_match
open Elab_generalize
open Elab_effect_collect
open Elab_ops

(** Bidirectional checking: verify [expr] against an [expected] type. *)
let check ops (ctx : Ctx.t) (expr : Surface.t) (expected : value) : term =
  let expected = Nbe.force ctx.metas expected in
  match (expr, expected) with
  | Lam (param, body), VPi { explicitness; domain = a_ty; effects; codomain = b_clo } ->
      if expl_of_surface param.explicitness <> explicitness then raise (ElabError ApplyingNonFunction);
      let ctx' = Ctx.bind ctx param.name a_ty in
      let binder = VRigid { lvl = ctx.lvl; spine = [] } in
      let rec insert_hidden_dicts ctx body_ty inserted =
        match Nbe.force ctx.Ctx.metas body_ty with
        | VPi { explicitness = Implicit; domain; codomain; _ } -> (
            match resolve_trait_dict_ty ctx domain with
            | Some (trait_info, args, dict_ty) ->
                let ctx', entry = Ctx.bind_anonymous ctx dict_ty in
                let evidence =
                  { evidence_trait_id = trait_info.trait_id;
                    evidence_trait_name = trait_info.trait_name;
                    evidence_args = args;
                    evidence_level = entry.level;
                    evidence_ty = dict_ty }
                in
                let dict_value = VRigid { lvl = entry.level; spine = [] } in
                insert_hidden_dicts (Ctx.add_trait_evidence ctx' evidence)
                  (Nbe.closure_apply ctx.Ctx.metas codomain dict_value)
                  (inserted + 1)
            | None -> (ctx, body_ty, inserted))
        | _ -> (ctx, body_ty, inserted)
      in
      let b_ty = Nbe.closure_apply ctx.metas b_clo binder in
      let body_ctx, body_expected, inserted = insert_hidden_dicts ctx' b_ty 0 in
      let body_core = ops.check body_ctx body body_expected in
      let body_effects = ops.collect_effects body_ctx body in
      check_effect_subset body_ctx body_effects (effect_row_values ctx effects binder);
      Lam (List.fold_left (fun acc _ -> Lam acc) body_core (List.init inserted Fun.id))
  | Match (scrutinee, branches), VPi _ ->
      let scrutinee_effects = ops.collect_effects ctx scrutinee in
      let effect_branches = surface_effect_branches branches in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      require_empty_effects ctx residual;
      let scrut_core = ops.check ctx scrutinee VU in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let value_branches = surface_value_branches branches in
      let value_branches' =
        List.map
          (fun (pat, body) ->
            let branch_ctx = refine_branch_context ctx refinement_target pat in
            let core_pat, ctx' = elaborate_pat branch_ctx pat VU in
            let refined_expected = refine_branch_expected ctx refinement_target pat expected in
            let body_core = ops.check ctx' body refined_expected in
            ValueBranch (core_pat, body_core))
          value_branches
      in
      let effect_branches' = List.map (elaborate_effect_branch ops ctx expected residual scrutinee_effects) effect_branches in
      check_match_exhaustive ctx VU (List.map fst (core_value_branches value_branches'));
      Match (scrut_core, value_branches' @ effect_branches')
  | If { cond; then_; else_ }, _ ->
      let cond_core = ops.check ctx cond (VAtomTy Atom_ty.TBool) in
      let then_core = ops.check ctx then_ expected in
      let else_core = ops.check ctx else_ expected in
      If (cond_core, then_core, else_core)
  | Prod elems, VProdTy tys ->
      if List.length elems <> List.length tys then
        raise (ElabError TupleLengthMismatch);
      let cores = List.map2 (ops.check ctx) elems tys in
      Prod cores
  | Let { name; type_; value; body; recursive }, _ ->
      if recursive then begin
        let rec_ty =
          match type_ with
          | Some ty_expr ->
              let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
              ty_val
          | None -> Ctx.raw_meta ctx
        in
        let ctx_with_self = Ctx.bind ctx name rec_ty in
        let val_core = ops.check ctx_with_self value rec_ty in
        let fix_core = Fix val_core in
        let fix_val = Ctx.eval ctx fix_core in
        let ty_term = Ctx.quote ctx rec_ty in
        let ctx' = Ctx.define ctx name rec_ty fix_val in
        let body_core = ops.check ctx' body expected in
        Let (ty_term, fix_core, body_core)
      end else begin
        let val_core, val_ty =
          match type_ with
          | Some ty_expr ->
              let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
              let core = ops.check ctx value ty_val in
              (core, ty_val)
          | None -> ops.infer ctx value
        in
        let gen_val_core, gen_val_ty = generalize ctx val_core val_ty in
        let ty_term = Ctx.quote ctx gen_val_ty in
        let ctx' =
          if is_empty_expr_effects (ops.collect_effects ctx value) && compile_time_safe value then Ctx.define ctx name gen_val_ty (Ctx.eval ctx gen_val_core)
          else Ctx.bind ctx name gen_val_ty
        in
        let body_core = ops.check ctx' body expected in
        Let (ty_term, gen_val_core, body_core)
      end
  | Match (scrutinee, branches), _ ->
      let scrut_core, scrut_ty = ops.infer ctx scrutinee in
      let value_branches = surface_value_branches branches in
      let effect_branches = surface_effect_branches branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let scrutinee_effects = ops.collect_effects ctx scrutinee in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      require_empty_effects ctx residual;
      let value_branches' =
        List.map (fun (pat, body) ->
          let branch_ctx = refine_branch_context ctx refinement_target pat in
          let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
          let refined_expected = refine_branch_expected ctx refinement_target pat expected in
          let body_core = ops.check ctx' body refined_expected in
          ValueBranch (core_pat, body_core))
          value_branches
      in
      let effect_branches' = List.map (elaborate_effect_branch ops ctx expected residual scrutinee_effects) effect_branches in
      check_match_exhaustive ctx scrut_ty (List.map fst (core_value_branches value_branches'));
      Match (scrut_core, value_branches' @ effect_branches')
  | _ ->
      let core, inferred = ops.infer ctx expr in
      let rec wrap_implicits core ty =
        match Nbe.force ctx.metas ty with
        | VPi { explicitness = Implicit; codomain = b_clo; _ } ->
            let meta_core = Ctx.fresh_meta ctx in
            let meta_val = Ctx.eval ctx meta_core in
            let ret_ty = Nbe.closure_apply ctx.metas b_clo meta_val in
            wrap_implicits (Ap (core, Implicit, meta_core)) ret_ty
        | _ -> (core, ty)
      in
      let core, inferred = wrap_implicits core inferred in
      if Ctx.conv ctx expected VU then
        check_type_like ctx inferred (Ctx.eval ctx core)
      else
        Ctx.unify ctx expected inferred;
      core
