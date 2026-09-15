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
open Elab_ops

(** Bidirectional checking: verify [expr] against an [expected] type. *)
let check ops (ctx : Ctx.t) (expr : Syntax.t) (expected : value) : term =
  let expected = Nbe.force ctx.metas expected in
  match (expr.kind, expected) with
  | Lam _, _ when Elab_poly_arrows.lambda_has_poly expr -> ops.check ctx (Elab_poly_arrows.lambda expr) expected
  (* An implicit row parameter the term does not bind itself: bound here, as
     [~>]'s row variables are (effect-arrow-syntax). *)
  | _, VPi { explicitness = Implicit; domain; codomain; _ }
    when (match Nbe.force ctx.metas domain with VEffectRowTy -> true | _ -> false)
         && (match expr.kind with Lam ({ explicitness = Explicitness.Implicit; _ }, _) -> false | _ -> true) ->
      let ctx' = Ctx.bind ctx (Elab_poly_arrows.fresh_row ()).name domain in
      let body_expected = Nbe.closure_apply ctx.metas codomain (VRigid { lvl = ctx.lvl; spine = [] }) in
      Lam (ops.check ctx' expr body_expected)
  | Lam (param, body), VPi { explicitness; domain = a_ty; effects; codomain = b_clo } ->
      if expl_of_syntax param.explicitness <> explicitness then raise (ElabError ApplyingNonFunction);
      let ctx' = Ctx.enclosing_scope (Ctx.bind ctx param.name.name a_ty) (fun m -> ignore (Expand.map_forms_with m body)) in
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
      let body_ctx = { body_ctx with Ctx.handler_scopes = [] } in
      let since = MetaContext.count ctx.metas in
      let body_core, body_effects = collecting body_ctx (fun body_ctx -> ops.check body_ctx body body_expected) in
      let body_effects =
        discharge_local_heaps body_ctx ~since ~visible:[ Ctx.quote ctx a_ty; Ctx.quote body_ctx body_expected ] body_effects
      in
      check_effect_subset body_ctx body_effects (effect_row_values ctx effects binder);
      Lam (List.fold_left (fun acc _ -> Lam acc) body_core (List.init inserted Fun.id))
  | Match (scrutinee, branches), VPi _ ->
      let within_handler, check_escapes = escape_guard ctx in
      let hctx = with_handler ctx branches in
      let scrut_core, scrutinee_effects = collecting hctx (fun ctx -> ops.check ctx scrutinee VU) in
      let effect_branches = effect_branches_of branches in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let handled = handled_instances ctx scrutinee_effects effect_branches in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let value_branches = value_branches_of branches in
      let (value_branches', effect_branches'), body_effects =
        within_handler handled (fun () -> collecting hctx (fun ctx ->
          ( List.map
              (fun (pat, body) ->
                let branch_ctx = refine_branch_context ctx refinement_target pat in
                let core_pat, ctx' = elaborate_pat branch_ctx pat VU in
                let refined_expected = refine_branch_expected ctx refinement_target pat expected in
                let body_core = ops.check ctx' body refined_expected in
                ValueBranch (core_pat, body_core))
              value_branches,
            List.map (elaborate_effect_branch ops ~handler_ctx:hctx ctx expected residual scrutinee_effects) effect_branches )))
      in
      emit_residual ctx ~residual_of:(fun effects -> residual_effects ctx effects effect_branches) scrutinee_effects body_effects;
      check_escapes handled expected;
      check_match_exhaustive ctx VU (List.map fst (core_value_branches value_branches'));
      Match (scrut_core, value_branches' @ effect_branches')
  | Prod elems, VProdTy tys ->
      if List.length elems <> List.length tys then
        raise (ElabError TupleLengthMismatch);
      let cores = List.map2 (ops.check ctx) elems tys in
      Prod cores
  | Let { name = { name; _ }; type_; value; body; recursive }, _ ->
      discharging ctx ~visible_of:(fun _ -> [ Ctx.quote ctx expected ]) @@ fun ctx ->
      if recursive then begin
        let ty_term, fix_core, rec_ty, fix_val = Elab_infer.elab_rec_let ops ctx ~name ~type_ value in
        let ctx' = Ctx.define ctx name rec_ty fix_val in
        let body_core = ops.check ctx' body expected in
        Let (ty_term, fix_core, body_core)
      end else begin
        let (val_core, val_ty), value_effects =
          collecting ctx (fun ctx ->
            match type_ with
            | Some ty_expr ->
                let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
                let core = ops.check ctx value ty_val in
                (core, ty_val)
            | None -> ops.infer ctx value)
        in
        emit ctx value_effects;
        let gen_val_core, gen_val_ty = generalize ctx val_core val_ty in
        let ty_term = Ctx.quote ctx gen_val_ty in
        let ctx' = let_body_ctx ctx name gen_val_ty gen_val_core value_effects in
        let body_core = ops.check ctx' body expected in
        Let (ty_term, gen_val_core, body_core)
      end
  | Match (scrutinee, branches), _ ->
      let within_handler, check_escapes = escape_guard ctx in
      let hctx = with_handler ctx branches in
      let (scrut_core, scrut_ty), scrutinee_effects = collecting hctx (fun ctx -> ops.infer ctx scrutinee) in
      let value_branches = value_branches_of branches in
      let effect_branches = effect_branches_of branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let handled = handled_instances ctx scrutinee_effects effect_branches in
      let (value_branches', effect_branches'), body_effects =
        within_handler handled (fun () -> collecting hctx (fun ctx ->
          ( List.map (fun (pat, body) ->
              let branch_ctx = refine_branch_context ctx refinement_target pat in
              let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
              let refined_expected = refine_branch_expected ctx refinement_target pat expected in
              let body_core = ops.check ctx' body refined_expected in
              ValueBranch (core_pat, body_core))
              value_branches,
            List.map (elaborate_effect_branch ops ~handler_ctx:hctx ctx expected residual scrutinee_effects) effect_branches )))
      in
      emit_residual ctx ~residual_of:(fun effects -> residual_effects ctx effects effect_branches) scrutinee_effects body_effects;
      check_escapes handled expected;
      check_match_exhaustive ctx scrut_ty (List.map fst (core_value_branches value_branches'));
      Match (scrut_core, value_branches' @ effect_branches')
  (* [quote { … }] where one [Decl] is expected - a [: Decl] macro's body - is
     that one declaration; anywhere else it is the list of its items. *)
  | QuoteDecls { items; holes }, _ when Ctx.conv ctx expected (Elab_stdlib.syntax_nominals ctx).decl -> (
      match items with
      | [ item ] when not (match item with Syntax.HoleBinding _ -> true | _ -> false) ->
          let ns = Elab_stdlib.syntax_nominals ctx in
          quote_core ~check:ops.check ctx (Macro_eval.w_decl ns item) holes
      | _ -> raise (ElabError (QuoteNotOneDecl (List.length items))))
  | MacroCall ({ kind = Var { name; _ }; _ }, args), _ ->
      fst (apply_typed_macro ~check:ops.check ctx ~name args ~expected:(Some expected))
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
      else begin
        (* Against a signature, the member types are the ones it gives this
           module: [empty : T] means the module's own [T]. *)
        let expected = match expected with VSig _ -> Nbe.module_type_of ctx.metas expected (Ctx.eval ctx core) | _ -> expected in
        Ctx.unify ctx expected inferred
      end;
      core
