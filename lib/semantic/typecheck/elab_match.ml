open Core
include Elab_error
open Elab_prelude
open Elab_validate
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_resolve
open Elab_patterns
open Elab_ops

let match_domain_of_ty ctx ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VNominal { params; constructors; _ } ->
      let ntp = List.length params in
      Core_match_compile.Nominal
        (List.map (fun (name, payload) -> (name, ntp, Option.is_some payload)) constructors)
  | VAtomTy atom_ty -> Atom atom_ty
  | VU -> Type
  | VProdTy tys -> Product (List.length tys)
  | VStruct { entries; _ } -> Record (List.map fst (visible_record_fields (struct_entry_fields entries)))
  | _ -> Unknown

let rec type_at_occurrence ctx ty (occ : Core_decision_tree.occurrence) =
  match occ with
  | OBase -> Some ty
  | OChild { parent; index } -> (
      match type_at_occurrence ctx ty parent with
      | Some parent_ty -> (
          match Nbe.force ctx.Ctx.metas parent_ty with
          | VProdTy tys -> List.nth_opt tys index
          | VNominal { params; constructors; _ } ->
              let num_type_params = List.length params in
              let payload_index = index - num_type_params in
              if payload_index = 0 then
                constructors
                |> List.find_map (fun (_, payload_opt) -> payload_opt)
                |> Option.map (fun payload_clo ->
                     Nbe.eval ctx.Ctx.metas (List.rev params @ payload_clo.env) payload_clo.body)
              else None
          | _ -> None)
      | None -> None)
  | OField { parent; name } -> (
      match type_at_occurrence ctx ty parent with
      | Some parent_ty -> (
          match Nbe.force ctx.Ctx.metas parent_ty with
          | VStruct { entries; _ } ->
              visible_record_fields (struct_entry_fields entries)
              |> fun fields -> find_record_field fields name
              |> Option.map snd
          | _ -> None)
      | None -> None)

let domain_of_occurrence ctx scrut_ty occ =
  match type_at_occurrence ctx scrut_ty occ with
  | Some ty -> match_domain_of_ty ctx ty
  | None -> Unknown


let surface_value_branches branches =
  List.filter_map (function
    | Surface.ValueBranch (pat, body) -> Some (pat, body)
    | Surface.EffectBranch _ -> None)
    branches

let core_value_branches branches =
  List.filter_map (function
    | ValueBranch (pat, body) -> Some (pat, body)
    | EffectBranch _ -> None)
    branches

type surface_effect_branch = {
  effect_path : string list;
  op : string;
  arg_pat : Surface.pat;
  body : Surface.t;
}

let surface_effect_branches branches =
  List.filter_map (function
    | Surface.ValueBranch _ -> None
    | Surface.EffectBranch { effect_path; op; arg_pat; body } ->
        Some { effect_path; op; arg_pat; body })
    branches

let refine_match_scrutinee_ty_opt ctx scrut_ty branches =
  let ty = Nbe.force ctx.Ctx.metas scrut_ty in
  match ty with
  | VNominal _ | VAtomTy _ | VProdTy _ -> Some ty
  | _ ->
      let rec find_pat = function
        | Surface.PatCon (path, name, _) -> (
            match find_nominal_template_opt ctx path name with
            | Some _ -> Some VU
            | None -> (
                match resolve_path_value_opt ctx path name with
                | Some (_, ctor_ty) -> nominal_from_constructor_type_opt ctx ctor_ty
                | None -> None))
        | Surface.PatAtom atom -> Some (VAtomTy (atom_ty_of_atom atom))
        | Surface.PatType _ -> Some VU
        | Surface.PatProd ps ->
            Some (VProdTy (List.map (fun _ -> Ctx.raw_meta ctx) ps))
        | Surface.PatRecord { typ_path; typ; _ } ->
            let record_value, ty = resolve_path_value ctx typ_path typ in
            (match Nbe.force ctx.Ctx.metas ty with
            | VU -> Some record_value
            | VStruct _ as record_ty -> Some record_ty
            | _ -> None)
        | Surface.PatStructType _ -> (
            match ty with VStruct _ -> Some ty | _ -> Some VU)
        | Surface.PatOr (lhs, rhs) -> (
            match find_pat lhs with Some _ as found -> found | None -> find_pat rhs)
        | PatWild | PatBind _ -> None
      in
      let rec find = function
        | [] -> None
        | (pat, _) :: rest -> (
            match find_pat pat with
            | Some target ->
                unify_scrutinee_ty ctx ty target;
                Some (Nbe.force ctx.Ctx.metas ty)
            | None -> find rest)
      in
      find branches

let refine_match_scrutinee_ty ctx scrut_ty branches =
  match refine_match_scrutinee_ty_opt ctx scrut_ty branches with
  | Some ty -> ty
  | None -> raise (ElabError NotANominalType)

let maybe_refine_match_scrutinee_ty ctx scrut_ty branches =
  Option.value (refine_match_scrutinee_ty_opt ctx scrut_ty branches) ~default:scrut_ty

let compile_match_exhaustiveness ~domain_of_occurrence pats =
  match Core_match_compile.compile_with_domains ~domain_of_occurrence pats with
  | _ -> None
  | exception Core_match_compile.Non_exhaustive missing -> Some missing

let check_match_exhaustive ctx scrut_ty pats =
  let domain_of_occurrence = domain_of_occurrence ctx scrut_ty in
  match compile_match_exhaustiveness ~domain_of_occurrence pats with
  | None -> ()
  | Some mp ->
      let rec pp_missing = function
        | Core_match_compile.MWild -> "_"
        | Core_match_compile.MCon (name, None) -> name
        | Core_match_compile.MCon (name, Some sub) ->
            name ^ "(" ^ pp_missing sub ^ ")"
      in
      raise (ElabError (NonExhaustive (pp_missing mp)))


let resolve_effect_branch_operation ctx scrutinee_effects branch =
  let effect_core, effect_value, input_ty, output_ty =
    resolve_perform_operation ctx ~effect_path:branch.effect_path ~op:branch.op
  in
  let adopt_matched_effect matched =
    (matched.core, matched.value, Nbe.force ctx.Ctx.metas input_ty, Nbe.force ctx.Ctx.metas output_ty)
  in
  match List.find_opt (fun performed -> Ctx.conv ctx performed.value effect_value) scrutinee_effects.effects with
  | Some matched -> adopt_matched_effect matched
  | None -> (
      let same_effect_family performed =
        match (Nbe.force ctx.Ctx.metas performed.value, Nbe.force ctx.Ctx.metas effect_value) with
        | VEffect performed_eff, VEffect branch_eff -> performed_eff.id = branch_eff.id
        | _ -> false
      in
      match List.filter same_effect_family scrutinee_effects.effects with
      | [ matched ] -> adopt_matched_effect matched
      | _ -> (effect_core, effect_value, input_ty, output_ty))

let handled_effects ctx scrutinee_effects branches =
  let handled = ref [] in
  List.iter
    (fun branch ->
      let _effect_core, effect_value, input_ty, _output_ty =
        resolve_effect_branch_operation ctx scrutinee_effects branch
      in
      let core_pat, _ = elaborate_pat ctx branch.arg_pat input_ty in
      check_match_exhaustive ctx input_ty [ core_pat ];
      (match Nbe.force ctx.Ctx.metas effect_value with
      | VEffect _ -> ()
      | _ -> raise (ElabError ExpectedEffect));
      if
        List.exists
          (fun (handled_effect, handled_op) ->
            String.equal branch.op handled_op && effect_values_match ctx effect_value handled_effect)
          !handled
      then raise (ElabError (DuplicateEffectBranch branch.op));
      handled := (effect_value, branch.op) :: !handled)
    branches;
  List.filter
    (fun eff_expr ->
      match Nbe.force ctx.Ctx.metas eff_expr.value with
      | VEffect eff ->
          List.exists (fun performed -> effect_values_match ctx performed.value eff_expr.value) scrutinee_effects.effects
          && List.for_all
               (fun (op_name, _, _) ->
                 List.exists
                   (fun (handled_effect, handled_op) ->
                     String.equal op_name handled_op && effect_values_match ctx eff_expr.value handled_effect)
                   !handled)
               eff.operations
      | _ -> false)
    scrutinee_effects.effects

let residual_effects ctx scrutinee_effects effect_branches =
  List.fold_left (fun effects handled -> remove_expr_effect ctx handled effects)
    scrutinee_effects
    (handled_effects ctx scrutinee_effects effect_branches)

let elaborate_effect_branch ops ctx ret_ty residual scrutinee_effects branch =
  let _effect_core, effect_value, input_ty, output_ty =
    resolve_effect_branch_operation ctx scrutinee_effects branch
  in
  let core_pat, arg_ctx = elaborate_pat ctx branch.arg_pat input_ty in
  let cont_ty =
    VPi
      { explicitness = Explicit;
        domain = output_ty;
        effects = effect_row_closure_of_expr_effects arg_ctx residual;
        codomain = { env = arg_ctx.env; body = Ctx.quote arg_ctx ret_ty } }
  in
  let body_ctx, resume_entry = Ctx.bind_anonymous arg_ctx cont_ty in
  let body_core = ops.check { body_ctx with Ctx.resume_entry = Some resume_entry } branch.body ret_ty in
  EffectBranch { eff = effect_value; op = branch.op; arg_pat = core_pat; body = body_core }
