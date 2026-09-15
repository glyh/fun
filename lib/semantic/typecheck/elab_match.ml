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
  match Nbe.force_shape ctx.Ctx.metas ty with
  | VNominal { id; params; constructors; _ } ->
      let ntp = List.length params in
      Core_match_compile.Nominal
        (List.map (fun (name, payloads) -> (name, ntp, List.length payloads)) (nominal_constructors id constructors))
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
          | VNominal { id; params; constructors; _ } ->
              let num_type_params = List.length params in
              let payload_index = index - num_type_params in
              if payload_index >= 0 then
                nominal_constructors id constructors
                |> List.find_map (fun (_, payloads) -> List.nth_opt payloads payload_index)
                |> Option.map (fun payload_clo ->
                     Nbe.eval ctx.Ctx.metas (List.rev params @ payload_clo.env) payload_clo.body)
              else None
          | _ -> None)
      | None -> None)
  | OField { parent; name } -> (
      match type_at_occurrence ctx ty parent with
      | Some parent_ty -> (
          match Nbe.force_shape ctx.Ctx.metas parent_ty with
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


let value_branches_of branches =
  List.filter_map (function
    | Syntax.ValueBranch (pat, body) -> Some (pat, body)
    | Syntax.EffectBranch _ -> None)
    branches

let core_value_branches branches =
  List.filter_map (function
    | ValueBranch (pat, body) -> Some (pat, body)
    | EffectBranch _ -> None)
    branches

type surface_effect_branch = {
  op_path : Syntax.path;
  op : string;
  arg_pat : Syntax.pat;
  body : Syntax.t;
}

let effect_branches_of branches =
  List.filter_map (function
    | Syntax.ValueBranch _ -> None
    | Syntax.EffectBranch { op = op_path; arg_pat; body } ->
        let op = Syntax.path_last op_path in
        Some { op_path; op; arg_pat; body })
    branches

let refine_match_scrutinee_ty_opt ctx scrut_ty branches =
  let ty = Nbe.force ctx.Ctx.metas scrut_ty in
  match ty with
  | VNominal _ | VAtomTy _ | VProdTy _ -> Some ty
  | _ ->
      let rec find_pat = function
        | Syntax.PatCon (con_path, _) -> (
            (* Type name first, constructor second - see
               [find_nominal_for_pattern_head_opt]. A type-name hit means the
               pattern head is a type, so the scrutinee is [Type] itself. *)
            match find_nominal_template_opt ctx con_path with
            | Some _ -> Some VU
            | None -> find_nominal_for_pattern_head_opt ctx con_path)
        | Syntax.PatAtom atom -> Some (VAtomTy (atom_ty_of_atom atom))
        | Syntax.PatType _ -> Some VU
        | Syntax.PatProd ps ->
            (* Refine each element from its sub-pattern where possible (e.g. a
               constructor sub-pattern pins that element to its nominal type),
               falling back to a fresh meta for wildcards/binders. *)
            Some
              (VProdTy
                 (List.map
                    (fun p ->
                      match find_pat p with
                      | Some t -> t
                      | None -> Ctx.raw_meta ctx)
                    ps))
        | Syntax.PatRecord { typ = typ_p; _ } ->
            let record_value, ty = resolve_path_value ctx typ_p in
            (match Nbe.force ctx.Ctx.metas ty with
            | VU -> Some record_value
            | VStruct _ as record_ty -> Some record_ty
            | _ -> None)
        | Syntax.PatStructType _ -> (
            match ty with VStruct _ -> Some ty | _ -> Some VU)
        | Syntax.PatOr (lhs, rhs) -> (
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
    resolve_perform_operation ctx branch.op_path
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

(* The effect families a match's effect branches handle: the scope its scrutinee
   and branch bodies elaborate in, for tunneling (E5). *)
let handler_scope ctx (branches : Syntax.match_branch list) =
  effect_branches_of branches
  |> List.filter_map (fun branch ->
         let _core, value, _input, _output = resolve_perform_operation ctx branch.op_path in
         match Nbe.force ctx.Ctx.metas value with VEffect { id; _ } -> Some id | _ -> None)
  |> List.sort_uniq compare

let with_handler ctx branches =
  match handler_scope ctx branches with
  | [] -> ctx
  | scope -> { ctx with Ctx.handler_scopes = scope :: ctx.Ctx.handler_scopes }

(* E6: a match's result may not carry a function whose row names an effect
   family the match handles - the closure would escape its handler. A saved
   continuation is fine: its row is the residual, without the handled effects.
   ponytail: looks through arrows, tuples, nominal parameters, refs and struct
   fields of the result type only; an escape through an outer ref's type is not
   checked. *)
let check_handled_effects_do_not_escape ctx branches ret_ty =
  match handler_scope ctx branches with
  | [] -> ()
  | ids ->
      let metas = ctx.Ctx.metas in
      let named v =
        match Nbe.force metas v with
        | VEffect { id; name; _ } when List.mem id ids -> Some name
        | _ -> None
      in
      let rec escaping lvl ty =
        match Nbe.force metas ty with
        | VPi { domain; effects; codomain; _ } -> (
            let x = VRigid { lvl; spine = [] } in
            match List.find_map named (Nbe.eval_effect_row_closure metas effects x).effect_values with
            | Some _ as found -> found
            | None -> (
                match escaping lvl domain with
                | Some _ as found -> found
                | None -> escaping (lvl + 1) (Nbe.closure_apply metas codomain x)))
        | VProdTy tys | VNominal { params = tys; _ } -> List.find_map (escaping lvl) tys
        | VRefTy (_, elem) -> escaping lvl elem
        | VStruct { entries; _ } ->
            List.find_map (function StructField (_, _, t) -> escaping lvl t | StructImpl _ -> None) entries
        | _ -> None
      in
      Option.iter (fun name -> raise (ElabError (HandledEffectEscapes name))) (escaping ctx.Ctx.lvl ret_ty)
