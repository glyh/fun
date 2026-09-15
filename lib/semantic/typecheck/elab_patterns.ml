open Core
include Elab_error
open Elab_prelude
open Elab_validate

module Ctx = Elab_ctx.Ctx

open Elab_resolve

let rec subst_syn_param (param_name : string) (replacement : core_pat) (pat : core_pat) : core_pat =
  match pat with
  | CPatBind -> replacement
  | CPatWild | CPatAtom _ | CPatType _ -> pat
  | CPatCon (n, ntp, sub_pats) -> CPatCon (n, ntp, List.map (subst_syn_param param_name replacement) sub_pats)
  | CPatSyn { name; sub_pats; rhs } ->
      CPatSyn { name; sub_pats = List.map (subst_syn_param param_name replacement) sub_pats;
                rhs = subst_syn_param param_name replacement rhs }
  | CPatProd sub_pats -> CPatProd (List.map (subst_syn_param param_name replacement) sub_pats)
  | CPatOr (lhs, rhs) -> CPatOr (subst_syn_param param_name replacement lhs,
                                  subst_syn_param param_name replacement rhs)
  | CPatRecord { fields; partial } ->
      CPatRecord { fields = List.map (fun (n, p) -> (n, subst_syn_param param_name replacement p)) fields;
                   partial }
  | CPatStructType { fields; partial } ->
      CPatStructType { fields = List.map (fun (n, p) -> (n, subst_syn_param param_name replacement p)) fields;
                       partial }
  | CPatNominalHead { id; name = n; num_params; param_pats } ->
      CPatNominalHead { id; name = n; num_params;
                        param_pats = List.map (subst_syn_param param_name replacement) param_pats }

let subst_syn_params (_params : string list) (replacements : core_pat list) (rhs : core_pat) : core_pat =
  let counter = ref 0 in
  let rec subst_positional pat =
    match pat with
    | CPatBind ->
        let idx = !counter in
        incr counter;
        if idx < List.length replacements then List.nth replacements idx
        else pat
    | CPatWild | CPatAtom _ | CPatType _ -> pat
    | CPatCon (n, ntp, sub_pats) -> CPatCon (n, ntp, List.map subst_positional sub_pats)
    | CPatSyn { name; sub_pats; rhs } ->
        CPatSyn { name; sub_pats = List.map subst_positional sub_pats;
                  rhs = subst_positional rhs }
    | CPatProd sub_pats -> CPatProd (List.map subst_positional sub_pats)
    | CPatOr (lhs, rhs) -> CPatOr (subst_positional lhs, subst_positional rhs)
    | CPatRecord { fields; partial } ->
        CPatRecord { fields = List.map (fun (n, p) -> (n, subst_positional p)) fields; partial }
    | CPatStructType { fields; partial } ->
        CPatStructType { fields = List.map (fun (n, p) -> (n, subst_positional p)) fields; partial }
    | CPatNominalHead { id; name = n; num_params; param_pats } ->
        CPatNominalHead { id; name = n; num_params;
                          param_pats = List.map subst_positional param_pats }
  in
  subst_positional rhs

let rec pattern_binder_types ctx scrutinee_ty = function
  | CPatBind -> [ scrutinee_ty ]
  | CPatWild | CPatAtom _ | CPatType _ -> []
  | CPatCon (name, _num_type_params, sub_pats) -> (
      match Nbe.force ctx.Ctx.metas scrutinee_ty with
      | VNominal n -> (
          match List.find_opt (fun (cname, _) -> String.equal cname name) (nominal_constructors n.id n.constructors) with
          | Some (_, payloads) ->
              let payload_tys =
                List.map
                  (fun payload_clo ->
                    Nbe.eval ctx.Ctx.metas
                      (List.rev n.params @ payload_clo.env)
                      payload_clo.body)
                  payloads
              in
              if List.length sub_pats <> List.length payload_tys then
                raise (ElabError PatternArityMismatch);
              List.concat
                (List.map2 (pattern_binder_types ctx) payload_tys sub_pats)
          | None -> [])
      | _ -> [])
  | CPatNominalHead { param_pats; _ } ->
      List.concat
        (List.map (pattern_binder_types ctx VU) param_pats)
  | CPatProd sub_pats -> (
      match Nbe.force ctx.Ctx.metas scrutinee_ty with
      | VProdTy tys ->
          if List.length sub_pats <> List.length tys then
            raise (ElabError TupleLengthMismatch);
          List.concat (List.map2 (pattern_binder_types ctx) tys sub_pats)
      | _ -> [])
  | CPatRecord { fields; _ } -> (
      match Nbe.force_shape ctx.Ctx.metas scrutinee_ty with
      | VStruct { entries; _ } ->
          let record_fields = visible_record_fields (struct_entry_fields entries) in
          List.concat
            (List.filter_map
               (fun (name, pat) ->
                 match find_record_field record_fields name with
                 | Some (_, field_ty) -> Some (pattern_binder_types ctx field_ty pat)
                 | None -> None)
               fields)
      | _ -> [])
  | CPatStructType { fields; _ } ->
      List.concat
        (List.map (fun (_, pat) -> pattern_binder_types ctx VU pat) fields)
  | CPatOr (lhs, rhs) ->
      let lhs_types = pattern_binder_types ctx scrutinee_ty lhs in
      let rhs_types = pattern_binder_types ctx scrutinee_ty rhs in
      if List.length lhs_types = List.length rhs_types then lhs_types else rhs_types
  | CPatSyn { rhs; _ } -> pattern_binder_types ctx scrutinee_ty rhs

(** Elaborate a pattern against a scrutinee type, producing a core
    pattern and extending the context with bound pattern variables. *)
let rec elaborate_pat (ctx : Ctx.t) (pat : Syntax.pat) (scrutinee_ty : value)
    : core_pat * Ctx.t =
  let core_pat, binders = elaborate_pat_binders ctx pat scrutinee_ty in
  let ctx' = List.fold_left (fun ctx (name, ty) -> Ctx.bind ctx name ty) ctx binders in
  (core_pat, ctx')

and elaborate_pat_binders (ctx : Ctx.t) (pat : Syntax.pat)
    (scrutinee_ty : value) : core_pat * (string * value) list =
  match pat with
  | PatWild -> (CPatWild, [])
  | PatBind { name; _ } -> (CPatBind, [ (name, scrutinee_ty) ])
  | PatAtom atom ->
      Ctx.unify ctx scrutinee_ty (VAtomTy (atom_ty_of_atom atom));
      (CPatAtom atom, [])
  | PatType atom_ty ->
      Ctx.unify ctx scrutinee_ty VU;
      (CPatType atom_ty, [])
  | PatOr (lhs, rhs) ->
      let lhs_core, lhs_binders = elaborate_pat_binders ctx lhs scrutinee_ty in
      let rhs_core, rhs_binders = elaborate_pat_binders ctx rhs scrutinee_ty in
      if List.length lhs_binders <> List.length rhs_binders then
        raise (ElabError PatternBindingMismatch);
      List.iter2
        (fun (lhs_name, lhs_ty) (rhs_name, rhs_ty) ->
          if not (String.equal lhs_name rhs_name) then
            raise (ElabError PatternBindingMismatch);
          Ctx.unify ctx lhs_ty rhs_ty)
        lhs_binders rhs_binders;
      (CPatOr (lhs_core, rhs_core), lhs_binders)
  | PatProd sub_pats -> (
      match Nbe.force ctx.metas scrutinee_ty with
      | VProdTy tys ->
          if List.length sub_pats <> List.length tys then
            raise (ElabError TupleLengthMismatch);
          let core_subs, binders =
            List.fold_left2
              (fun (core_acc, binder_acc) pat ty ->
                let core_pat, binders = elaborate_pat_binders ctx pat ty in
                (core_pat :: core_acc, binders @ binder_acc))
              ([], []) sub_pats tys
          in
          (CPatProd (List.rev core_subs), List.rev binders)
      | _ -> raise (ElabError TupleLengthMismatch))
  | PatRecord { typ = typ_p; fields; partial } ->
      let _record_value, record_ty = resolve_path_value ctx typ_p in
      Ctx.unify ctx scrutinee_ty record_ty;
      (match Nbe.force ctx.metas record_ty with
      | VStruct { entries = struct_entries; _ } ->
          let record_fields = visible_record_fields (struct_entry_fields struct_entries) in
          check_duplicate_names (List.map fst fields);
          List.iter
            (fun (name, _) ->
              if Option.is_none (find_record_field record_fields name) then
                raise (ElabError (UnknownRecordField name)))
            fields;
          if not partial then
            List.iter
              (fun (name, _) ->
                if Option.is_none (List.assoc_opt name fields) then
                  raise (ElabError (MissingRecordField name)))
              record_fields;
          let core_fields, binders =
            List.fold_left
              (fun (core_acc, binder_acc) (name, pat_opt) ->
                let field_ty =
                  match find_record_field record_fields name with
                  | Some (_, ty) -> ty
                  | None -> raise (ElabError (UnknownRecordField name))
                in
                let field_pat = Option.value pat_opt ~default:(Syntax.PatBind (Syntax.fresh_id name)) in
                let core_pat, binders = elaborate_pat_binders ctx field_pat field_ty in
                ((name, core_pat) :: core_acc, binders @ binder_acc))
              ([], []) fields
          in
          (CPatRecord { fields = List.rev core_fields; partial }, List.rev binders)
      | _ -> raise (ElabError ApplyingNonFunction))
  | PatStructType { fields; partial } ->
      (match Nbe.force_shape ctx.metas scrutinee_ty with
      | VStruct _ -> ()
      | _ -> Ctx.unify ctx scrutinee_ty VU);
      check_duplicate_names (List.map fst fields);
      let core_fields, binders =
        List.fold_left
          (fun (core_acc, binder_acc) (name, field_pat) ->
            let core_pat, binders = elaborate_pat_binders ctx field_pat VU in
            ((name, core_pat) :: core_acc, binders @ binder_acc))
          ([], []) fields
      in
      (CPatStructType { fields = List.rev core_fields; partial }, List.rev binders)
  | PatCon (con_path, sub_pats) -> (
      let name = Syntax.path_last con_path in      match Nbe.force ctx.metas scrutinee_ty with
      | VU -> (
        let resolve =
            match find_nominal_for_pattern_head_opt ctx con_path with
            | Some (VNominal n) ->
                let ctor_params =
                  match List.find_opt (fun (cname, _) -> String.equal cname name) (nominal_constructors n.id n.constructors) with
                  | Some (_, params) -> List.length params
                  | None -> n.num_params
                in
                Some (n.id, n.name, ctor_params)
            | _ -> None
          in
          match resolve with
          | Some (id, nm, ctor_params) ->
              if List.length sub_pats <> ctor_params then
                raise (ElabError PatternArityMismatch);
              let param_tys = List.init ctor_params (fun _ -> VU) in
              let core_param_pats, binders =
                List.fold_left2
                  (fun (pat_acc, binder_acc) sub_pat param_ty ->
                    let core_pat, sub_binders = elaborate_pat_binders ctx sub_pat param_ty in
                    (core_pat :: pat_acc, sub_binders @ binder_acc))
                  ([], []) sub_pats param_tys
              in
              (CPatNominalHead { id; name = nm; num_params = ctor_params;
                                 param_pats = List.rev core_param_pats },
               List.rev binders)
          | None when con_path.members = [] && sub_pats = [] && starts_lowercase name ->
              (CPatBind, [ (name, VU) ])
          | None ->
              (match resolve_path_value_opt ctx con_path with
               | Some (syn_val, _) ->
                   (match Nbe.force ctx.metas syn_val with
                     | VPatternSyn { rhs; params; scrutinee_ty = syn_ty; _ } ->
                         if List.length sub_pats <> List.length params then
                           raise (ElabError PatternArityMismatch);
                         let core_subs, binders = elaborate_pattern_syn_args ctx sub_pats syn_ty rhs in
                         let expanded = subst_syn_params params (List.rev core_subs) rhs in
                         (expanded, List.rev binders)
                    | _ -> raise (ElabError (UnknownConstructor name)))
               | None -> raise (ElabError (UnknownConstructor name))))
      | _ ->
          (match con_path.members with
           | [] -> None
           | _ ->
               let ctor_value, _ = resolve_path_value ctx con_path in
               match Nbe.force ctx.metas ctor_value with
                | VPatternSyn { name = _; params; rhs; scrutinee_ty = syn_ty } ->
                    if List.length sub_pats <> List.length params then
                      raise (ElabError PatternArityMismatch);
                    Ctx.unify ctx scrutinee_ty syn_ty;
                    let core_subs, binders = elaborate_pattern_syn_args ctx sub_pats syn_ty rhs in
                    let expanded = subst_syn_params params (List.rev core_subs) rhs in
                    Some (expanded, List.rev binders)
               | _ -> None)
          |> function
          | Some result -> result
          | None ->
          let resolved_nominal =
            match con_path.members with
            | [] -> None
            | _ ->
                let ctor_value, _ = resolve_path_value ctx con_path in
                match Nbe.force ctx.metas ctor_value with
                | VCon { nominal; _ } -> Some nominal
                | VLam _ | VPi _ -> None
                | _ -> raise (ElabError (UnknownConstructor name))
          in
          (match Nbe.force ctx.metas scrutinee_ty with
          | VNominal n ->
              Option.iter (Ctx.unify ctx scrutinee_ty) resolved_nominal;
              (match List.find_opt (fun (cname, _) -> String.equal cname name) (nominal_constructors n.id n.constructors) with
              | Some (_, payloads) ->
                  let num_type_params = List.length n.params in
                  if List.length sub_pats <> List.length payloads then
                    raise (ElabError PatternArityMismatch);
                  let core_subs, binders =
                    List.fold_left2
                      (fun (core_acc, binder_acc) sub_pat payload_clo ->
                        let payload_ty =
                          Nbe.eval ctx.metas (List.rev n.params @ payload_clo.env) payload_clo.body in
                        let core_sub, binders = elaborate_pat_binders ctx sub_pat payload_ty in
                        (core_sub :: core_acc, binders @ binder_acc))
                      ([], []) sub_pats payloads
                  in
                  (CPatCon (name, num_type_params, List.rev core_subs), List.rev binders)
              | None -> raise (ElabError (UnknownConstructor name)))
          | _ -> raise (ElabError NotANominalType)))

and elaborate_pattern_syn_args ctx sub_pats syn_ty rhs =
  let expected_tys = pattern_binder_types ctx syn_ty rhs in
  if List.length sub_pats <> List.length expected_tys then
    raise (ElabError PatternArityMismatch);
  List.fold_left2
    (fun (pats, binds) sub_pat expected_ty ->
      let core_p, bs = elaborate_pat_binders ctx sub_pat expected_ty in
      (core_p :: pats, bs @ binds))
    ([], []) sub_pats expected_tys
