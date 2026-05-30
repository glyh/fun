open Core
include Elab_error
open Elab_prelude
open Elab_validate

module Ctx = Elab_ctx.Ctx

open Elab_resolve

(** Elaborate a surface pattern against a scrutinee type, producing a core
    pattern and extending the context with bound pattern variables. *)
let rec elaborate_pat (ctx : Ctx.t) (pat : Surface.pat) (scrutinee_ty : value)
    : core_pat * Ctx.t =
  let core_pat, binders = elaborate_pat_binders ctx pat scrutinee_ty in
  let ctx' = List.fold_left (fun ctx (name, ty) -> Ctx.bind ctx name ty) ctx binders in
  (core_pat, ctx')

and elaborate_pat_binders (ctx : Ctx.t) (pat : Surface.pat)
    (scrutinee_ty : value) : core_pat * (string * value) list =
  match pat with
  | PatWild -> (CPatWild, [])
  | PatBind name -> (CPatBind, [ (name, scrutinee_ty) ])
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
  | PatRecord { typ_path; typ; fields; partial } ->
      let _record_value, record_ty = resolve_path_value ctx typ_path typ in
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
                let field_pat = Option.value pat_opt ~default:(Surface.PatBind name) in
                let core_pat, binders = elaborate_pat_binders ctx field_pat field_ty in
                ((name, core_pat) :: core_acc, binders @ binder_acc))
              ([], []) fields
          in
          (CPatRecord { fields = List.rev core_fields; partial }, List.rev binders)
      | _ -> raise (ElabError ApplyingNonFunction))
  | PatStructType { fields; partial } ->
      (match Nbe.force ctx.metas scrutinee_ty with
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
  | PatCon (path, name, sub_pats) -> (
      match Nbe.force ctx.metas scrutinee_ty with
      | VU -> (
          match find_nominal_template_opt ctx path name with
          | Some (VNominal n) ->
              if List.length sub_pats <> n.num_params then
                raise (ElabError PatternArityMismatch);
              let param_tys = List.init n.num_params (fun _ -> VU) in
              let core_param_pats, binders =
                List.fold_left2
                  (fun (pat_acc, binder_acc) sub_pat param_ty ->
                    let core_pat, sub_binders = elaborate_pat_binders ctx sub_pat param_ty in
                    (core_pat :: pat_acc, sub_binders @ binder_acc))
                  ([], []) sub_pats param_tys
              in
              (CPatNominalHead { id = n.id; name = n.name; num_params = n.num_params;
                                 param_pats = List.rev core_param_pats },
               List.rev binders)
          | Some _ -> raise (ElabError (UnknownConstructor name))
          | None when path = [] && sub_pats = [] && starts_lowercase name ->
              (CPatBind, [ (name, VU) ])
          | None -> raise (ElabError (UnknownConstructor name)))
      | _ ->
          let resolved_nominal =
            match path with
            | [] -> None
            | _ ->
                let ctor_value, _ = resolve_path_value ctx path name in
                match Nbe.force ctx.metas ctor_value with
                | VCon { nominal; _ } -> Some nominal
                | VLam _ | VPi _ -> None
                | _ -> raise (ElabError (UnknownConstructor name))
          in
          (match Nbe.force ctx.metas scrutinee_ty with
          | VNominal n ->
              Option.iter (Ctx.unify ctx scrutinee_ty) resolved_nominal;
              if path = [] && not (unqualified_constructor_in_scope ctx name scrutinee_ty) then
                raise (ElabError (UnknownConstructor name));
              (match List.find_opt (fun (cname, _) -> String.equal cname name) n.constructors with
              | Some (_, payload_opt) ->
                  let num_type_params = List.length n.params in
                  (match (sub_pats, payload_opt) with
                  | [], None -> (CPatCon (name, num_type_params, []), [])
                  | [sub_pat], Some payload_clo ->
                      let payload_ty =
                        Nbe.eval ctx.metas (List.rev n.params @ payload_clo.env) payload_clo.body in
                      let core_sub, binders = elaborate_pat_binders ctx sub_pat payload_ty in
                      (CPatCon (name, num_type_params, [core_sub]), binders)
                  | _ ->
                      raise (ElabError PatternArityMismatch))
              | None -> raise (ElabError (UnknownConstructor name)))
          | _ -> raise (ElabError NotANominalType)))
