open Core
open Elab_error
module Ctx = Elab_ctx.Ctx

let nominal_from_constructor_type ctx ctor_ty =
  let rec follow ty =
    match Nbe.force ctx.Ctx.metas ty with
    | VPi { codomain = b_clo; _ } ->
        follow
          (Nbe.closure_apply ctx.Ctx.metas b_clo
             (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
    | VNominal n ->
        let fresh_params = List.init (List.length n.params) (fun _ -> Ctx.raw_meta ctx) in
        VNominal { n with params = fresh_params }
    | _ -> raise (ElabError NotANominalType)
  in
  follow ctor_ty

let unify_scrutinee_ty ctx ty target =
  match Nbe.force ctx.Ctx.metas ty with
  | VFlex { id; spine = [] } -> MetaContext.solve ctx.Ctx.metas id target
  | _ -> Ctx.unify ctx ty target

let visible_record_fields fields =
  List.filter_map
    (fun (name, kind, ty) -> if kind = Field then Some (name, ty) else None)
    fields

let visible_module_fields entries =
  let fields = module_entry_fields entries in
  validate_module_fields fields;
  List.filter (fun (_, kind, _) -> kind = Public) fields

let visible_struct_members fields =
  List.filter (fun (_, kind, _) -> kind <> Field && kind <> Private && kind <> PrivateMethod) fields

let find_record_field fields name =
  List.find_opt (fun (n, _) -> String.equal n name) fields

let rec is_type_like_value ctx value =
  match Nbe.force ctx.Ctx.metas value with
  | VU | VEffectRowTy | VAtomTy _ | VPi _ | VProdTy _ | VNominal _ | VEffect _ | VTraitDict _ | VRefTy _ -> true
  | VModule { entries; partial = true } ->
      let fields = module_entry_fields entries in
      validate_module_fields fields;
      List.for_all
        (fun (_, kind, value) ->
          match kind with
          | Public -> is_type_like_value ctx value
          | Private -> true
          | Field | Method | PrivateMethod -> false)
        fields
  | VModule { partial = false; _ } -> false
  | VStruct { entries; _ } ->
      List.for_all
        (function
          | StructField (_, kind, value) -> (
              match kind with
              | Private | PrivateMethod -> true
              | Field | Public | Method -> is_type_like_value ctx value)
          | StructImpl (_, ty, value) -> is_type_like_value ctx ty && is_type_like_value ctx value)
        entries
  | _ -> false

let check_type_like ctx ty value =
  if not (Ctx.conv ctx ty VU || is_type_like_value ctx value) then Ctx.unify ctx ty VU

let check_duplicate_names names =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun name ->
      if Hashtbl.mem seen name then raise (ElabError (DuplicateRecordField name));
      Hashtbl.replace seen name ())
    names

let check_duplicate_eff_ops ops =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (op : Surface.effect_op) ->
      if Hashtbl.mem seen op.name then raise (ElabError (DuplicateEffectOperation op.name));
      Hashtbl.replace seen op.name ())
    ops

let check_duplicate_trait_fields fields =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (name, _) ->
      if Hashtbl.mem seen name then raise (ElabError (DuplicateTraitField name));
      Hashtbl.replace seen name ())
    fields

let starts_lowercase name =
  String.length name > 0 && Char.lowercase_ascii name.[0] = name.[0]
