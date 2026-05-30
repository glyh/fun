open Core
include Elab_error
open Elab_common
open Elab_validate

module Ctx = Elab_ctx.Ctx

let trait_dict_ty ?trait_id trait_name args fields =
  VTraitDict
    { trait_id = Option.value trait_id ~default:(-1);
      trait_name;
      args;
      fields }

let trait_key trait_name args =
  trait_name ^ " " ^ String.concat " " (List.map (fun _ -> "_") args)


let resolve_path_core_value ctx path name =
  match path with
  | [] ->
      let ix, ty = Ctx.lookup ctx name in
      let core = Var ix in
      (core, Ctx.eval ctx core, ty)
  | first :: rest ->
      let ix, ty = Ctx.lookup ctx first in
      let rec go current_core current_value current_ty = function
        | [] -> (current_core, current_value, current_ty)
        | segment :: rest -> (
            match Nbe.force ctx.Ctx.metas current_ty with
            | VModule { entries; partial = _ } -> (
                match List.find_opt (fun (n, _, _) -> String.equal n segment) (visible_module_fields entries) with
                | Some (_, _, field_ty) ->
                    let next_core = Dot (current_core, segment) in
                    let next_value = Nbe.dot_value current_value segment in
                    go next_core next_value field_ty rest
                | None -> raise (ElabError (UnboundVariable segment)))
            | VStruct { entries; _ } -> (
                match List.find_opt (fun (n, _, _) -> String.equal n segment) (visible_struct_members (struct_entry_fields entries)) with
                | Some (_, _, field_ty) ->
                    let next_core = Dot (current_core, segment) in
                    let next_value = Nbe.dot_value current_value segment in
                    go next_core next_value field_ty rest
                | None -> raise (ElabError (UnboundVariable segment)))
            | _ -> raise (ElabError (UnboundVariable segment)))
      in
      go (Var ix) (Ctx.eval ctx (Var ix)) ty (rest @ [ name ])

let resolve_path_value ctx path name =
  let _, value, ty = resolve_path_core_value ctx path name in
  (value, ty)

let resolve_path_value_opt ctx path name =
  match path with
  | [] -> (
      match NameMap.find_opt name ctx.Ctx.name_table with
      | Some { level; ty } ->
          let core = Var (Nbe.lvl_to_ix ctx.Ctx.lvl level) in
          Some (Ctx.eval ctx core, ty)
      | None -> None)
  | first :: rest -> (
      match NameMap.find_opt first ctx.Ctx.name_table with
      | None -> None
      | Some { level; ty } ->
          let core = Var (Nbe.lvl_to_ix ctx.Ctx.lvl level) in
          let rec go current_value current_ty = function
            | [] -> Some (current_value, current_ty)
            | segment :: rest -> (
                match Nbe.force ctx.Ctx.metas current_ty with
                | VModule { entries; partial = _ } -> (
                    match List.find_opt (fun (n, _, _) -> String.equal n segment) (visible_module_fields entries) with
                    | Some (_, _, field_ty) -> go (Nbe.dot_value current_value segment) field_ty rest
                    | None -> None)
                | VStruct { entries; _ } -> (
                    match List.find_opt (fun (n, _, _) -> String.equal n segment) (visible_struct_members (struct_entry_fields entries)) with
                    | Some (_, _, field_ty) -> go (Nbe.dot_value current_value segment) field_ty rest
                    | None -> None)
                | _ -> None)
          in
          go (Ctx.eval ctx core) ty (rest @ [ name ]))

let lookup_trait ctx name =
  match NameMap.find_opt name ctx.Ctx.traits with
  | Some info -> info
  | None -> raise (ElabError (UnknownTrait name))

let lookup_trait_by_id ctx _trait_name trait_id =
  NameMap.fold (fun _ info acc -> if info.trait_id = trait_id then Some info else acc) ctx.Ctx.traits None

let eval_trait_fields ctx trait_info args =
  List.map
    (fun (field, clo) -> (field, Nbe.eval ctx.Ctx.metas (List.rev args @ clo.env) clo.body))
    trait_info.trait_fields

let struct_trait_evidence ctx trait_info args =
  match List.map (Nbe.force ctx.Ctx.metas) args with
  | [ (VStruct { entries; _ } as self_ty) ] ->
      List.filter_map
        (function
          | StructImpl (Public, impl_ty, impl_value) -> (
              match Nbe.force ctx.Ctx.metas impl_ty with
              | VTraitDict { trait_id; args = [ impl_arg ]; _ }
                when trait_id = trait_info.trait_id && Ctx.conv ctx impl_arg self_ty ->
                  Some (Ctx.quote ctx impl_value, impl_ty)
              | _ -> None)
          | _ -> None)
        entries
  | _ -> []

let trait_evidence_matches ctx trait_info args =
  List.map
    (fun evidence -> (Var (Nbe.lvl_to_ix ctx.Ctx.lvl evidence.evidence_level), evidence.evidence_ty))
    (List.filter
       (fun evidence ->
         evidence.evidence_trait_id = trait_info.trait_id
         && List.length evidence.evidence_args = List.length args
         && List.for_all2 (Ctx.conv ctx) evidence.evidence_args args)
       ctx.Ctx.trait_evidence)
  @ struct_trait_evidence ctx trait_info args

let resolve_trait_evidence_opt ctx trait_info args =
  match trait_evidence_matches ctx trait_info args with
  | [ evidence ] -> Ok (Some evidence)
  | [] -> Ok None
  | _ -> Error (AmbiguousTraitImplementation (trait_key trait_info.trait_name args))

let resolve_trait_evidence ctx trait_info args =
  match resolve_trait_evidence_opt ctx trait_info args with
  | Ok (Some evidence) -> evidence
  | Ok None -> raise (ElabError (UnknownTrait trait_info.trait_name))
  | Error err -> raise (ElabError err)

let resolve_trait_dict_ty ctx = function
  | VTraitDict { trait_id; trait_name; args; fields } ->
      let trait_info =
        match lookup_trait_by_id ctx trait_name trait_id with
        | Some info -> info
        | None ->
            { trait_id;
              trait_name;
              trait_params = List.map (fun _ -> "_") args;
              trait_fields = List.map (fun (name, ty) -> (name, { env = ctx.Ctx.env; body = Ctx.quote ctx ty })) fields }
      in
      Some (trait_info, args, trait_dict_ty ~trait_id:trait_info.trait_id trait_info.trait_name args fields)
  | _ -> None

let add_opened_field ctx fname field_ty value =
  let ctx = Ctx.define ctx fname field_ty value in
  let ctx =
    match Nbe.force ctx.Ctx.metas value with
    | VTrait { trait_id; _ } -> (
        match Hashtbl.find_opt trait_registry trait_id with
        | Some trait_info -> Ctx.add_trait ctx trait_info
        | None -> ctx)
    | _ -> ctx
  in
  match resolve_trait_dict_ty ctx field_ty with
  | Some (trait_info, args, _) ->
      let level = ctx.Ctx.lvl - 1 in
      let evidence =
        { evidence_trait_id = trait_info.trait_id;
          evidence_trait_name = trait_info.trait_name;
          evidence_args = args;
          evidence_level = level;
          evidence_ty = field_ty }
      in
      Ctx.add_trait_evidence ctx evidence
  | None -> ctx

let add_opened_impl ctx impl_ty impl_value =
  let ctx, entry = Ctx.define_anonymous ctx impl_ty impl_value in
  match resolve_trait_dict_ty ctx impl_ty with
  | Some (trait_info, args, _) ->
      let evidence =
        { evidence_trait_id = trait_info.trait_id;
          evidence_trait_name = trait_info.trait_name;
          evidence_args = args;
          evidence_level = entry.level;
          evidence_ty = impl_ty }
      in
      Ctx.add_trait_evidence ctx evidence
  | None -> ctx

let open_module_value ctx module_ty module_value =
  match (Nbe.force ctx.Ctx.metas module_ty, Nbe.force ctx.Ctx.metas module_value) with
  | VModule { entries = type_entries; partial = _ }, VModule { entries = value_entries; partial = _ } ->
      List.fold_left2
        (fun c type_entry value_entry ->
          match type_entry, value_entry with
          | ModuleField (fname, Public, field_ty), ModuleField (vname, Public, value) when String.equal fname vname ->
              add_opened_field c fname field_ty value
          | ModuleImpl (Public, impl_ty, _), ModuleImpl (Public, _, impl_value) ->
              add_opened_impl c impl_ty impl_value
          | ModuleField (_, Private, _), ModuleField (_, Private, _)
          | ModuleImpl (Private, _, _), ModuleImpl (Private, _, _) -> c
          | _ -> c)
        ctx type_entries value_entries
  | _ -> ctx

let resolve_trait_method ctx trait_info method_name =
  match List.find_opt (fun evidence -> evidence.evidence_trait_id = trait_info.trait_id) ctx.Ctx.trait_evidence with
  | None -> raise (ElabError (UnknownTrait trait_info.trait_name))
  | Some evidence -> (
      match Nbe.force ctx.Ctx.metas evidence.evidence_ty with
      | VTraitDict { fields; _ } -> (
          match List.assoc_opt method_name fields with
          | Some method_ty -> (Dot (Var (Nbe.lvl_to_ix ctx.Ctx.lvl evidence.evidence_level), method_name), method_ty)
          | None -> raise (ElabError (UnknownTraitMethod method_name)))
      | _ -> raise (ElabError (UnknownTraitMethod method_name)))

let find_nominal_template_opt ctx path name =
  let scan_env () =
    List.find_map
      (function
        | VNominal n when String.equal n.name name -> Some (VNominal n)
        | _ -> None)
      ctx.Ctx.env
  in
  match path with
  | [] -> (
      match resolve_path_value_opt ctx [] name with
      | Some (value, _) -> (
          match Nbe.force ctx.Ctx.metas value with
          | VNominal _ as nominal -> Some nominal
          | _ -> scan_env ())
      | None -> scan_env ())
  | _ -> (
      match resolve_path_value_opt ctx path name with
      | Some (value, _) -> (
          match Nbe.force ctx.Ctx.metas value with
          | VNominal _ as nominal -> Some nominal
          | _ -> None)
      | None -> None)

let find_nominal_template ctx path name =
  match find_nominal_template_opt ctx path name with
  | Some nominal -> nominal
  | None -> raise (ElabError (UnknownConstructor name))

let nominal_from_constructor_type_opt ctx ctor_ty =
  let rec follow ty =
    match Nbe.force ctx.Ctx.metas ty with
    | VPi { codomain = b_clo; _ } ->
        follow
          (Nbe.closure_apply ctx.Ctx.metas b_clo
             (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
    | VNominal n ->
        let fresh_params = List.init (List.length n.params) (fun _ -> Ctx.raw_meta ctx) in
        Some (VNominal { n with params = fresh_params })
    | _ -> None
  in
  follow ctor_ty

let unqualified_constructor_in_scope ctx name nominal =
  let same_nominal_id ctor_nominal =
    match (Nbe.force ctx.Ctx.metas ctor_nominal, Nbe.force ctx.Ctx.metas nominal) with
    | VNominal ctor_n, VNominal n -> ctor_n.id = n.id
    | _ -> false
  in
  match resolve_path_value_opt ctx [] name with
  | Some (value, ty) -> (
      match Nbe.force ctx.Ctx.metas value with
      | VCon { name = ctor_name; nominal = ctor_nominal; _ } ->
          String.equal ctor_name name && same_nominal_id ctor_nominal
      | VLam _ | VFix _ -> (
          match nominal_from_constructor_type_opt ctx ty with
          | Some ctor_nominal -> same_nominal_id ctor_nominal
          | None -> false)
      | _ -> false)
  | None -> false

let rec insert_explicit_effect_metas ctx core ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VPi { explicitness = Explicit; codomain; _ } ->
      let arg_core = Ctx.fresh_meta ctx in
      let arg_value = Ctx.eval ctx arg_core in
      insert_explicit_effect_metas ctx
        (Ap (core, Explicit, arg_core))
        (Nbe.closure_apply ctx.Ctx.metas codomain arg_value)
  | _ -> (core, ty)

let resolve_perform_operation ctx ~effect_path ~op =
  match List.rev effect_path with
  | [] -> raise (ElabError EffectOperationPathExpected)
  | effect_name :: rev_path ->
      let path = List.rev rev_path in
      let effect_core, _effect_value, effect_ty = resolve_path_core_value ctx path effect_name in
      let effect_core, _effect_ty = insert_explicit_effect_metas ctx effect_core effect_ty in
      let effect_value = Ctx.eval ctx effect_core in
      match Nbe.force ctx.Ctx.metas effect_value with
      | VEffect eff -> (
          match List.find_opt (fun (name, _, _) -> String.equal name op) eff.operations with
          | Some (_, input, output) ->
              let env = List.rev eff.params @ input.env in
              let input_ty = Nbe.eval ctx.Ctx.metas env input.body in
              let output_ty = Nbe.eval ctx.Ctx.metas (List.rev eff.params @ output.env) output.body in
              (effect_core, VEffect eff, input_ty, output_ty)
          | None -> raise (ElabError (UnknownEffectOperation op)))
      | _ -> raise (ElabError ExpectedEffect)

let rec insert_implicit_args ctx core ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VPi { explicitness = Implicit; domain; codomain; _ } ->
      let arg_core_opt =
        match resolve_trait_dict_ty ctx domain with
        | Some (trait_info, args, _) -> (
            match resolve_trait_evidence_opt ctx trait_info args with
            | Ok (Some (core, _)) -> Some core
            | Ok None -> None
            | Error err -> raise (ElabError err))
        | None -> Some (Ctx.fresh_meta ctx)
      in
      (match arg_core_opt with
      | None -> (core, ty)
      | Some arg_core ->
          let arg_value = Ctx.eval ctx arg_core in
          insert_implicit_args ctx
            (Ap (core, Implicit, arg_core))
            (Nbe.closure_apply ctx.Ctx.metas codomain arg_value))
  | _ -> (core, ty)
