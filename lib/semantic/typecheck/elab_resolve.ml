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

(* Follow a path (M12): its head's entry, located through the head's open
   choice or resolved name, then each member through the module or struct type
   it lands on. [Error] carries the segment that was not found. *)
let resolve_path_result ctx (p : Syntax.path) : (term * value * value, string) Stdlib.result =
  match Ctx.lookup_head_opt ctx p with
  | None -> Error p.head.name
  | Some (ix, ty) ->
      let rec go core value ty = function
        | [] -> Ok (core, value, ty)
        | segment :: rest -> (
            let members =
              match Nbe.force ctx.Ctx.metas ty with
              | VModule { entries; partial = _ } -> visible_module_fields entries
              | VStruct { entries; _ } -> visible_struct_members (struct_entry_fields entries)
              | _ -> []
            in
            match find_field_last (fun (n, _, _) -> String.equal n segment) members with
            | Some (_, _, field_ty) -> go (Dot (core, segment)) (Nbe.dot_value value segment) field_ty rest
            | None -> Error segment)
      in
      go (Var ix) (Ctx.eval ctx (Var ix)) ty p.members

let resolve_path_core_value ctx p =
  match resolve_path_result ctx p with
  | Ok found -> found
  | Error segment -> raise (ElabError (UnboundVariable segment))

let resolve_path_value ctx p =
  let _, value, ty = resolve_path_core_value ctx p in
  (value, ty)

let resolve_path_value_opt ctx p =
  Result.to_option (Result.map (fun (_, value, ty) -> (value, ty)) (resolve_path_result ctx p))

(** Run a type-aware macro call whose result type [ty] is already unified with
    the annotation's constraint: apply the macro to [ty] and its syntax
    arguments, expand the output in place like every macro's output (M6), and
    hand it to [elaborate]. All of it is one call under the evaluation budget
    (M5), so type-aware calls in the output spend from the same request. A
    result that is not syntax is an error naming the macro, never a hole. *)
let run_type_aware_macro (runtime : Ctx.macro_runtime) ~name macro_fn macro_nominals ty args elaborate =
  let wrapped_ty =
    match macro_nominals with
    | Some nominals ->
        VCon { name = Compiler_names.Constructor_name.r_expr; spine = [ty]; nominal = nominals.Macro_eval.r_ }
    | None -> ty
  in
  runtime.Ctx.macro_application ~name (fun () ->
    let app = runtime.application () in
    let fn = runtime.run_macro macro_fn wrapped_ty in
    let fn = List.fold_left (fun fn arg ->
      match arg with
      | { Syntax.kind = Syntax.Stx stx_arg; _ } ->
          runtime.run_macro fn (Macro_eval.wrap_stx ~nominals:macro_nominals (app.Expand.receive stx_arg))
      | _ -> fn) fn args in
    match Macro_eval.unwrap_stx ?nominals:macro_nominals fn with
    | Some expanded -> elaborate (runtime.expand (app.emit expanded))
    | None -> raise (ElabError (MacroDidNotReturnSyntax name)))

(* A trait is located through the entry its path resolves to, by the identity
   that entry's value carries - never by the name it was written with. *)
let trait_of_path_opt ctx (p : Syntax.path) =
  Option.bind (resolve_path_value_opt ctx p) (fun (value, _) ->
      match Nbe.force ctx.Ctx.metas value with
      | VTrait { trait_id; _ } -> Hashtbl.find_opt trait_registry trait_id
      | _ -> None)

let lookup_trait ctx (p : Syntax.path) =
  match trait_of_path_opt ctx p with
  | Some info -> info
  | None -> raise (ElabError (UnknownTrait (Syntax.path_last p)))

(* A form naming a trait - [Eq], [M.Eq] - as the trait it names, if it does. *)
let trait_of_form_opt ctx (form : Syntax.t) = Option.bind (Syntax.path_of_form form) (trait_of_path_opt ctx)

(* The traits of a bound sugar [A : Eq + Show], when every summand names one. *)
let trait_bounds_opt ctx (expr : Syntax.t) =
  List.fold_right
    (fun form acc -> Option.bind acc (fun infos -> Option.map (fun i -> i :: infos) (trait_of_form_opt ctx form)))
    (Elab_syntax_util.trait_bound_forms expr) (Some [])

let eval_trait_fields ctx trait_info args =
  List.map
    (fun (field, clo) -> (field, Nbe.eval ctx.Ctx.metas (List.rev args @ clo.env) clo.body))
    trait_info.trait_fields

(* [[A : Eq + Show] -> …]: bind [A], then one dictionary per bound as evidence
   for [A]. The context under the dictionaries, and each dictionary's type. *)
let bind_trait_bound_dicts ctx name trait_infos =
  let arg = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
  List.fold_left
    (fun (c, layers) trait_info ->
      let dict_ty =
        trait_dict_ty ~trait_id:trait_info.trait_id trait_info.trait_name [ arg ] (eval_trait_fields ctx trait_info [ arg ])
      in
      let dict_core = Ctx.quote c dict_ty in
      let c', entry = Ctx.bind_anonymous c dict_ty in
      let evidence =
        { evidence_trait_id = trait_info.trait_id;
          evidence_trait_name = trait_info.trait_name;
          evidence_args = [ arg ];
          evidence_level = entry.level;
          evidence_ty = dict_ty }
      in
      (Ctx.add_trait_evidence c' evidence, layers @ [ dict_core ]))
    (Ctx.bind ctx name VU, []) trait_infos

let struct_trait_evidence ctx trait_info args =
  match List.map (Nbe.force ctx.Ctx.metas) args with
  | [ (VStruct { entries; _ } as self_ty) ] ->
      List.filter_map
        (function
          | StructImpl (_, Public, impl_ty, impl_value) -> (
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
  (* The trait IS known here - it was looked up to get [trait_info]. What is
     missing is an impl. Reusing [UnknownTrait] for this sent the
     impl-visibility investigation looking for a trait that was in scope the
     whole time. *)
  | Ok None -> raise (ElabError (MissingTraitImplementation (trait_key trait_info.trait_name args)))
  | Error err -> raise (ElabError err)

let resolve_trait_dict_ty ctx = function
  | VTraitDict { trait_id; trait_name; args; fields } ->
      let trait_info =
        match Hashtbl.find_opt trait_registry trait_id with
        | Some info -> info
        | None ->
            { trait_id;
              trait_name;
              trait_params = List.map (fun _ -> "_") args;
              trait_fields = List.map (fun (name, ty) -> (name, { env = ctx.Ctx.env; body = Ctx.quote ctx ty })) fields }
      in
      Some (trait_info, args, trait_dict_ty ~trait_id:trait_info.trait_id trait_info.trait_name args fields)
  | _ -> None

(* The value an evidence entry stands for. Entries are addressed by level; the
   environment is most-recent-first. *)
let evidence_value ctx evidence =
  List.nth ctx.Ctx.env (ctx.Ctx.lvl - evidence.evidence_level - 1)

(* [open] is idempotent: opening the same module twice brings the same impl into
   consideration twice, and two copies of one impl are not an ambiguity. Identity
   is the impl value itself, so two *different* impls for the same trait and
   arguments still collide, which is the report worth making. *)
let duplicate_impl_evidence ctx trait_info args impl_value =
  List.exists
    (fun ev ->
      ev.evidence_trait_id = trait_info.trait_id
      && List.length ev.evidence_args = List.length args
      && List.for_all2 (Ctx.conv ctx) ev.evidence_args args
      && Ctx.conv ctx (evidence_value ctx ev) impl_value)
    ctx.Ctx.trait_evidence

let add_opened_field ctx fname field_ty value =
  let ctx = Ctx.define ctx fname field_ty value in
  match resolve_trait_dict_ty ctx field_ty with
  | Some (trait_info, args, _) when not (duplicate_impl_evidence ctx trait_info args value) ->
      let level = ctx.Ctx.lvl - 1 in
      let evidence =
        { evidence_trait_id = trait_info.trait_id;
          evidence_trait_name = trait_info.trait_name;
          evidence_args = args;
          evidence_level = level;
          evidence_ty = field_ty }
      in
      Ctx.add_trait_evidence ctx evidence
  | Some _ | None -> ctx

let add_opened_impl ctx impl_ty impl_value =
  (* The entry is pushed either way - [Nbe] widens the environment once per
     public impl, and the two sides must agree on the count. Only the evidence
     is deduplicated. *)
  let duplicate =
    match resolve_trait_dict_ty ctx impl_ty with
    | Some (trait_info, args, _) -> duplicate_impl_evidence ctx trait_info args impl_value
    | None -> false
  in
  let ctx, entry = Ctx.define_anonymous ctx impl_ty impl_value in
  if duplicate then ctx
  else
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

let open_module_value ~label ctx module_ty module_value =
  match (Nbe.force ctx.Ctx.metas module_ty, Nbe.force ctx.Ctx.metas module_value) with
  | VModule { entries = type_entries; partial = _ }, VModule { entries = value_entries; partial = _ } ->
      let members = ref NameMap.empty in
      let ctx =
      List.fold_left2
        (fun c type_entry value_entry ->
          match type_entry, value_entry with
          | ModuleField (fname, Public, field_ty), ModuleField (vname, Public, value) when String.equal fname vname ->
              members := NameMap.add fname { level = c.Ctx.lvl; ty = field_ty } !members;
              add_opened_field c fname field_ty value
          | ModuleImpl (_, Public, impl_ty, _), ModuleImpl (_, Public, _, impl_value) ->
              add_opened_impl c impl_ty impl_value
          | ModuleField (_, Private, _), ModuleField (_, Private, _)
          | ModuleImpl (_, Private, _, _), ModuleImpl (_, Private, _, _) -> c
          | _ -> c)
        ctx type_entries value_entries
      in
      { ctx with Ctx.opened = (label, !members) :: ctx.Ctx.opened }
  | _ -> ctx

let resolve_trait_method ctx trait_info method_name =
  match List.find_opt (fun evidence -> evidence.evidence_trait_id = trait_info.trait_id) ctx.Ctx.trait_evidence with
  | None -> raise (ElabError (MissingTraitImplementation trait_info.trait_name))
  | Some evidence -> (
      match Nbe.force ctx.Ctx.metas evidence.evidence_ty with
      | VTraitDict { fields; _ } -> (
          match List.assoc_opt method_name fields with
          | Some method_ty -> (Dot (Var (Nbe.lvl_to_ix ctx.Ctx.lvl evidence.evidence_level), method_name), method_ty)
          | None -> raise (ElabError (UnknownTraitMethod method_name)))
      | _ -> raise (ElabError (UnknownTraitMethod method_name)))

(* The nominal type a path names, located through the entry it resolves to. A
   parametric type's entry is its type former, applied here to fresh metas. *)
let find_nominal_template_opt ctx (p : Syntax.path) =
  let rec former value ty =
    match Nbe.force ctx.Ctx.metas ty, Nbe.force ctx.Ctx.metas value with
    | _, (VNominal _ as nominal) -> Some nominal
    | VPi { codomain; _ }, (VLam _ as f) ->
        let arg = Ctx.raw_meta ctx in
        former (Nbe.apply ctx.Ctx.metas f arg) (Nbe.closure_apply ctx.Ctx.metas codomain arg)
    | _ -> None
  in
  Option.bind (resolve_path_value_opt ctx p) (fun (value, ty) -> former value ty)

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

let nominal_for_constructor_path_opt ctx (p : Syntax.path) =
  match resolve_path_value_opt ctx p with
  | Some (value, ty) -> (
      match Nbe.force ctx.Ctx.metas value with
      | VCon { nominal; _ } -> Some nominal
      | VLam _ | VFix _ -> nominal_from_constructor_type_opt ctx ty
      | _ -> None)
  | None -> None

(* Resolution order for a pattern head: the path is taken as naming a *type*
   first ([find_nominal_template_opt]), then as naming a *constructor*. Both go
   through the entry the path resolves to. *)
let find_nominal_for_pattern_head_opt ctx (p : Syntax.path) =
  match find_nominal_template_opt ctx p with
  | Some nominal -> Some nominal
  | None -> nominal_for_constructor_path_opt ctx p

let rec insert_explicit_effect_metas ctx core ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VPi { explicitness = Explicit; codomain; _ } ->
      let arg_core = Ctx.fresh_meta ctx in
      let arg_value = Ctx.eval ctx arg_core in
      insert_explicit_effect_metas ctx
        (Ap (core, Explicit, arg_core))
        (Nbe.closure_apply ctx.Ctx.metas codomain arg_value)
  | _ -> (core, ty)

(* [E.op]: the effect is the path without its last member, the operation that
   member's label. *)
let resolve_perform_operation ctx (op_path : Syntax.path) =
  match List.rev op_path.members with
  | [] -> raise (ElabError EffectOperationPathExpected)
  | op :: rev_members ->
      let effect_path = { op_path with members = List.rev rev_members } in
      let effect_core, _effect_value, effect_ty = resolve_path_core_value ctx effect_path in
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
