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
            | Some (_, _, field_ty) -> go (Dot (core, segment)) (Nbe.dot_value ctx.Ctx.metas value segment) field_ty rest
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

(* Typed macro arguments already elaborated, by [Syntax.Elaborated]'s [arg]: the
   core, its type, and the context level it was elaborated at. An entry lives for
   one typed macro application. *)
let elaborated_args : (int, term * value * lvl * Elab_effects.expr_effects) Hashtbl.t = Hashtbl.create 8
let elaborated_counter = ref 0

(** A call to a macro whose signature promises types (macro-annotation
    decisions, 2026-09-15). The macro applies like a function over types: its
    type binders, and an output that promises nothing, become metas; each
    [(x : Expr(T))] argument is checked at [T]; the result type meets
    [expected]. Every binder must be solved by then, for the macro runs with them,
    each as the reflected type it was solved to. Its output is expanded in place
    like every macro's output (M6) and checked at the type it promised. The run
    and the output's expansion are one call under the evaluation budget (M5).
    [check] elaborates a form at a type; returns the output's core and type. *)
let apply_typed_macro ~check (ctx : Ctx.t) ~name (args : Syntax.capture list) ~(expected : value option) =
  (* Expansion defers only a call whose macro has a signature, and only with a
     runtime to hand it back to. *)
  let runtime, entry, signature =
    match ctx.Ctx.macro_runtime with
    | Some runtime -> (
        match runtime.Ctx.lookup_macro name with
        | Some ({ signature = Some signature; _ } as entry) -> (runtime, entry, signature)
        | _ -> failwith ("Elab_resolve.apply_typed_macro: a deferred call names no typed macro: " ^ name))
    | None -> failwith "Elab_resolve.apply_typed_macro: a deferred macro call with no macro runtime"
  in
  let show v = Debug.pp_value_short ctx.Ctx.metas v in
  let macro = Eval_budget.written name in
  let rec instantiate ty metas =
    match Nbe.force ctx.Ctx.metas ty with
    | VPi { explicitness = Implicit; codomain; _ } ->
        let meta = Ctx.eval ctx (Ctx.fresh_meta ctx) in
        instantiate (Nbe.closure_apply ctx.Ctx.metas codomain meta) (meta :: metas)
    | ty -> (ty, List.rev metas)
  in
  let ty, metas = instantiate signature.type_ [] in
  (* A plain argument is syntax the macro reads; a typed one elaborates too. *)
  let arg_syntax = function Syntax.CapExpr { kind = Syntax.Stx stx; _ } -> Syntax.CapExpr stx | c -> c in
  let args = List.map arg_syntax args in
  (* Each typed argument elaborates here, once: where the output places it
     unchanged, it becomes [Syntax.Elaborated] naming this result. *)
  let elaborated = ref [] in
  let result_ty =
    List.fold_left2
      (fun ty (param, typed) arg ->
        match typed, arg, Nbe.force ctx.Ctx.metas ty with
        | false, _, _ -> ty
        | true, Syntax.CapExpr stx, VPi { explicitness = Explicit; domain; codomain; _ } ->
            let form = runtime.Ctx.expand stx in
            (* What the argument performs happens where the output places it. *)
            let core, effects =
              Elab_effects.collecting ctx (fun ctx ->
                try check ctx form domain
                with Unify.UnifyError _ as e ->
                  raise (ElabError (MacroArgumentType { macro; param; promised = show domain; reason = Printexc.to_string e })))
            in
            incr elaborated_counter;
            Hashtbl.replace elaborated_args !elaborated_counter (core, domain, ctx.Ctx.lvl, effects);
            elaborated := (!elaborated_counter, stx, form) :: !elaborated;
            Nbe.closure_apply ctx.Ctx.metas codomain (Ctx.eval ctx core)
        | true, _, _ -> failwith "Elab_resolve.apply_typed_macro: a typed parameter's argument is an Expr in the signature's order")
      ty signature.params args
  in
  Option.iter (fun expected -> Ctx.unify ctx expected result_ty) expected;
  let binders =
    List.map2
      (fun binder meta ->
        match Nbe.force ctx.Ctx.metas meta with
        | VFlex _ -> raise (ElabError (MacroBinderUnsolved { macro; binder }))
        | solved -> solved)
      signature.binders
      (List.filteri (fun i _ -> i < List.length signature.binders) metas)
  in
  let nominals = entry.Expand_ctx.syntax_nominals in
  let reflect ty =
    match nominals with
    | Some ns -> VCon { name = Compiler_names.Constructor_name.r_expr; spine = [ ty ]; nominal = ns.Macro_eval.r_ }
    | None -> ty
  in
  let promised = Nbe.force ctx.Ctx.metas result_ty in
  runtime.Ctx.macro_application ~name ~nominals (fun () ->
    let app = runtime.application () in
    let fn = List.fold_left (fun fn ty -> runtime.run_macro fn (reflect ty)) entry.Expand_ctx.value binders in
    let fn =
      List.fold_left
        (fun fn arg -> runtime.run_macro fn (Macro_eval.wrap_capture ~nominals (app.Expand.receive_capture arg)))
        fn args
    in
    match Macro_eval.unwrap_stx ?nominals fn with
    | Some expanded ->
        (* An argument as the output holds it when the macro placed it unchanged. *)
        let placed =
          List.map
            (fun (arg, stx, form) ->
              match app.Expand.receive_capture (Syntax.CapExpr stx) with
              | Syntax.CapExpr received -> (app.emit received, Syntax.Elaborated { arg; form })
              | _ -> failwith "Elab_resolve.apply_typed_macro: receiving an expression gave another capture")
            !elaborated
        in
        (* ponytail: bottom-up structural match - an argument nested inside another
           argument's placement is reused there and the outer one elaborates again;
           match top-down if that case matters. *)
        let mark (f : Syntax.t) = match List.assoc_opt f placed with Some kind -> { f with kind } | None -> f in
        let output = runtime.expand (Expand.map_forms Fun.id mark (app.emit expanded)) in
        let core =
          try check ctx output promised
          with Unify.UnifyError _ as e ->
            raise (ElabError (MacroOutputType { macro; promised = show promised; reason = Printexc.to_string e }))
        in
        List.iter (fun (arg, _, _) -> Hashtbl.remove elaborated_args arg) !elaborated;
        (core, promised)
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

(* [open M]: the members an open brings into scope, and how many context entries
   it adds, are read off the module's type - one per public field, one per public
   impl, the slot list [Nbe.push_opened_values] pushes (I2). The value only
   supplies each member's payload, entry for entry; a value whose entries do not
   line up with its type is a broken invariant, not a smaller open. *)
(* An open binds what the module's type lists (I2): its public fields and impls,
   in entry order. Each member's value is the module value's entry when the
   module is known, and its projection when it is a parameter (a neutral). The
   member list is the term's, so the evaluator pushes the same entries. *)
let open_module_value ~label ctx module_ty module_value =
  let broken () = failwith "Elab_resolve.open_module_value: a module value's entries do not match its type" in
  match Nbe.module_type_of ctx.Ctx.metas module_ty module_value with
  | VModule { entries = type_entries; partial = _ } ->
      let known = match Nbe.force ctx.Ctx.metas module_value with VModule { entries; _ } -> Some entries | _ -> None in
      (match known with Some es when List.compare_lengths es type_entries <> 0 -> broken () | _ -> ());
      let value_at i = Option.map (fun es -> List.nth es i) known in
      let members = ref NameMap.empty in
      let ctx, rev_opened, _ =
        List.fold_left
          (fun (c, opened, (i, impl_ix)) type_entry ->
            match type_entry, value_at i with
            | ModuleField (fname, Public, field_ty), (Some (ModuleField (_, Public, _)) | None as v) ->
                let value =
                  match v with
                  | Some (ModuleField (_, _, value)) -> value
                  | _ -> Nbe_support.dot_value ctx.Ctx.metas module_value fname
                in
                members := NameMap.add fname { level = c.Ctx.lvl; ty = field_ty } !members;
                (add_opened_field c fname field_ty value, OpenField fname :: opened, (i + 1, impl_ix))
            | ModuleImpl (_, Public, impl_ty, _), Some (ModuleImpl (_, Public, _, impl_value)) ->
                (add_opened_impl c impl_ty impl_value, OpenImpl impl_ix :: opened, (i + 1, impl_ix + 1))
            (* A parameter's impl is named (a signature names every impl it
               requires): its projection is the dictionary. *)
            | ModuleImpl (Some iname, Public, impl_ty, _), None ->
                (add_opened_impl c impl_ty (Nbe_support.dot_value ctx.Ctx.metas module_value iname), OpenImpl impl_ix :: opened, (i + 1, impl_ix + 1))
            | ModuleImpl (None, Public, _, _), None -> broken ()
            | (ModuleField (_, Private, _) | ModuleImpl (_, Private, _, _)), _ -> (c, opened, (i + 1, impl_ix))
            | _ -> broken ())
          (ctx, [], (0, 0)) type_entries
      in
      (match ctx.Ctx.macro_runtime with
       | Some runtime -> (
           match List.find_opt (fun name -> NameMap.mem name !members) (runtime.roles_in_open label) with
           | Some name -> raise (ElabError (OpenSuppliesRole name))
           | None -> ())
       | None -> ());
      ({ ctx with Ctx.opened = (label, !members) :: ctx.Ctx.opened }, List.rev rev_opened)
  | _ -> raise (ElabError NotAModule)

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
  | None -> (
      match nominal_for_constructor_path_opt ctx p, List.rev p.members with
      | (Some _ as found), _ -> found
      (* [T.C]: a constructor as a member of the type the rest of the path names. *)
      | None, constructor :: rev_type_members -> (
          match find_nominal_template_opt ctx { p with members = List.rev rev_type_members } with
          | Some (VNominal n) as found when List.mem_assoc constructor (nominal_constructors n.id n.captures) -> found
          | _ -> None)
      | None, [] -> None)

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

(* A quote's core: its template, and each hole checked at the kind its position
   gives it (M10). *)
let quote_core ~check (ctx : Ctx.t) template_value holes =
  let ns = Elab_stdlib.syntax_nominals ctx in
  let occurrences = Quote_holes.occurrences template_value in
  let hole_core (name, hole) =
    let kinds = List.filter_map (fun (n, k) -> if String.equal n name then Some k else None) occurrences in
    let expected =
      match List.sort_uniq compare kinds with
      | [ Quote_holes.Expr ] -> ns.Macro_eval.expr
      | [ Quote_holes.Pattern ] -> ns.pat
      | [ Quote_holes.Decl ] -> Elab_stdlib.resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.decls ]
      | [ Quote_holes.Id ] -> Elab_stdlib.resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.id ]
      | _ -> raise (ElabError (QuoteHoleKindConflict name))
    in
    (name, check ctx hole expected)
  in
  Quote { template = template_value; holes = List.map hole_core holes }
