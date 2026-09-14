open Core
include Elab_error
open Elab_common
open Elab_validate
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_syntax_util
open Elab_resolve
open Elab_refine
open Elab_patterns
open Elab_match
open Elab_defs
open Elab_generalize
open Elab_apply
open Elab_ops

(* The one place a module or struct binding extends the elaboration context.
   [Core.binding_slots] owns the order and the count; the payloads - a type and a
   value per slot - are the elaborator's own, and a disagreement between the term
   a binding emits and the entries it pushes fails here rather than surfacing
   later as a wrong de Bruijn index.
   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
let extend_from_slots (ctx : Ctx.t) (bind : Core.struct_binding_term) payloads =
  let slots =
    match Core.binding_slots bind with
    | Some slots -> slots
    | None -> failwith "binding contributes no slot list"
  in
  List.fold_left2
    (fun (ctx : Ctx.t) _slot payload ->
      match payload with
      | `Param param_name ->
          Ctx.define ctx param_name VU (VRigid { lvl = ctx.Ctx.lvl; spine = [] })
      | `Entry (name, ty, value) -> Ctx.define ctx name ty value
      | `Anonymous (ty, value) -> fst (Ctx.define_anonymous ctx ty value))
    ctx slots payloads

(* THE nominal-type binding elaboration, in one place.

   [type T(p...) = C1(..) | C2(..)] as a module or struct member. Three things
   have to line up and used to be written out twice, once here and once in the
   [Struct] fold, with the copies disagreeing:

   - the entries the binding contributes, which must equal [Core.binding_width]:
     the params, then the constructors, then the type itself, in that order,
     because that is the order [Nbe]'s [TypeBind] pushes them;
   - the context the payloads are elaborated in, which names the type so a
     payload can refer to it recursively, and which is *temporary* - it is not
     part of the binding's width;
   - the closure environment each payload is stored under, which excludes the
     params ([build_ctor] supplies those itself) and carries the nominal
     placeholder outermost so [NomRef] can find it by name.

   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
(* A type declaration's names as the elaborator's context keys them. *)
type type_member = { member_name : string; member_params : string list; member_ctors : (string * Syntax.t list) list }

let elab_type_group (ops : Elab_ops.t) (ctx : Ctx.t) ~(members : Syntax.type_decl list) ~public
    : Ctx.t * (Core.struct_binding_term * (string * struct_field_kind * value) list) list =
  let members =
    List.map
      (fun (m : Syntax.type_decl) ->
        { member_name = m.name.name; member_params = Syntax.names m.params;
          member_ctors = List.map (fun ((c : Syntax.id), payloads) -> (c.name, payloads)) m.ctors })
      members
  in
  let group = List.map (fun (m : type_member) -> (m.member_name, List.length m.member_params)) members in
  let param_ctx_of (m : type_member) =
    List.fold_left
      (fun ctx param_name ->
        Ctx.define ctx param_name VU (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
      ctx m.member_params
  in
  (* The type's own type: [Type] when nullary, an explicit [Pi] chain otherwise
     so [T I64] elaborates. *)
  let nominal_ty_of (m : type_member) placeholder_env param_ctx =
    if m.member_params = [] then VU
    else
      let depth = List.length param_ctx.Ctx.env + 1 in
      List.fold_right
        (fun _ acc ->
          VPi { explicitness = Explicit; domain = VU;
                effects = effect_row_closure placeholder_env empty_effect_row;
                codomain = { env = placeholder_env; body = Nbe.quote param_ctx.Ctx.metas depth acc } })
        m.member_params VU
  in
  (* Phase 1, register: a placeholder per member, sharing its id with the
     finished nominal, so payloads written against it mean the finished type. *)
  let registered =
    List.map
      (fun (m : type_member) ->
        let nominal_id = NominalId.fresh () in
        let placeholder =
          VNominal { id = nominal_id; name = m.member_name; num_params = 0; params = []; constructors = [] }
        in
        let param_ctx = param_ctx_of m in
        let placeholder_env = placeholder :: param_ctx.Ctx.env in
        (m, nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty_of m placeholder_env param_ctx))
      members
  in
  let placeholders = List.map (fun (_, _, p, _, _, _) -> p) registered in
  (* Phase 2, elaborate: every member's payloads, in a context naming every
     member. Those names are temporary - they contribute no width. *)
  let elaborated =
    List.map
      (fun ((m : type_member), nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty) ->
        let group_ctx =
          List.fold_left
            (fun gctx ((other : type_member), _, other_placeholder, _, _, other_ty) ->
              let num_params = List.length other.member_params in
              if num_params = 0 then Ctx.define gctx other.member_name VU other_placeholder
              else
                let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) other.member_params in
                let type_core_term =
                  List.fold_right (fun _ acc -> Lam acc) other.member_params (NomRef (other.member_name, type_var_terms))
                in
                let type_val = Nbe.eval param_ctx.Ctx.metas (other_placeholder :: param_ctx.Ctx.env) type_core_term in
                Ctx.define gctx other.member_name other_ty type_val)
            param_ctx registered
        in
        let elaborated_ctors =
          List.map
            (fun (cname, payloads) ->
              ( cname,
                List.map
                  (fun payload_expr ->
                    let payload_core, payload_ty = ops.infer group_ctx payload_expr in
                    check_type_like group_ctx payload_ty (Ctx.eval group_ctx payload_core);
                    { env = ctx.Ctx.env @ placeholders; body = close_recursive_payload_group group payload_core })
                  payloads ))
            m.member_ctors
        in
        (m, nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty, elaborated_ctors))
      registered
  in
  (* Phase 3, finish: build each nominal and its constructors, and extend the
     context in chain order - params, constructors, then the type. *)
  let kind = if public then Public else Private in
  let ctx', results =
    List.fold_left
      (fun (ctx, acc) (i, ((m : type_member), nominal_id, _, param_ctx, placeholder_env, nominal_ty, elaborated_ctors)) ->
        let num_params = List.length m.member_params in
        let nominal =
          VNominal { id = nominal_id; name = m.member_name; num_params; params = []; constructors = elaborated_ctors }
        in
        finish_nominal nominal_id elaborated_ctors;
        let ctor_values, ctor_types =
          List.split
            (List.map
               (fun (cname, payload_clos) ->
                 let ctor_value, ctor_ty =
                   (* The other members' placeholders sit under the head so a
                      constructor type naming them re-evaluates; levels count
                      from the tail, so no index moves. *)
                   let others = List.filteri (fun j _ -> j <> i) placeholders in
                   build_ctor param_ctx.Ctx.metas (nominal :: others @ placeholder_env) m.member_name cname num_params payload_clos
                 in
                 ((cname, ctor_value), (cname, ctor_ty)))
               elaborated_ctors)
        in
        let bind = TypeBind (m.member_name, kind, nominal, ctor_values) in
        let ctx' =
          extend_from_slots ctx bind
            (List.map (fun p -> `Param p) m.member_params
            @ List.map2
                (fun (cname, ctor_value) (_, ctor_ty) -> `Entry (cname, ctor_ty, ctor_value))
                ctor_values ctor_types
            @ [ `Entry (m.member_name, nominal_ty, nominal) ])
        in
        let fields = (m.member_name, kind, nominal_ty) :: List.map (fun (c, ty) -> (c, kind, ty)) ctor_types in
        (ctx', (bind, fields) :: acc))
      (ctx, []) (List.mapi (fun i e -> (i, e)) elaborated)
  in
  (ctx', List.rev results)

(** Stage 7: per-binding module elaboration. Processes a single
    [Syntax.struct_binding] and returns the updated elaboration context,
    the resulting [Core.struct_binding_term] list, and the [Core.module_entry]
    list. [MacroBinding]/[MacroCallBinding] return empty results as the
    expander handles these separately. *)
let elab_module_binding (ops : Elab_ops.t) (ctx : Ctx.t) (b : Syntax.struct_binding)
    : Ctx.t * Core.struct_binding_term list * Core.module_entry list =
  match b with
  | Syntax.MethodBinding _ -> failwith "module binding cannot be method"
  | Syntax.MacroBinding _ -> (ctx, [], [])
  | Syntax.MacroCallBinding _ -> (ctx, [], [])
  | Syntax.PatternSynBinding { name = { name; _ }; params; rhs; public } ->
      let params = Syntax.names params in
      let scrutinee_ty =
        match rhs with
        | Syntax.PatCon (con_path, _) ->
            (match Elab_resolve.find_nominal_for_pattern_head_opt ctx con_path with
             | Some nominal -> nominal
             | None -> VU)
        | _ -> VU
      in
      let core_rhs, _binders = Elab_patterns.elaborate_pat_binders ctx rhs scrutinee_ty in
      let syn_val = VPatternSyn { name; params; rhs = core_rhs; scrutinee_ty } in
      let kind = if public then Public else Private in
      let bind = PatternSynBind (name, kind, syn_val) in
      let ctx' = extend_from_slots ctx bind [ `Entry (name, VU, syn_val) ] in
      (ctx', [bind], [ModuleField (name, kind, VU)])
  | Syntax.OpenBinding (mod_expr, label) ->
      (* Module-level [open]: the opened module's public fields are in scope for
         the bindings that *follow* (the caller folds this ctx forward), and the
         open exports nothing itself. [OpenBind] carries the same scope
         extension to the evaluator. *)
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      let mod_value = Ctx.eval ctx mod_core in
      (match (Nbe.force ctx.metas mod_ty, Nbe.force ctx.metas mod_value) with
       | VModule _, VModule _ ->
           (open_module_value ~label ctx mod_ty mod_value, [OpenBind mod_core], [])
       | _ -> raise (ElabError NotAModule))
  | Syntax.LetBinding { name = { name; _ }; value; public; recursive } ->
      let rec_ty = Ctx.raw_meta ctx in
      let value_ctx = Ctx.clear_self_scope ctx in
      let value_ctx = if recursive then Ctx.bind value_ctx name rec_ty else value_ctx in
      let val_core, val_ty = ops.infer value_ctx value in
      (if recursive then Ctx.unify ctx rec_ty val_ty);
      let val_core = if recursive then Fix val_core else val_core in
      let val_val = Ctx.eval ctx val_core in
      let kind = if public then Public else Private in
      let bind = LetBind (name, kind, val_core) in
      let ctx' = extend_from_slots ctx bind [ `Entry (name, val_ty, val_val) ] in
      (ctx', [bind], [ModuleField (name, kind, val_ty)])
  | Syntax.EffectBinding { name = { name; _ }; params; ops = eff_ops; public } ->
      let params = Syntax.names params in
      let _effect_id, eff, eff_ty, _elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let kind = if public then Public else Private in
      let bind = EffectBind (name, kind, eff) in
      let ctx' = extend_from_slots ctx bind [ `Entry (name, eff_ty, eff) ] in
      (ctx', [bind], [ModuleField (name, kind, eff_ty)])
  | Syntax.TraitBinding { name = { name; _ }; params; fields; public } ->
      let params = Syntax.names params in
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let kind = if public then Public else Private in
      let bind =
        LetBind (name, kind, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name })
      in
      let ctx' = extend_from_slots ctx bind [ `Entry (name, VU, trait_ty) ] in
      (ctx', [bind], [ModuleField (name, kind, VU)])
  | Syntax.ImplBinding { name; trait; args; fields; public } ->
      let name = Option.map (fun (i : Syntax.id) -> i.name) name in
      let c = elaborate_impl_contribution ops ctx trait args fields in
      let kind = if public then Public else Private in
      let bind = ImplBind (name, kind, c.impl_core, c.impl_dict_ty) in
      let level = ctx.Ctx.lvl in
      let ctx' =
        extend_from_slots ctx bind [ `Anonymous (c.impl_dict_ty, c.impl_value) ]
      in
      let ctx', _evidence = install_impl_evidence ?impl_name:name ctx' c ~level in
      (ctx', [bind], [ModuleImpl (name, kind, c.impl_dict_ty, c.impl_value)])
  | Syntax.RecordTypeBinding { name = { name; _ }; params; fields; public } ->
      let params = Syntax.names params in
      check_duplicate_names (List.map fst fields);
      let rewritten_fields =
        List.map
          (fun (field, ty) -> (field, rewrite_record_self_refs name params ty))
          fields
      in
      let rec elaborate_params ctx param_values = function
        | [] ->
            let self_type = VSelfType param_values in
            ops.infer (Ctx.with_self_type ctx self_type)
              (Syntax.synth (Syntax.Struct { con_fields = rewritten_fields; bindings = [] }))
        | param :: rest ->
            let param_value = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
            let ctx' = Ctx.bind ctx param VU in
            let body_core, body_ty = elaborate_params ctx' (param_values @ [ param_value ]) rest in
            let body_ty_term = Ctx.quote ctx' body_ty in
            ( Lam body_core,
              VPi
                { explicitness = Implicit;
                  domain = VU;
                  effects = effect_row_closure ctx.Ctx.env empty_effect_row;
                  codomain = { env = ctx.Ctx.env; body = body_ty_term } } )
      in
      let val_core, val_ty = elaborate_params ctx [] params in
      let val_val = Ctx.eval ctx val_core in
      let kind = if public then Public else Private in
      let bind = LetBind (name, kind, val_core) in
      let ctx' = extend_from_slots ctx bind [ `Entry (name, val_ty, val_val) ] in
      (ctx', [bind], [ModuleField (name, kind, val_ty)])
  | Syntax.TypeBinding { members; public } ->
      (* The module fold prepends each binding's results and reverses at the
         end, so a chain's binds and entries come back last member first. *)
      let ctx', results = elab_type_group ops ctx ~members ~public in
      (ctx', List.rev_map fst results,
       List.rev_map (fun (name, kind, ty) -> ModuleField (name, kind, ty)) (List.concat_map snd results))

let infer ops (ctx : Ctx.t) (expr : Syntax.t) : term * value =
  match expr.kind with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy Atom_ty.TI64)
  | Atom Unit -> (Atom Unit, VAtomTy Atom_ty.TUnit)
  | Atom (Char c) -> (Atom (Char c), VAtomTy Atom_ty.TChar)
  | Atom (String s) -> (Atom (String s), VAtomTy Atom_ty.TString)
  | Atom (Scopes s) -> (Atom (Scopes s), VAtomTy Atom_ty.TScopes)
  | Var { name; _ } ->
      let ix, ty = Ctx.lookup ctx name in
      let core = Var ix in
      (core, ty)
  | Self ->
      let ix, ty = Ctx.lookup_self ctx in
      (Var ix, ty)
  | SelfType ->
      (Ctx.quote ctx (Ctx.lookup_self_type ctx), VU)
  | Perform { op = op_path; arg } ->
      let op = Syntax.path_last op_path in
      let effect_core, _effect_value, input_ty, output_ty = resolve_perform_operation ctx op_path in
      let arg_core = ops.check ctx arg input_ty in
      (Perform { eff = effect_core; op; arg = arg_core }, Nbe.force ctx.metas output_ty)
  | Resume arg -> infer_resume ops ctx arg
  | RefNew e ->
      let core, ty = ops.infer ctx e in
      (RefNew core, VRefTy ty)
  | RefGet r ->
      let r_core, r_ty = ops.infer ctx r in
      let r_core, r_ty = insert_implicit_args ctx r_core r_ty in
      (match Nbe.force ctx.metas r_ty with
      | VRefTy elem_ty -> (RefGet r_core, elem_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | RefSet (r, e) ->
      let r_core, r_ty = ops.infer ctx r in
      let r_core, r_ty = insert_implicit_args ctx r_core r_ty in
      (match Nbe.force ctx.metas r_ty with
      | VRefTy elem_ty ->
          let e_core = ops.check ctx e elem_ty in
          (RefSet (r_core, e_core), VAtomTy Atom_ty.TUnit)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Ap (f, Explicitness.Explicit, a) -> infer_ap ops ctx f a
  | Ap (f, Explicitness.Implicit, a) -> infer_ap_implicit ops ctx f a
  | Let { name = { name; _ }; type_; value; body; recursive } ->
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
        let body_core, body_ty = ops.infer ctx' body in
        (Let (ty_term, fix_core, body_core), body_ty)
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
        let val_val = Ctx.eval ctx gen_val_core in
        let ty_term = Ctx.quote ctx gen_val_ty in
        let ctx' = Ctx.define ctx name gen_val_ty val_val in
        let body_core, body_ty = ops.infer ctx' body in
        (Let (ty_term, gen_val_core, body_core), body_ty)
      end
  | Lam (param, body) -> infer_lam ops ctx param body
  | Annotated { inner; typ } ->
      require_empty_effects ctx (ops.collect_effects ctx typ);
      let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx typ in
      let core = ops.check ctx inner ty_val in
      (core, ty_val)
  | Prod elems ->
      let cores_tys = List.map (ops.infer ctx) elems in
      let cores = List.map fst cores_tys in
      let tys = List.map snd cores_tys in
      (Prod cores, VProdTy tys)
  | ProdTy elems ->
      let core_elems =
        List.map
          (fun elem ->
            require_empty_effects ctx (ops.collect_effects ctx elem);
            let elem_core, elem_ty = ops.infer ctx elem in
            check_type_like ctx elem_ty (Ctx.eval ctx elem_core);
            elem_core)
          elems
      in
      (ProdTy core_elems, VU)
  | Arrow (Explicitness.Implicit, Some { name; _ }, a, effects, b) -> (
      match trait_bounds_opt ctx a with
      | Some trait_infos ->
          let type_ctx = Ctx.bind ctx name VU in
          let dict_ctx, dict_layers = bind_trait_bound_dicts ctx name trait_infos in
          Option.iter (fun (row : Syntax.effect_row) -> List.iter (fun eff -> require_empty_effects type_ctx (ops.collect_effects type_ctx eff)) row.effects; Option.iter (fun tail -> require_empty_effects type_ctx (ops.collect_effects type_ctx tail)) row.tail) effects;
          let effects = Elab_type_expr.elaborate_effect_row ops type_ctx effects in
          require_empty_effects dict_ctx (ops.collect_effects dict_ctx b);
          let b_core, b_ty = ops.infer dict_ctx b in
          check_type_like dict_ctx b_ty (Ctx.eval dict_ctx b_core);
          let core =
            Pi
              { explicitness = Implicit;
                domain = U;
                effects;
                codomain =
                  List.fold_right
                    (fun dict_core codomain ->
                      Pi { explicitness = Implicit; domain = dict_core; effects = empty_effect_row; codomain })
                    dict_layers b_core }
          in
          (core, VU)
      | _ ->
          require_empty_effects ctx (ops.collect_effects ctx a);
          let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
          let ctx' = Ctx.bind ctx name a_val in
          Option.iter (fun (row : Syntax.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) effects;
          let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
          require_empty_effects ctx' (ops.collect_effects ctx' b);
          let b_core, b_ty = ops.infer ctx' b in
          check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
          (Pi { explicitness = Implicit; domain = a_core; effects; codomain = b_core }, VU))
  | Arrow (expl, name, a, effects, b) ->
      require_empty_effects ctx (ops.collect_effects ctx a);
      let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
      let ctx' = Ctx.bind ctx (Option.fold ~none:"_" ~some:(fun (i : Syntax.id) -> i.name) name) a_val in
      Option.iter (fun (row : Syntax.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) effects;
      let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
      require_empty_effects ctx' (ops.collect_effects ctx' b);
      let b_core, b_ty = ops.infer ctx' b in
      check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
      (Pi { explicitness = expl_of_syntax expl; domain = a_core; effects; codomain = b_core }, VU)
  | FieldAccess (head, name)
    when Option.is_some (trait_of_form_opt ctx head) ->
      resolve_trait_method ctx (Option.get (trait_of_form_opt ctx head)) name
  | FieldAccess (e, name) ->
      let e_core, e_ty = ops.infer ctx e in
      let e_core, e_ty = insert_implicit_args ctx e_core e_ty in
      (match Nbe.force ctx.metas e_ty with
      | VModule { entries; partial = _ } -> (
          match find_field_last (fun (n, _, _) -> String.equal n name) (visible_module_fields entries) with
          | Some (_, _, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
          (* A named impl is a member: [M.eq_C] has the trait dictionary type,
             which is what makes it usable in evidence position. *)
          | None -> (
              match module_impl_type_opt entries name with
              | Some (Public, impl_ty) -> (Dot (e_core, name), Nbe.force ctx.metas impl_ty)
              | _ -> raise (ElabError (UnboundVariable name))))
      | VStruct { entries; partial } -> (
          let fields = struct_entry_fields entries in
          match Nbe.force ctx.metas (Ctx.eval ctx e_core) with
          | VStruct _ -> (
              match find_field_last (fun (n, _, _) -> String.equal n name) (visible_struct_members fields) with
              | Some (_, _, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
              | None -> raise (ElabError (UnboundVariable name)))
          | _ -> (
              let record_fields = visible_record_fields fields in
              match find_record_field record_fields name with
              | Some (_, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
              | None when partial ->
                  let result_ty = Ctx.raw_meta ctx in
                  let constraint_ty =
                    VStruct { entries = entries @ [ StructField (name, Field, result_ty) ]; partial = true }
                  in
                  Ctx.unify ctx e_ty constraint_ty;
                  (Dot (e_core, name), result_ty)
              | None -> raise (ElabError (UnboundVariable name))))
      | VRecord { typ; _ } -> (
          match Nbe.force ctx.metas typ with
          | VStruct { entries; _ } -> (
              match find_record_field (visible_record_fields (struct_entry_fields entries)) name with
              | Some (_, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
              | None -> raise (ElabError (UnboundVariable name)))
          | _ -> raise (ElabError ApplyingNonFunction))
      | VFlex _ | VRigid _ | VNeutral _ ->
          let result_ty = Ctx.raw_meta ctx in
          let constraint_ty =
            VStruct { entries = [ StructField (name, Field, result_ty) ]; partial = true }
          in
          Ctx.unify ctx e_ty constraint_ty;
          (Dot (e_core, name), result_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Proj (e, i) ->
      let e_core, e_ty = ops.infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VProdTy tys ->
          if i < 0 || i >= List.length tys then
            raise (ElabError TupleLengthMismatch);
          (Proj (e_core, i), Nbe.force ctx.metas (List.nth tys i))
      | _ -> raise (ElabError ApplyingNonFunction))
  | Import path when String.equal path Compiler_names.Module_name.std_import_path ->
      (* Reserved path: [import "std"] resolves to the builtin prelude module,
         already elaborated and bound by [init_ctx] as [stdlib]. Resolving it
         here (typecheck layer) keeps the loader from having to name the prelude
         upward across the layer boundary. *)
      let ix, ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
      (Var ix, ty)
  | Import path -> (
      match ctx.loader with
      | Some loader ->
          (* A compilation unit's meaning depends only on its own source plus
             what it imports and opens, so it elaborates against the base
             context, not this one. Its term is therefore anchored at the base
             and cannot be spliced in here; its value can, and is. *)
          let _core, value, ty =
              Core_loader.load_elaborated loader path
                ~elaborate:(fun imported expand_ctx ->
                    (* Only the unit's context takes the unit's expander. The
                       importer keeps its own: elaboration reads [eval_and_apply]
                       and the evaluation budget out of [expand_ctx], and after an import
                       both would otherwise come from the last imported unit.
                       See docs/wayfinder/tickets/base-context-shared-state.md. *)
                    let unit_ctx = Ctx.unit_base ctx in
                    unit_ctx.macro_runtime <- Ctx.macro_runtime_of_expander expand_ctx;
                    let core, ty = ops.infer unit_ctx imported in
                    (core, Ctx.eval unit_ctx core, ty))
                ~eval_and_apply:Nbe.apply_macro
                ~syntax_nominals:(Elab_stdlib.syntax_nominals ctx)
              in
          (Imported value, ty)
      | None -> raise (ElabError (ImportRequiresLoader path)))
  | RecordConstruct { typ; fields } ->
      let typ_core, typ_ty = ops.infer ctx typ in
      let typ_core, typ_ty = insert_implicit_args ctx typ_core typ_ty in
      (match Nbe.force ctx.metas typ_ty with
      | VStruct { entries = struct_entries; _ } as record_ty ->
          let record_fields = visible_record_fields (struct_entry_fields struct_entries) in
          check_duplicate_names (List.map fst fields);
          List.iter
            (fun (name, _) ->
              if Option.is_none (find_record_field record_fields name) then
                raise (ElabError (UnknownRecordField name)))
            fields;
          List.iter
            (fun (name, _) ->
              if Option.is_none (List.assoc_opt name fields) then
                raise (ElabError (MissingRecordField name)))
            record_fields;
          let field_cores =
            List.map
              (fun (name, value) ->
                let field_ty =
                  match find_record_field record_fields name with
                  | Some (_, ty) -> ty
                  | None -> raise (ElabError (UnknownRecordField name))
                in
                (name, ops.check ctx value field_ty))
              fields
          in
          (RecordConstruct { typ = typ_core; fields = field_cores }, record_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Module { bindings } ->
      let binding_ctx = Ctx.clear_self_scope ctx in
      let _end_ctx, core_bindings, entries =
        List.fold_left (fun (ctx, acc_binds, acc_entries) b ->
          let ctx', b, e = elab_module_binding ops ctx b in
          (ctx', b @ acc_binds, e @ acc_entries))
        (binding_ctx, [], []) bindings
      in
      let core_bindings = List.rev core_bindings in
      let entries = List.rev entries in
      let fields = module_entry_fields entries in
      validate_module_fields fields;
      (Module { bindings = core_bindings }, VModule { entries; partial = false })
  | Struct { con_fields; bindings } ->
      let con_cores =
        List.map (fun (name, ty_expr) ->
          let ty_core, _ty_ty, ty_value = ops.type_value_of_expr ctx ty_expr in
          (name, ty_core, ty_value))
        con_fields
      in
      let self_ty =
        VStruct {
          entries = List.map (fun (name, _, ty) -> StructField (name, Field, ty)) con_cores;
          partial = true;
        }
      in
      let binding_ctx = Ctx.with_self_type ctx self_ty in
      let rec elaborate_method_params ctx params body =
        match params with
        | [] ->
            let body_core, body_ty = ops.infer ctx body in
            (body_core, body_ty)
        | param :: rest ->
            let a_ty =
              match param.Syntax.type_ with
              | Some ty_expr ->
                  let _ty_core, _ty_ty, ty_value = ops.type_value_of_expr ctx ty_expr in
                  ty_value
              | None -> Ctx.raw_meta ctx
            in
            let ctx' = Ctx.bind ctx param.Syntax.name.name a_ty in
            let body_core, body_ty = elaborate_method_params ctx' rest body in
            let body_ty_term = Ctx.quote ctx' body_ty in
            let method_ty =
              VPi {
                explicitness = expl_of_syntax param.explicitness;
                domain = a_ty;
                effects = effect_row_closure ctx.env empty_effect_row;
                codomain = { env = ctx.env; body = body_ty_term };
              }
            in
            (Lam body_core, method_ty)
      in
      let elaborate_method ctx params body =
        let self_ctx, self_entry = Ctx.bind_anonymous ctx self_ty in
        let self_ctx = { self_ctx with Ctx.self_entry = Some self_entry } in
        let body_core, body_ty = elaborate_method_params self_ctx params body in
        let body_ty_term = Ctx.quote self_ctx body_ty in
        let method_ty =
          VPi {
            explicitness = Explicit;
            domain = self_ty;
            effects = effect_row_closure ctx.env empty_effect_row;
            codomain = { env = ctx.env; body = body_ty_term };
          }
        in
        (Lam body_core, method_ty)
      in
      let rec go ctx (acc_binds, acc_entries) = function
        | [] -> (ctx, List.rev acc_binds, List.rev acc_entries)
        | Syntax.MacroBinding _ :: rest -> go ctx (acc_binds, acc_entries) rest
        | Syntax.MacroCallBinding _ :: rest -> go ctx (acc_binds, acc_entries) rest
        | Syntax.PatternSynBinding { name = { name; _ }; params; rhs; public } :: rest ->
            let params = Syntax.names params in
            let scrutinee_ty =
              match rhs with
              | Syntax.PatCon (con_path, _) ->
                  (match Elab_resolve.find_nominal_for_pattern_head_opt ctx con_path with
                   | Some nominal -> nominal
                   | None -> VU)
              | _ -> VU
            in
            let core_rhs, _binders = Elab_patterns.elaborate_pat_binders ctx rhs scrutinee_ty in
            let syn_val = VPatternSyn { name; params; rhs = core_rhs; scrutinee_ty } in
            let kind = if public then Public else Private in
            let bind = PatternSynBind (name, kind, syn_val) in
            let ctx' = extend_from_slots ctx bind [ `Entry (name, VU, syn_val) ] in
            go ctx'
               (bind :: acc_binds,
                StructField (name, kind, VU) :: acc_entries)
              rest
        | Syntax.OpenBinding (mod_expr, label) :: rest ->
            let mod_core, mod_ty = ops.infer ctx mod_expr in
            let mod_value = Ctx.eval ctx mod_core in
            (match (Nbe.force ctx.metas mod_ty, Nbe.force ctx.metas mod_value) with
             | VModule _, VModule _ ->
                 go (open_module_value ~label ctx mod_ty mod_value)
                   (OpenBind mod_core :: acc_binds, acc_entries) rest
             | _ -> raise (ElabError NotAModule))
        | Syntax.LetBinding { name = { name; _ }; value; public; recursive; _ } :: rest ->
            let rec_ty = Ctx.raw_meta ctx in
            let value_ctx = Ctx.clear_self ctx in
            let value_ctx = if recursive then Ctx.bind value_ctx name rec_ty else value_ctx in
            let val_core, val_ty = ops.infer value_ctx value in
            (if recursive then Ctx.unify ctx rec_ty val_ty);
            let val_core = if recursive then Fix val_core else val_core in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let bind = LetBind (name, kind, val_core) in
            let ctx' = extend_from_slots ctx bind [ `Entry (name, val_ty, val_val) ] in
            let entries = if public then [ StructField (name, kind, val_ty) ] else [] in
            go ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.MethodBinding { name = { name; _ }; params; body; public } :: rest ->
            let method_core, method_ty = elaborate_method ctx params body in
            let method_val = Ctx.eval ctx method_core in
            let kind = if public then Method else PrivateMethod in
            let bind = LetBind (name, kind, method_core) in
            let ctx' = extend_from_slots ctx bind [ `Entry (name, method_ty, method_val) ] in
            let entries = if public then [ StructField (name, kind, method_ty) ] else [] in
            go ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.EffectBinding { name = { name; _ }; params; ops = eff_ops; public } :: rest ->
            let params = Syntax.names params in
            let _effect_id, eff, eff_ty, _elaborated_ops =
              elaborate_eff_family ops ctx name params eff_ops
            in
            let kind = if public then Public else Private in
            let bind = EffectBind (name, kind, eff) in
            let ctx' = extend_from_slots ctx bind [ `Entry (name, eff_ty, eff) ] in
            let entries = if public then [ StructField (name, kind, eff_ty) ] else [] in
            go ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.TraitBinding _ :: _ ->
            raise (ElabError ApplyingNonFunction)
        | Syntax.ImplBinding { name; trait; args; fields; public } :: rest ->
            let name = Option.map (fun (i : Syntax.id) -> i.name) name in
            let c = elaborate_impl_contribution ops ctx trait args fields in
            let kind = if public then Public else Private in
            let bind = ImplBind (name, kind, c.impl_core, c.impl_dict_ty) in
            let level = ctx.Ctx.lvl in
            let ctx' =
              extend_from_slots ctx bind [ `Anonymous (c.impl_dict_ty, c.impl_value) ]
            in
            let ctx', _evidence = install_impl_evidence ?impl_name:name ctx' c ~level in
            go ctx'
              (bind :: acc_binds,
               StructImpl (name, kind, c.impl_dict_ty, c.impl_value) :: acc_entries)
              rest
        | Syntax.RecordTypeBinding { name = { name; _ }; params; fields; public } :: rest ->
            let params = Syntax.names params in
            check_duplicate_names (List.map fst fields);
            let rewritten_fields =
              List.map
                (fun (field, ty) -> (field, rewrite_record_self_refs name params ty))
                fields
            in
            let rec elaborate_params ctx param_values = function
              | [] ->
                  let self_type = VSelfType param_values in
                  ops.infer (Ctx.with_self_type ctx self_type)
                    (Syntax.synth (Syntax.Struct { con_fields = rewritten_fields; bindings = [] }))
              | param :: rest ->
                  let param_value = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
                  let ctx' = Ctx.bind ctx param VU in
                  let body_core, body_ty = elaborate_params ctx' (param_values @ [ param_value ]) rest in
                  let body_ty_term = Ctx.quote ctx' body_ty in
                  ( Lam body_core,
                    VPi
                      { explicitness = Implicit;
                        domain = VU;
                        effects = effect_row_closure ctx.Ctx.env empty_effect_row;
                        codomain = { env = ctx.Ctx.env; body = body_ty_term } } )
            in
            let val_core, val_ty = elaborate_params ctx [] params in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let bind = LetBind (name, kind, val_core) in
            let ctx' = extend_from_slots ctx bind [ `Entry (name, val_ty, val_val) ] in
            let entries = if public then [ StructField (name, kind, val_ty) ] else [] in
            go ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.TypeBinding { members; public } :: rest ->
            let ctx', results = elab_type_group ops ctx ~members ~public in
            let acc =
              List.fold_left
                (fun (acc_binds, acc_entries) (bind, fields) ->
                  let type_entries = List.map (fun (name, kind, ty) -> StructField (name, kind, ty)) fields in
                  (bind :: acc_binds, List.rev_append type_entries acc_entries))
                (acc_binds, acc_entries) results
            in
            go ctx' acc rest
      in
      let _end_ctx, core_bindings, extra_entries = go binding_ctx ([], []) bindings in
      let result_con_fields = List.map (fun (n, c, _) -> (n, c)) con_cores in
      let type_entries =
        List.map (fun (n, _, ty) -> StructField (n, Field, ty)) con_cores
        @ extra_entries
      in
      (Struct { con_fields = result_con_fields; bindings = core_bindings; partial = false },
       VStruct { entries = type_entries; partial = false })
  | OpenChoice { name = { name; _ }; opens; fallback } -> (
      match Ctx.lookup_choice_opt ctx name { opens; fallback } with
      | Some (ix, ty) -> (Var ix, ty)
      | None -> raise (ElabError (UnboundVariable name)))
  | Open (mod_expr, body, label) ->
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      let mod_value = Ctx.eval ctx mod_core in
      (match (Nbe.force ctx.metas mod_ty, Nbe.force ctx.metas mod_value) with
      | VModule _, VModule _ ->
          let body_core, body_ty = ops.infer (open_module_value ~label ctx mod_ty mod_value) body in
          (Open (mod_core, body_core), body_ty)
      | _ -> raise (ElabError NotAModule))
  | RecordTypeDef { name = { name; _ }; params; fields; body } ->
      let params = Syntax.names params in
      check_duplicate_names (List.map fst fields);
      let rewritten_fields =
        List.map
          (fun (field, ty) -> (field, rewrite_record_self_refs name params ty))
          fields
      in
      let rec elaborate_params ctx param_values = function
        | [] ->
            let self_type = VSelfType param_values in
            ops.infer (Ctx.with_self_type ctx self_type)
              (Syntax.synth (Syntax.Struct { con_fields = rewritten_fields; bindings = [] }))
        | param :: rest ->
            let param_value = VRigid { lvl = ctx.lvl; spine = [] } in
            let ctx' = Ctx.bind ctx param VU in
            let body_core, body_ty = elaborate_params ctx' (param_values @ [ param_value ]) rest in
            let body_ty_term = Ctx.quote ctx' body_ty in
            ( Lam body_core,
              VPi
                { explicitness = Implicit;
                  domain = VU;
                  effects = effect_row_closure ctx.env empty_effect_row;
                  codomain = { env = ctx.env; body = body_ty_term } } )
      in
      let val_core, val_ty = elaborate_params ctx [] params in
      let val_val = Ctx.eval ctx val_core in
      let ty_term = Ctx.quote ctx val_ty in
      let ctx' = Ctx.define ctx name val_ty val_val in
      let body_core, body_ty = ops.infer ctx' body in
      (Let (ty_term, val_core, body_core), body_ty)
  | TypeDef { name = { name; _ }; params; ctors; body } ->
      let params = Syntax.names params in
      let ctors = List.map (fun ((c : Syntax.id), payloads) -> (c.name, payloads)) ctors in
      let num_params = List.length params in
      (* Bind type params as rigid variables (locally abstract types) *)
      let param_ctx =
        List.fold_left
          (fun ctx param_name ->
            Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
          ctx params
      in
      let nominal_id = NominalId.fresh () in
      let nominal_placeholder = VNominal { id = nominal_id; name; num_params = 0; params = []; constructors = [] } in
      let recursive_param_ctx =
        if num_params = 0 then Ctx.define param_ctx name VU nominal_placeholder
        else
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
          let type_body_term = NomRef (name, type_var_terms) in
          let type_core_term = List.fold_right (fun _ acc -> Lam acc) params type_body_term in
          let type_val = Nbe.eval param_ctx.metas (nominal_placeholder :: param_ctx.env) type_core_term in
          let type_ty =
            let depth = List.length param_ctx.env + 1 in
            List.fold_right
              (fun _ acc ->
                VPi { explicitness = Explicit; domain = VU;
                      effects = effect_row_closure (nominal_placeholder :: param_ctx.env) empty_effect_row;
                      codomain = { env = nominal_placeholder :: param_ctx.env; body = Nbe.quote param_ctx.metas depth acc } })
              params VU
          in
          Ctx.define param_ctx name type_ty type_val
      in
      let elaborated_ctors =
        List.map
          (fun (cname, payloads) ->
            let payload_clos =
              List.map
                (fun payload_expr ->
                let payload_core, payload_ty = ops.infer recursive_param_ctx payload_expr in
                check_type_like recursive_param_ctx payload_ty (Ctx.eval recursive_param_ctx payload_core);
                let payload_core = close_recursive_payload_term name num_params payload_core in
                { env = ctx.env @ [ nominal_placeholder ]; body = payload_core })
                payloads
            in
            (cname, payload_clos))
          ctors
      in
      let nominal = VNominal { id = nominal_id; name; num_params; params = []; constructors = elaborated_ctors } in
      finish_nominal nominal_id elaborated_ctors;
      (* For parameterized types, build an Explicit VPi chain so Option I64 works.
         For nullary types, just bind with VU as before. *)
      let body_ctx =
        if num_params = 0 then
          Ctx.define param_ctx name VU nominal
        else begin
          (* Push VNominal first so NomRef evaluation can find it *)
          let body_ctx = { param_ctx with
            env = nominal :: param_ctx.env;
            lvl = param_ctx.lvl + 1;
            bds = Defined :: param_ctx.bds
          } in
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
          let type_body_term = NomRef (name, type_var_terms) in
          let type_core_term =
            List.fold_right (fun _ acc -> Lam acc) params type_body_term
          in
          let type_val = Nbe.eval body_ctx.metas body_ctx.env type_core_term in
          let type_ty =
            let depth = List.length body_ctx.env in
            List.fold_right
              (fun _ acc ->
                VPi { explicitness = Explicit; domain = VU;
                      effects = effect_row_closure body_ctx.env empty_effect_row;
                      codomain = { env = body_ctx.env; body = Nbe.quote body_ctx.metas depth acc } })
              params VU
          in
          Ctx.define body_ctx name type_ty type_val
        end
      in
      let env = nominal :: body_ctx.env in
      let body_ctx =
        List.fold_left2
          (fun ctx (cname, _payload_surface) payload_clos ->
            let ctor_val, ctor_ty =
              build_ctor body_ctx.metas env name cname num_params payload_clos in
            Ctx.define ctx cname ctor_ty ctor_val)
          body_ctx ctors (List.map snd elaborated_ctors)
      in
      let body_core, body_ty = ops.infer body_ctx body in
      let ctor_payload_terms =
        List.map
          (fun (cname, payloads) ->
            let payload_terms =
              List.map
                (fun payload_expr ->
                let payload_core, payload_ty = ops.infer recursive_param_ctx payload_expr in
                check_type_like recursive_param_ctx payload_ty (Ctx.eval recursive_param_ctx payload_core);
                close_recursive_payload_term name num_params payload_core)
                payloads
            in
            (cname, payload_terms))
          ctors
      in
      (NominalDef { id = nominal_id; name; num_params; ctors = ctor_payload_terms; body = body_core },
       body_ty)
  | EffectDef { name = { name; _ }; params; ops = eff_ops; body } ->
      let params = Syntax.names params in
      let num_params = List.length params in
      let effect_id, eff, eff_ty, elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let body_ctx = Ctx.define ctx name eff_ty eff in
      let body_core, body_ty = ops.infer body_ctx body in
      (EffectDef { id = effect_id; name; num_params; ops = elaborated_ops; body = body_core },
       body_ty)
  | TraitDef { name = { name; _ }; params; fields; body } ->
      let params = Syntax.names params in
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let body_ctx = Ctx.define ctx name VU trait_ty in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (U, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name }, body_core), body_ty)
  | ImplDef { name; trait; args; fields; body } ->
      let name = Option.map (fun (i : Syntax.id) -> i.name) name in
      let body_ctx, _impl_effects, _evidence, impl_ty, impl_core =
        elaborate_impl ?impl_name:name ops ctx trait args fields in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (Ctx.quote ctx impl_ty, impl_core, body_core), body_ty)
  | Match (scrutinee, branches) ->
      let scrut_core, scrut_ty = ops.infer ctx scrutinee in
      let value_branches = value_branches_of branches in
      let effect_branches = effect_branches_of branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let ret_ty = Ctx.raw_meta ctx in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let scrutinee_effects = ops.collect_effects ctx scrutinee in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let value_branches' =
        List.map (fun (pat, body) ->
          let branch_ctx = refine_branch_context ctx refinement_target pat in
          let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
          let body_core = ops.check ctx' body ret_ty in
          ValueBranch (core_pat, body_core))
          value_branches
      in
      let effect_branches' = List.map (elaborate_effect_branch ops ctx ret_ty residual scrutinee_effects) effect_branches in
      let pats = List.map fst (core_value_branches value_branches') in
      check_match_exhaustive ctx scrut_ty pats;
      (Match (scrut_core, value_branches' @ effect_branches'), Nbe.force ctx.metas ret_ty)
  | MacroCall (f, args) ->
      let macro_name = match f.kind with Var n -> Some n.name | _ -> None in
      (match macro_name with
       | Some name ->
           (match Hashtbl.find_opt ctx.macro_table name with
             | Some (macro_fn, macro_kind, macro_nominals) ->
                (match ctx.macro_runtime with
                 | Some runtime ->
                     let ty = Ctx.raw_meta ctx in
                     (match Syntax.MacroKind.type_constraint_name macro_kind with
                      | Some constraint_name ->
                          (match resolve_dotted_value_opt ctx constraint_name with
                           | Some (constraint_val, _) -> Ctx.unify ctx ty constraint_val
                           | None -> raise (ElabError (UnboundVariable constraint_name)))
                      | None -> ());
                     run_type_aware_macro runtime ~name macro_fn macro_nominals ty args (ops.infer ctx)
                 | None -> failwith "macro runtime required")
            | None -> failwith "macro-only syntax should not reach elaboration")
       | None -> failwith "macro-only syntax should not reach elaboration")
  | MacroDef _ | SyntaxOperatorUse _ ->
      failwith "macro-only syntax should not reach elaboration"
  | Stx _ -> failwith "stx-only syntax should not reach elaboration"
  | Quote { template; holes } ->
      (* Quoted syntax is its reflection value, built here with the scopes it
         was written with. Each hole is checked against the reflection type
         its position gives it (M10); one hole in two kinds of position is an
         error, not a coercion. *)
      let ns = Elab_stdlib.syntax_nominals ctx in
      let template_value = Macro_eval.wrap_stx ~nominals:(Some ns) template in
      let occurrences = Quote_holes.occurrences template_value in
      let hole_core (name, hole) =
        let kinds = List.filter_map (fun (n, k) -> if String.equal n name then Some k else None) occurrences in
        let expected =
          match List.sort_uniq compare kinds with
          | [ Quote_holes.Expr ] -> ns.Macro_eval.expr
          | [ Quote_holes.Pattern ] -> ns.pat
          | [ Quote_holes.Id ] -> Elab_stdlib.resolve ctx [ Compiler_names.Module_name.syntax; "Id" ]
          | _ -> raise (ElabError (QuoteHoleKindConflict name))
        in
        (name, ops.check ctx hole expected)
      in
      (Quote { template = template_value; holes = List.map hole_core holes }, ns.expr)
