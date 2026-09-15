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

(* The parameters a [rec] value takes before it is a struct type:
   [struct { … }], [fn(A : Type) { struct { … } }] or [fn[A : Type] { … }]. [None] for any other value. *)
let rec struct_type_params (value : Syntax.t) =
  match value.kind with
  | Syntax.Struct _ -> Some []
  | Syntax.Lam (param, body) ->
      Option.map (fun params -> param :: params) (struct_type_params body)
  | _ -> None

(* A recursive record: [rec Numbers = struct { head : I64; tail : Option(Numbers) }].
   The binding mints an identity; its body sees the name as a recursive
   occurrence of it (a function of the parameters to one), bound by a [Let] the
   core keeps; the finished value is what an occurrence unfolds to. *)
let elab_rec_struct (ops : Elab_ops.t) (ctx : Ctx.t) ~value_ctx ~key ~name params value =
  let id = fresh_record_id () in
  let n = List.length params in
  let occ_term = List.fold_right (fun _ acc -> Lam acc) params (RecOcc { id; name; args = List.init n (fun i -> Var (n - 1 - i)) }) in
  let occ_ty =
    List.fold_right
      (fun (param : Syntax.param) acc ->
        VPi { explicitness = expl_of_syntax param.explicitness; domain = VU;
              effects = effect_row_closure ctx.Ctx.env empty_effect_row;
              codomain = { env = ctx.Ctx.env; body = Nbe.quote ctx.Ctx.metas (ctx.Ctx.lvl + 1) acc } })
      params VU
  in
  let body_ctx = Ctx.define value_ctx key occ_ty (Ctx.eval ctx occ_term) in
  let (body_core, body_ty), effects = collecting body_ctx (fun body_ctx -> ops.infer body_ctx value) in
  emit ctx effects;
  let core = Let (Ctx.quote ctx occ_ty, occ_term, body_core) in
  let finished = Ctx.eval ctx core in
  finish_record id finished;
  (core, body_ty, finished)

(* A module or struct member [name = value]: its core, its type, and the value
   the items after it see - evaluated when evaluating it performs nothing,
   otherwise opaque, for evaluating it here would run what it performs. *)
let elab_member_value (ops : Elab_ops.t) (ctx : Ctx.t) ~value_ctx ~key ~name ~recursive value =
  match if recursive then struct_type_params value else None with
  | Some params -> elab_rec_struct ops ctx ~value_ctx ~key ~name params value
  | None ->
  let rec_ty = Ctx.raw_meta ctx in
  let value_ctx = if recursive then Ctx.bind value_ctx key rec_ty else value_ctx in
  let (val_core, val_ty), effects = collecting value_ctx (fun value_ctx -> ops.infer value_ctx value) in
  emit ctx effects;
  (if recursive then Ctx.unify ctx rec_ty val_ty);
  let val_core = if recursive then Fix (name, Ctx.pure_call ctx rec_ty, val_core) else val_core in
  let val_val = if is_empty_expr_effects effects then Ctx.eval ctx val_core else VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
  (val_core, val_ty, val_val)

(* A block's [rec name : type_ = value]: its type's term, its core, and the type
   and value the body sees. A struct type is a recursive record; anything else
   is a fixpoint. *)
let elab_rec_let (ops : Elab_ops.t) (ctx : Ctx.t) ~name ~type_ value =
  let annotation = Option.map (fun ty_expr -> let _, _, ty_val = ops.type_value_of_expr ctx ty_expr in ty_val) type_ in
  match struct_type_params value with
  | Some params ->
      let core, ty, finished = elab_rec_struct ops ctx ~value_ctx:ctx ~key:name ~name:(Syntax.label name) params value in
      Option.iter (Ctx.unify ctx ty) annotation;
      (Ctx.quote ctx ty, core, ty, finished)
  | None ->
      let rec_ty = match annotation with Some ty -> ty | None -> Ctx.raw_meta ctx in
      let val_core = ops.check (Ctx.bind ctx name rec_ty) value rec_ty in
      let fix_core = Fix (name, Ctx.pure_call ctx rec_ty, val_core) in
      (Ctx.quote ctx rec_ty, fix_core, rec_ty, Ctx.eval ctx fix_core)

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
     placeholder outermost so [NomRef] can find it by id.

   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
(* A type declaration's names as the elaborator's context keys them. *)
type type_member = {
  member_key : string;  (* the context's key: the binder's resolved name *)
  member_name : string;  (* the label the type and its constructors carry *)
  member_params : string list;
  member_ctors : (string * Syntax.t list) list;
  member_ctor_keys : string list;
}

let elab_type_group (ops : Elab_ops.t) (ctx : Ctx.t) ~(members : Syntax.type_decl list) ~public
    : Ctx.t * (Core.struct_binding_term * (string * struct_field_kind * value) list) list =
  let members =
    List.map
      (fun (m : Syntax.type_decl) ->
        { member_key = m.name.name; member_name = Syntax.label m.name.name; member_params = Syntax.names m.params;
          member_ctors = List.map (fun ((c : Syntax.id), payloads) -> (Syntax.label c.name, payloads)) m.ctors;
          member_ctor_keys = List.map (fun ((c : Syntax.id), _) -> c.name) m.ctors })
      members
  in
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
  let group = List.map (fun ((m : type_member), id, _, _, _, _) -> (id, m.member_name, List.length m.member_params)) registered in
  (* Phase 2, elaborate: every member's payloads, in a context naming every
     member. Those names are temporary - they contribute no width. *)
  let elaborated =
    List.map
      (fun ((m : type_member), nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty) ->
        let group_ctx =
          List.fold_left
            (fun gctx ((other : type_member), other_id, other_placeholder, _, _, other_ty) ->
              let num_params = List.length other.member_params in
              if num_params = 0 then Ctx.define gctx other.member_key VU other_placeholder
              else
                let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) other.member_params in
                let type_core_term =
                  List.fold_right (fun _ acc -> Lam acc) other.member_params (NomRef { id = other_id; name = other.member_name; params = type_var_terms })
                in
                let type_val = Nbe.eval param_ctx.Ctx.metas (other_placeholder :: param_ctx.Ctx.env) type_core_term in
                Ctx.define gctx other.member_key other_ty type_val)
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
                (fun key ((_, ctor_value), (_, ctor_ty)) -> `Entry (key, ctor_ty, ctor_value))
                m.member_ctor_keys (List.combine ctor_values ctor_types)
            @ [ `Entry (m.member_key, nominal_ty, nominal) ])
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
  | Syntax.FieldBinding _ -> failwith "a field is expanded only inside a struct"
  | Syntax.MacroBinding _ | Syntax.SyntaxBinding _ -> (ctx, [], [])
  | Syntax.MacroCallBinding _ -> (ctx, [], [])
  | Syntax.HoleBinding _ | Syntax.Items _ | Syntax.InstantiateBinding _ -> failwith "unexpanded declarations should not reach elaboration"
  | Syntax.PatternSynBinding { name = { name = key; _ }; params; rhs; public } ->
      let name = Syntax.label key in
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
      let ctx' = extend_from_slots ctx bind [ `Entry (key, VU, syn_val) ] in
      (ctx', [bind], [ModuleField (name, kind, VU)])
  | Syntax.OpenBinding (mod_expr, label) ->
      (* Module-level [open]: the opened module's public fields are in scope for
         the bindings that *follow* (the caller folds this ctx forward), and the
         open exports nothing itself. [OpenBind] carries the same scope
         extension to the evaluator. *)
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      let mod_value = Ctx.eval ctx mod_core in
      let ctx, members = open_module_value ~label ctx mod_ty mod_value in
      (ctx, [OpenBind (mod_core, members)], [])
  | Syntax.LetBinding { name = { name = key; _ }; value; public; recursive } ->
      let name = Syntax.label key in
      let val_core, val_ty, val_val = elab_member_value ops ctx ~value_ctx:(Ctx.clear_self_scope ctx) ~key ~name ~recursive value in
      let kind = if public then Public else Private in
      let bind = LetBind (name, kind, val_core) in
      let ctx' = extend_from_slots ctx bind [ `Entry (key, val_ty, val_val) ] in
      (ctx', [bind], [ModuleField (name, kind, val_ty)])
  | Syntax.EffectBinding { name = { name = key; _ }; params; ops = eff_ops; public } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let _effect_id, eff, eff_ty, _elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let kind = if public then Public else Private in
      let bind = EffectBind (name, kind, eff) in
      let ctx' = extend_from_slots ctx bind [ `Entry (key, eff_ty, eff) ] in
      (ctx', [bind], [ModuleField (name, kind, eff_ty)])
  | Syntax.TraitBinding { name = { name = key; _ }; params; fields; public } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let kind = if public then Public else Private in
      let bind =
        LetBind (name, kind, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name })
      in
      let ctx' = extend_from_slots ctx bind [ `Entry (key, VU, trait_ty) ] in
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
  | Syntax.TypeBinding { members; public } ->
      (* The module fold prepends each binding's results and reverses at the
         end, so a chain's binds and entries come back last member first. *)
      let ctx', results = elab_type_group ops ctx ~members ~public in
      (ctx', List.rev_map fst results,
       List.rev_map (fun (name, kind, ty) -> ModuleField (name, kind, ty)) (List.concat_map snd results))

(* Quoted syntax is its reflection value, built with the scopes it was written
   with. Each hole is checked against the reflection type its position gives it
   (M10); one hole in two kinds of position is an error, not a coercion. *)
let infer ops (ctx : Ctx.t) (expr : Syntax.t) : term * value =
  match expr.kind with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy Atom_ty.TI64)
  | Atom Unit -> (Atom Unit, VAtomTy Atom_ty.TUnit)
  | Atom (Char c) -> (Atom (Char c), VAtomTy Atom_ty.TChar)
  | Atom (String s) -> (Atom (String s), VAtomTy Atom_ty.TString)
  | Atom (Scopes _ as s) -> (Atom s, VAtomTy Atom_ty.TScopes)
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
      let effect_core, effect_value, input_ty, output_ty = resolve_perform_operation ctx op_path in
      let arg_core = ops.check ctx arg input_ty in
      emit ctx (singleton_expr_effect effect_core effect_value);
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
        let ty_term, fix_core, rec_ty, fix_val = elab_rec_let ops ctx ~name ~type_ value in
        let ctx' = Ctx.define ctx name rec_ty fix_val in
        let body_core, body_ty = ops.infer ctx' body in
        (Let (ty_term, fix_core, body_core), body_ty)
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
        let body_core, body_ty = ops.infer ctx' body in
        (Let (ty_term, gen_val_core, body_core), body_ty)
      end
  | Lam (param, body) -> infer_lam ops ctx param body
  | Annotated { inner; typ } ->
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
            let elem_core, elem_ty = pure ctx (fun ctx -> ops.infer ctx elem) in
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
          let effects = Elab_type_expr.elaborate_effect_row ops type_ctx effects in
          let b_core, b_ty = pure dict_ctx (fun dict_ctx -> ops.infer dict_ctx b) in
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
          let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
          let ctx' = Ctx.bind ctx name a_val in
          let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
          let b_core, b_ty = pure ctx' (fun ctx' -> ops.infer ctx' b) in
          check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
          (Pi { explicitness = Implicit; domain = a_core; effects; codomain = b_core }, VU))
  | Arrow (expl, name, a, effects, b) ->
      let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
      let ctx' = Ctx.bind ctx (Option.fold ~none:"_" ~some:(fun (i : Syntax.id) -> i.name) name) a_val in
      let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
      let b_core, b_ty = pure ctx' (fun ctx' -> ops.infer ctx' b) in
      check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
      (Pi { explicitness = expl_of_syntax expl; domain = a_core; effects; codomain = b_core }, VU)
  | FieldAccess (head, name)
    when Option.is_some (trait_of_form_opt ctx head) ->
      resolve_trait_method ctx (Option.get (trait_of_form_opt ctx head)) name
  | FieldAccess (e, name) ->
      let e_core, e_ty = ops.infer ctx e in
      let e_core, e_ty = insert_implicit_args ctx e_core e_ty in
      (match Nbe.force_shape ctx.metas e_ty with
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
          match Nbe.force_shape ctx.metas typ with
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
  | Import { path; _ } when String.equal path Compiler_names.Module_name.std_import_path ->
      (* Reserved path: [import "std"] resolves to the builtin prelude module,
         already elaborated and bound by [init_ctx] as [stdlib]. Resolving it
         here (typecheck layer) keeps the loader from having to name the prelude
         upward across the layer boundary. *)
      let ix, ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
      (Var ix, ty)
  | Import { path; _ } -> (
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
                    let unit_ctx = Ctx.with_expander (Ctx.unit_base ctx) expand_ctx in
                    (* A unit's top-level bindings run when it loads: a program's top. *)
                    let (core, ty), effects = collecting unit_ctx (fun unit_ctx -> ops.infer unit_ctx imported) in
                    Elab_effects.require_handled_at_entry unit_ctx effects;
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
  | Sig { bindings } -> Elab_type_expr.infer_signature ops ctx bindings
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
      (Module { bindings = core_bindings; signature = false }, VModule { entries; partial = false })
  | Struct { bindings } ->
      (* Items elaborate in source order (as a module's do): a field's type sees
         the items written before it, and leaves as a value, quoted at the
         struct's own level - every slot an item adds holds a value. A method
         needs [self]'s type, which is every field, so a method written before
         the last field is elaborated right after it. *)
      let outer = ctx in
      let fields = ref [] and deferred = ref [] in
      let partial_self () =
        VStruct { entries = List.rev_map (fun (name, _, ty) -> StructField (name, Field, ty)) !fields; partial = true }
      in
      let rec elaborate_method_params ctx params body =
        match params with
        | [] ->
            (* ponytail: a method's type carries no row, so what its body performs is
               dropped here (as before); give the innermost arrow the body's row when
               methods get latent effects. *)
            fst (collecting ctx (fun ctx -> ops.infer ctx body))
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
        let self_ty = partial_self () in
        let ctx = Ctx.with_self_type ctx self_ty in
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
      (* [Self] in an item other than a field is the fields written so far; a
         field's own type keeps the enclosing [Self] (a record declaration's). *)
      let rec go ~defer ctx acc items = go_item ~defer (Ctx.with_self_type ctx (partial_self ())) acc items
      and go_item ~defer ctx (acc_binds, acc_entries) = function
        | [] -> (ctx, (acc_binds, acc_entries))
        | Syntax.FieldBinding { name; type_ } :: rest ->
            let mentions (m : Syntax.id) =
              let found = ref false in
              ignore (Expand.map_ids (fun (i : Syntax.id) -> if String.equal i.name m.name then found := true; i) type_);
              !found
            in
            (match List.find_map (function Syntax.MethodBinding { name = m; _ } when mentions m -> Some m | _ -> None) !deferred with
             | Some m -> raise (ElabError (FieldTypeMentionsMethod { field = name; method_ = m.name }))
             | None -> ());
            let _core, _ty_ty, value = ops.type_value_of_expr { ctx with Ctx.self_type = outer.Ctx.self_type } type_ in
            fields := (name, Ctx.quote outer value, value) :: !fields;
            go ~defer ctx (acc_binds, acc_entries) rest
        | (Syntax.MethodBinding _ as m) :: rest when defer ->
            deferred := m :: !deferred;
            go ~defer ctx (acc_binds, acc_entries) rest
        | (Syntax.MacroBinding _ | Syntax.SyntaxBinding _) :: rest -> go ~defer ctx (acc_binds, acc_entries) rest
        | (Syntax.HoleBinding _ | Syntax.Items _ | Syntax.InstantiateBinding _) :: _ -> failwith "unexpanded declarations should not reach elaboration"
        | Syntax.MacroCallBinding _ :: rest -> go ~defer ctx (acc_binds, acc_entries) rest
        | Syntax.PatternSynBinding { name = { name = key; _ }; params; rhs; public } :: rest ->
            let name = Syntax.label key in
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
            let ctx' = extend_from_slots ctx bind [ `Entry (key, VU, syn_val) ] in
            go ~defer ctx'
               (bind :: acc_binds,
                StructField (name, kind, VU) :: acc_entries)
              rest
        | Syntax.OpenBinding (mod_expr, label) :: rest ->
            let mod_core, mod_ty = ops.infer ctx mod_expr in
            let mod_value = Ctx.eval ctx mod_core in
            let ctx, members = open_module_value ~label ctx mod_ty mod_value in
            go ~defer ctx (OpenBind (mod_core, members) :: acc_binds, acc_entries) rest
        | Syntax.LetBinding { name = { name = key; _ }; value; public; recursive; _ } :: rest ->
            let name = Syntax.label key in
            let val_core, val_ty, val_val = elab_member_value ops ctx ~value_ctx:(Ctx.clear_self ctx) ~key ~name ~recursive value in
            let kind = if public then Public else Private in
            let bind = LetBind (name, kind, val_core) in
            let ctx' = extend_from_slots ctx bind [ `Entry (key, val_ty, val_val) ] in
            let entries = if public then [ StructField (name, kind, val_ty) ] else [] in
            go ~defer ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.MethodBinding { name = { name = key; _ }; params; body; public } :: rest ->
            let name = Syntax.label key in
            let method_core, method_ty = elaborate_method ctx params body in
            let method_val = Ctx.eval ctx method_core in
            let kind = if public then Method else PrivateMethod in
            let bind = LetBind (name, kind, method_core) in
            let ctx' = extend_from_slots ctx bind [ `Entry (key, method_ty, method_val) ] in
            let entries = if public then [ StructField (name, kind, method_ty) ] else [] in
            go ~defer ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.EffectBinding { name = { name = key; _ }; params; ops = eff_ops; public } :: rest ->
            let name = Syntax.label key in
            let params = Syntax.names params in
            let _effect_id, eff, eff_ty, _elaborated_ops =
              elaborate_eff_family ops ctx name params eff_ops
            in
            let kind = if public then Public else Private in
            let bind = EffectBind (name, kind, eff) in
            let ctx' = extend_from_slots ctx bind [ `Entry (key, eff_ty, eff) ] in
            let entries = if public then [ StructField (name, kind, eff_ty) ] else [] in
            go ~defer ctx'
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
            go ~defer ctx'
              (bind :: acc_binds,
               StructImpl (name, kind, c.impl_dict_ty, c.impl_value) :: acc_entries)
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
            go ~defer ctx' acc rest
      in
      let is_field = function Syntax.FieldBinding _ -> true | _ -> false in
      let rec split_after_last_field = function
        | items when not (List.exists is_field items) -> ([], items)
        | item :: rest -> let before, after = split_after_last_field rest in (item :: before, after)
        | [] -> ([], [])
      in
      let before, after = split_after_last_field bindings in
      let ctx', acc = go ~defer:true ctx ([], []) before in
      let ctx', acc = go ~defer:false ctx' acc (List.rev !deferred) in
      let _end_ctx, (rev_binds, rev_entries) = go ~defer:false ctx' acc after in
      let con = List.rev !fields in
      check_duplicate_names (List.map (fun (n, _, _) -> n) con);
      (Struct { con_fields = List.map (fun (n, c, _) -> (n, c)) con; bindings = List.rev rev_binds; partial = false },
       VStruct { entries = List.map (fun (n, _, ty) -> StructField (n, Field, ty)) con @ List.rev rev_entries; partial = false })
  | OpenChoice { name = { name; _ }; opens; fallback } -> (
      match Ctx.lookup_choice_opt ctx name { opens; fallback } with
      | Some (ix, ty) -> (Var ix, ty)
      | None -> raise (ElabError (UnboundVariable name)))
  | Open (mod_expr, body, label) ->
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      let mod_value = Ctx.eval ctx mod_core in
      let body_ctx, members = open_module_value ~label ctx mod_ty mod_value in
      let body_core, body_ty = ops.infer body_ctx body in
      (Open (mod_core, members, body_core), body_ty)
  | TypeDef { name = { name = key; _ }; params; ctors; body } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let ctor_keys = List.map (fun ((c : Syntax.id), _) -> c.name) ctors in
      let ctors = List.map (fun ((c : Syntax.id), payloads) -> (Syntax.label c.name, payloads)) ctors in
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
        if num_params = 0 then Ctx.define param_ctx key VU nominal_placeholder
        else
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
          let type_body_term = NomRef { id = nominal_id; name; params = type_var_terms } in
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
          Ctx.define param_ctx key type_ty type_val
      in
      let elaborated_ctors =
        List.map
          (fun (cname, payloads) ->
            let payload_clos =
              List.map
                (fun payload_expr ->
                let payload_core, payload_ty = ops.infer recursive_param_ctx payload_expr in
                check_type_like recursive_param_ctx payload_ty (Ctx.eval recursive_param_ctx payload_core);
                let payload_core = close_recursive_payload_term nominal_id name num_params payload_core in
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
          Ctx.define param_ctx key VU nominal
        else begin
          (* Push VNominal first so NomRef evaluation can find it by id *)
          let body_ctx = { param_ctx with
            env = nominal :: param_ctx.env;
            lvl = param_ctx.lvl + 1;
            bds = Defined :: param_ctx.bds
          } in
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
          let type_body_term = NomRef { id = nominal_id; name; params = type_var_terms } in
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
          Ctx.define body_ctx key type_ty type_val
        end
      in
      let env = nominal :: body_ctx.env in
      let body_ctx =
        List.fold_left2
          (fun ctx (key, (cname, _payload_surface)) payload_clos ->
            let ctor_val, ctor_ty =
              build_ctor body_ctx.metas env name cname num_params payload_clos in
            Ctx.define ctx key ctor_ty ctor_val)
          body_ctx (List.combine ctor_keys ctors) (List.map snd elaborated_ctors)
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
                close_recursive_payload_term nominal_id name num_params payload_core)
                payloads
            in
            (cname, payload_terms))
          ctors
      in
      (NominalDef { id = nominal_id; name; num_params; ctors = ctor_payload_terms; body = body_core },
       body_ty)
  | EffectDef { name = { name = key; _ }; params; ops = eff_ops; body } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let num_params = List.length params in
      let effect_id, eff, eff_ty, elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let body_ctx = Ctx.define ctx key eff_ty eff in
      let body_core, body_ty = ops.infer body_ctx body in
      (EffectDef { id = effect_id; name; num_params; ops = elaborated_ops; body = body_core },
       body_ty)
  | TraitDef { name = { name = key; _ }; params; fields; body } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let body_ctx = Ctx.define ctx key VU trait_ty in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (U, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name }, body_core), body_ty)
  | ImplDef { name; trait; args; fields; body } ->
      let name = Option.map (fun (i : Syntax.id) -> i.name) name in
      let body_ctx, _evidence, impl_ty, impl_core =
        elaborate_impl ?impl_name:name ops ctx trait args fields in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (Ctx.quote ctx impl_ty, impl_core, body_core), body_ty)
  | Match (scrutinee, branches) ->
      let (scrut_core, scrut_ty), scrutinee_effects = collecting ctx (fun ctx -> ops.infer ctx scrutinee) in
      let value_branches = value_branches_of branches in
      let effect_branches = effect_branches_of branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let ret_ty = Ctx.raw_meta ctx in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let (value_branches', effect_branches'), body_effects =
        collecting ctx (fun ctx ->
          ( List.map (fun (pat, body) ->
              let branch_ctx = refine_branch_context ctx refinement_target pat in
              let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
              let body_core = ops.check ctx' body ret_ty in
              ValueBranch (core_pat, body_core))
              value_branches,
            List.map (elaborate_effect_branch ops ctx ret_ty residual scrutinee_effects) effect_branches ))
      in
      emit_residual ctx ~residual_of:(fun effects -> residual_effects ctx effects effect_branches) scrutinee_effects body_effects;
      let pats = List.map fst (core_value_branches value_branches') in
      check_match_exhaustive ctx scrut_ty pats;
      (Match (scrut_core, value_branches' @ effect_branches'), Nbe.force ctx.metas ret_ty)
  | MacroCall (f, args) ->
      let macro_name = match f.kind with Var n -> Some n.name | _ -> None in
      (match macro_name with
       | Some name ->
           apply_typed_macro ~check:ops.check ctx ~name args ~expected:None
       | None -> failwith "macro-only syntax should not reach elaboration")
  | MacroDef _ | SyntaxDef _ | SyntaxOperatorUse _ | Block _ | Instantiate _ ->
      failwith "macro-only syntax should not reach elaboration"
  | Stx _ -> failwith "stx-only syntax should not reach elaboration"
  (* A typed macro argument, elaborated where the call was written: the output
     places it under at most the binders it added there, so its core is weakened
     past them. An argument whose core holds an [open] cannot be, and elaborates
     again from its expanded form. *)
  | Elaborated { arg; form } -> (
      match Hashtbl.find_opt elaborated_args arg with
      | Some (core, ty, at_lvl, effects) when ctx.lvl >= at_lvl && (ctx.lvl = at_lvl || shiftable core) ->
          emit ctx effects;
          (shift_term (ctx.lvl - at_lvl) 0 core, ty)
      | Some _ -> ops.infer ctx form
      | None -> failwith "Elab_infer: an elaborated macro argument outlived its application")
  | Quote { template; holes } ->
      let ns = Elab_stdlib.syntax_nominals ctx in
      (quote_core ~check:ops.check ctx (Macro_eval.wrap_stx ~nominals:(Some ns) template) holes, ns.expr)
  | QuoteDecls { items; holes } ->
      let ns = Elab_stdlib.syntax_nominals ctx in
      ( quote_core ~check:ops.check ctx (Macro_eval.wrap_stx_decl ~nominals:(Some ns) items) holes,
        Elab_stdlib.resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.decls ] )
