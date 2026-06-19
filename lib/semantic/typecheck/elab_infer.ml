open Core
include Elab_error
open Elab_common
open Elab_validate
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_surface_rewrite
open Elab_resolve
open Elab_refine
open Elab_patterns
open Elab_match
open Elab_defs
open Elab_generalize
open Elab_apply
open Elab_ops

let infer ops (ctx : Ctx.t) (expr : Surface.t) : term * value =
  match expr with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy Atom_ty.TI64)
  | Atom (Bool b) -> (Atom (Bool b), VAtomTy Atom_ty.TBool)
  | Atom Unit -> (Atom Unit, VAtomTy Atom_ty.TUnit)
  | Atom (Char c) -> (Atom (Char c), VAtomTy Atom_ty.TChar)
  | Atom (String s) -> (Atom (String s), VAtomTy Atom_ty.TString)
  | Var "EffectRow" -> (EffectRowTy, VU)
  | Var name ->
      let ix, ty = Ctx.lookup ctx name in
      let core = Var ix in
      let core =
        let prefix = "stx_" in
        if String.length name >= String.length prefix
           && String.sub name 0 (String.length prefix) = prefix then
          match Ctx.eval ctx core with
          | VNeutral { neutral = { head = HPrim pname; _ }; _ } -> Prim pname
          | _ -> core
        else core
      in
      (core, ty)
  | Self ->
      let ix, ty = Ctx.lookup_self ctx in
      (Var ix, ty)
  | SelfType ->
      (Ctx.quote ctx (Ctx.lookup_self_type ctx), VU)
  | Perform { effect_path; op; arg } ->
      let effect_core, _effect_value, input_ty, output_ty = resolve_perform_operation ctx ~effect_path ~op in
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
  | Let { name; type_; value; body; recursive } ->
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
  | If { cond; then_; else_ } ->
      let cond_core = ops.check ctx cond (VAtomTy Atom_ty.TBool) in
      let then_core, then_ty = ops.infer ctx then_ in
      let else_core = ops.check ctx else_ then_ty in
      (If (cond_core, then_core, else_core), then_ty)
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
  | Arrow (Explicitness.Implicit, Some name, a, effects, b) -> (
      match surface_trait_bound_names a with
      | Some trait_names when List.for_all (fun trait_name -> NameMap.mem trait_name ctx.Ctx.traits) trait_names ->
          let type_ctx = Ctx.bind ctx name VU in
          let arg = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
          let dict_ctx, dict_layers =
            List.fold_left
              (fun (c, layers) trait_name ->
                let trait_info = lookup_trait ctx trait_name in
                let dict_ty = trait_dict_ty ~trait_id:trait_info.trait_id trait_name [ arg ] (eval_trait_fields ctx trait_info [ arg ]) in
                let dict_core = Ctx.quote c dict_ty in
                let c', entry = Ctx.bind_anonymous c dict_ty in
                let evidence =
                  { evidence_trait_id = trait_info.trait_id;
                    evidence_trait_name = trait_name;
                    evidence_args = [ arg ];
                    evidence_level = entry.level;
                    evidence_ty = dict_ty }
                in
                (Ctx.add_trait_evidence c' evidence, layers @ [ dict_core ]))
              (type_ctx, []) trait_names
          in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects type_ctx (ops.collect_effects type_ctx eff)) row.effects; Option.iter (fun tail -> require_empty_effects type_ctx (ops.collect_effects type_ctx tail)) row.tail) effects;
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
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) effects;
          let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
          require_empty_effects ctx' (ops.collect_effects ctx' b);
          let b_core, b_ty = ops.infer ctx' b in
          check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
          (Pi { explicitness = Implicit; domain = a_core; effects; codomain = b_core }, VU))
  | Arrow (expl, name, a, effects, b) ->
      require_empty_effects ctx (ops.collect_effects ctx a);
      let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
      let ctx' = Ctx.bind ctx (Option.value name ~default:"_") a_val in
      Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects ctx' (ops.collect_effects ctx' eff)) row.effects; Option.iter (fun tail -> require_empty_effects ctx' (ops.collect_effects ctx' tail)) row.tail) effects;
      let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
      require_empty_effects ctx' (ops.collect_effects ctx' b);
      let b_core, b_ty = ops.infer ctx' b in
      check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
      (Pi { explicitness = expl_of_surface expl; domain = a_core; effects; codomain = b_core }, VU)
  | FieldAccess (Var trait_name, name) when NameMap.mem trait_name ctx.Ctx.traits ->
      resolve_trait_method ctx (lookup_trait ctx trait_name) name
  | FieldAccess (e, name) ->
      let e_core, e_ty = ops.infer ctx e in
      let e_core, e_ty = insert_implicit_args ctx e_core e_ty in
      (match Nbe.force ctx.metas e_ty with
      | VModule { entries; partial = _ } -> (
          match List.find_opt (fun (n, _, _) -> String.equal n name) (visible_module_fields entries) with
          | Some (_, _, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
          | None -> raise (ElabError (UnboundVariable name)))
      | VStruct { entries; partial } -> (
          let fields = struct_entry_fields entries in
          match Nbe.force ctx.metas (Ctx.eval ctx e_core) with
          | VStruct _ -> (
              match List.find_opt (fun (n, _, _) -> String.equal n name) (visible_struct_members fields) with
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
  | Import path -> (
      match ctx.loader with
      | Some loader ->
          let core, _value, ty =
            Core_loader.load_elaborated loader path ~elaborate:(fun imported ->
                let core, ty = ops.infer ctx imported in
                (core, Ctx.eval ctx core, ty))
          in
          (core, ty)
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
      let rec go ctx (acc_binds, acc_entries) = function
        | [] -> (ctx, List.rev acc_binds, List.rev acc_entries)
        | Surface.MethodBinding _ :: _ -> failwith "module binding cannot be method"
        | Surface.MacroBinding _ :: rest -> go ctx (acc_binds, acc_entries) rest
        | Surface.PatternSynBinding { name; params; rhs; public } :: rest ->
            let scrutinee_ty =
              try
                let ix, _ty = Ctx.lookup ctx "Expr" in
                let expr_val = Ctx.eval ctx (Var ix) in
                match Nbe.force ctx.metas expr_val with
                | VNominal _ -> expr_val
                | _ -> VU
              with _ -> VU
            in
            let core_rhs, _binders = Elab_patterns.elaborate_pat_binders ctx rhs scrutinee_ty in
            let syn_val = VPatternSyn { name; params; rhs = core_rhs; scrutinee_ty } in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name VU syn_val in
            go ctx'
              (PatternSynBind (name, kind, syn_val) :: acc_binds,
               ModuleField (name, kind, VU) :: acc_entries)
              rest
        | Surface.LetBinding { name; value; public } :: rest ->
            let value_ctx = Ctx.clear_self_scope ctx in
            let val_core, val_ty = ops.infer value_ctx value in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               ModuleField (name, kind, val_ty) :: acc_entries)
              rest
        | Surface.EffectBinding { name; params; ops = eff_ops; public } :: rest ->
            let _effect_id, eff, eff_ty, _elaborated_ops =
              elaborate_eff_family ops ctx name params eff_ops
            in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name eff_ty eff in
            go ctx'
              (EffectBind (name, kind, eff) :: acc_binds,
               ModuleField (name, kind, eff_ty) :: acc_entries)
              rest
        | Surface.TraitBinding { name; params; fields; public } :: rest ->
            let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
            let kind = if public then Public else Private in
            let ctx' = Ctx.add_trait (Ctx.define ctx name VU trait_ty) trait_info in
            go ctx'
              (LetBind (name, kind, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name }) :: acc_binds,
               ModuleField (name, kind, VU) :: acc_entries)
              rest
        | Surface.ImplBinding { trait_path = []; trait_name; args; fields; public } :: rest ->
            let ctx', _impl_effects, _evidence, impl_ty, impl_core = elaborate_impl ops ctx trait_name args fields in
            let kind = if public then Public else Private in
            go ctx'
              (ImplBind (kind, impl_core, impl_ty) :: acc_binds,
               ModuleImpl (kind, impl_ty, Ctx.eval ctx impl_core) :: acc_entries)
              rest
        | Surface.ImplBinding { trait_path = _ :: _; trait_name; _ } :: _ ->
            raise (ElabError (UnknownTrait trait_name))
        | Surface.RecordTypeBinding { name; params; fields; public } :: rest ->
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
                    (Surface.Struct { con_fields = rewritten_fields; bindings = [] })
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
            let ctx' = Ctx.define ctx name val_ty val_val in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               ModuleField (name, kind, val_ty) :: acc_entries)
              rest
        | Surface.TypeBinding { name; params; ctors; public } :: rest ->
            let num_params = List.length params in
            if num_params = 0 then begin
              let nominal_id = NominalId.fresh () in
              let nominal_placeholder = VNominal { id = nominal_id; name; num_params = 0; params = []; constructors = [] } in
              let tmp_ctx =
                { ctx with env = nominal_placeholder :: ctx.env;
                           types = VU :: ctx.types;
                           lvl = ctx.lvl + 1;
                           bds = Defined :: ctx.bds;
                           name_table = NameMap.add name { level = ctx.lvl; ty = VU } ctx.name_table }
              in
              let elaborated_ctors =
                List.map
                  (fun (cname, payloads) ->
                    let payload_clos =
                      List.map
                        (fun payload_expr ->
                        let payload_core, payload_ty = ops.infer tmp_ctx payload_expr in
                        check_type_like tmp_ctx payload_ty (Ctx.eval tmp_ctx payload_core);
                        { env = tmp_ctx.env; body = payload_core })
                        payloads
                    in
                    (cname, payload_clos))
                  ctors
              in
              let nominal = VNominal { id = nominal_id; name; num_params = 0; params = []; constructors = elaborated_ctors } in
              let kind = if public then Public else Private in
              let ctor_values, ctor_types =
                List.split
                  (List.map
                     (fun (cname, payload_clo_opt) ->
                       let ctor_value, ctor_ty =
                         build_ctor tmp_ctx.metas (nominal :: tmp_ctx.env) name cname 0 payload_clo_opt
                       in
                       ((cname, ctor_value), (cname, ctor_ty)))
                     elaborated_ctors)
              in
              let ctx =
                List.fold_left
                  (fun ctx ((ctor_name, ctor_value), (_, ctor_ty)) ->
                    Ctx.define ctx ctor_name ctor_ty ctor_value)
                  ctx (List.combine ctor_values ctor_types)
              in
              let ctx = Ctx.define ctx name VU nominal in
              let fields = (name, kind, VU) :: List.map (fun (c, ty) -> (c, kind, ty)) ctor_types in
              go ctx
                (TypeBind (name, kind, nominal, ctor_values) :: acc_binds,
                 List.rev_append (List.map (fun (name, kind, ty) -> ModuleField (name, kind, ty)) fields) acc_entries)
                rest
            end else begin
              let param_ctx =
                List.fold_left
                  (fun ctx param_name ->
                    Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
                  ctx params
              in
              let nominal_id = NominalId.fresh () in
              let nominal_placeholder = VNominal { id = nominal_id; name; num_params = 0; params = []; constructors = [] } in
              let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
              let type_body_term = NomRef (name, type_var_terms) in
              let type_core_term = List.fold_right (fun _ acc -> Lam acc) params type_body_term in
              let _type_val = Nbe.eval param_ctx.metas (nominal_placeholder :: param_ctx.env) type_core_term in
              let type_ty =
                let depth = List.length param_ctx.env + 1 in
                List.fold_right
                  (fun _ acc ->
                    VPi { explicitness = Explicit; domain = VU;
                          effects = effect_row_closure (nominal_placeholder :: param_ctx.env) empty_effect_row;
                          codomain = { env = nominal_placeholder :: param_ctx.env; body = Nbe.quote param_ctx.metas depth acc } })
                  params VU
              in
              (* Build a temporary context with the recursive name available
                 for payload elaboration, without adding a permanent env entry. *)
              let tmp_ctx =
                { param_ctx with env = nominal_placeholder :: param_ctx.env;
                                lvl = param_ctx.lvl + 1;
                                name_table = NameMap.add name { level = param_ctx.lvl; ty = type_ty } param_ctx.name_table }
              in
              let elaborated_ctors =
                List.map
                  (fun (cname, payloads) ->
                    let payload_clos =
                      List.map
                        (fun payload_expr ->
                        let payload_core, payload_ty = ops.infer tmp_ctx payload_expr in
                        check_type_like tmp_ctx payload_ty (Ctx.eval tmp_ctx payload_core);
                        let payload_core = close_recursive_payload_term name num_params payload_core in
                        { env = ctx.env @ [ nominal_placeholder ]; body = payload_core })
                        payloads
                    in
                    (cname, payload_clos))
                  ctors
              in
              let nominal = VNominal { id = nominal_id; name; num_params; params = []; constructors = elaborated_ctors } in
              let kind = if public then Public else Private in
              let ctor_values, ctor_types =
                List.split
                  (List.map
                     (fun (cname, payload_clo_opt) ->
                       let ctor_value, ctor_ty =
                         build_ctor tmp_ctx.metas (nominal :: tmp_ctx.env) name cname num_params payload_clo_opt
                       in
                       ((cname, ctor_value), (cname, ctor_ty)))
                     elaborated_ctors)
              in
              let ctx_after_ctors =
                List.fold_left
                  (fun ctx ((ctor_name, ctor_value), (_, ctor_ty)) ->
                    Ctx.define ctx ctor_name ctor_ty ctor_value)
                  param_ctx (List.combine ctor_values ctor_types)
              in
              let ctx = Ctx.define ctx_after_ctors name type_ty nominal in
              let fields = (name, kind, type_ty) :: List.map (fun (c, ty) -> (c, kind, ty)) ctor_types in
              go ctx
                (TypeBind (name, kind, nominal, ctor_values) :: acc_binds,
                 List.rev_append (List.map (fun (name, kind, ty) -> ModuleField (name, kind, ty)) fields) acc_entries)
                rest
            end
      in
      let _ctx, core_bindings, entries = go (Ctx.clear_self_scope ctx) ([], []) bindings in
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
              match param.Surface.type_ with
              | Some ty_expr ->
                  let _ty_core, _ty_ty, ty_value = ops.type_value_of_expr ctx ty_expr in
                  ty_value
              | None -> Ctx.raw_meta ctx
            in
            let ctx' = Ctx.bind ctx param.name a_ty in
            let body_core, body_ty = elaborate_method_params ctx' rest body in
            let body_ty_term = Ctx.quote ctx' body_ty in
            let method_ty =
              VPi {
                explicitness = expl_of_surface param.explicitness;
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
        | [] -> (List.rev acc_binds, List.rev acc_entries)
        | Surface.MacroBinding _ :: rest -> go ctx (acc_binds, acc_entries) rest
        | Surface.PatternSynBinding { name; params; rhs; public } :: rest ->
            let scrutinee_ty =
              try
                let ix, _ty = Ctx.lookup ctx "Expr" in
                let expr_val = Ctx.eval ctx (Var ix) in
                match Nbe.force ctx.metas expr_val with
                | VNominal _ -> expr_val
                | _ -> VU
              with _ -> VU
            in
            let core_rhs, _binders = Elab_patterns.elaborate_pat_binders ctx rhs scrutinee_ty in
            let syn_val = VPatternSyn { name; params; rhs = core_rhs; scrutinee_ty } in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name VU syn_val in
            go ctx'
              (PatternSynBind (name, kind, syn_val) :: acc_binds,
               StructField (name, kind, VU) :: acc_entries)
              rest
        | Surface.LetBinding { name; value; public } :: rest ->
            let value_ctx = Ctx.clear_self ctx in
            let val_core, val_ty = ops.infer value_ctx value in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            let entries = if public then [ StructField (name, kind, val_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Surface.MethodBinding { name; params; body; public } :: rest ->
            let method_core, method_ty = elaborate_method ctx params body in
            let method_val = Ctx.eval ctx method_core in
            let kind = if public then Method else PrivateMethod in
            let ctx' = Ctx.define ctx name method_ty method_val in
            let entries = if public then [ StructField (name, kind, method_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, method_core) :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Surface.EffectBinding { name; params; ops = eff_ops; public } :: rest ->
            let _effect_id, eff, eff_ty, _elaborated_ops =
              elaborate_eff_family ops ctx name params eff_ops
            in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name eff_ty eff in
            let entries = if public then [ StructField (name, kind, eff_ty) ] else [] in
            go ctx'
              (EffectBind (name, kind, eff) :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Surface.TraitBinding _ :: _ ->
            raise (ElabError ApplyingNonFunction)
        | Surface.ImplBinding { trait_path = []; trait_name; args; fields; public } :: rest ->
            let ctx', _impl_effects, _evidence, impl_ty, impl_core = elaborate_impl ops ctx trait_name args fields in
            let kind = if public then Public else Private in
            go ctx'
              (ImplBind (kind, impl_core, impl_ty) :: acc_binds,
               StructImpl (kind, impl_ty, Ctx.eval ctx impl_core) :: acc_entries)
              rest
        | Surface.ImplBinding { trait_path = _ :: _; trait_name; _ } :: _ ->
            raise (ElabError (UnknownTrait trait_name))
        | Surface.RecordTypeBinding { name; params; fields; public } :: rest ->
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
                    (Surface.Struct { con_fields = rewritten_fields; bindings = [] })
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
            let ctx' = Ctx.define ctx name val_ty val_val in
            let entries = if public then [ StructField (name, kind, val_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Surface.TypeBinding { name; params; ctors; public } :: rest ->
            let num_params = List.length params in
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
              let _type_val = Nbe.eval param_ctx.metas (nominal_placeholder :: param_ctx.env) type_core_term in
                let type_ty =
                  let depth = List.length param_ctx.env + 1 in
                  List.fold_right
                    (fun _ acc ->
                      VPi { explicitness = Explicit; domain = VU;
                            effects = effect_row_closure (nominal_placeholder :: param_ctx.env) empty_effect_row;
                            codomain = { env = nominal_placeholder :: param_ctx.env; body = Nbe.quote param_ctx.metas depth acc } })
                    params VU
                in
                Ctx.define param_ctx name type_ty _type_val
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
            let kind = if public then Public else Private in
            let body_ctx =
              if num_params = 0 then Ctx.define param_ctx name VU nominal
              else
                let name_ty = Ctx.lookup recursive_param_ctx name |> snd in
                let name_val = Ctx.eval recursive_param_ctx (Var (Ctx.lookup recursive_param_ctx name |> fst)) in
                Ctx.define recursive_param_ctx name name_ty name_val
            in
            let ctor_values, ctor_types =
              List.split
                (List.map
                   (fun (cname, payload_clo_opt) ->
                     let ctor_value, ctor_ty =
                       build_ctor body_ctx.metas (nominal :: body_ctx.env) name cname num_params payload_clo_opt
                     in
                     ((cname, ctor_value), (cname, ctor_ty)))
                   elaborated_ctors)
            in
            let ctx_after_ctors =
              List.fold_left
                (fun ctx ((ctor_name, ctor_value), (_, ctor_ty)) ->
                  Ctx.define ctx ctor_name ctor_ty ctor_value)
                body_ctx (List.combine ctor_values ctor_types)
            in
            let nominal_ty =
              if num_params = 0 then VU
              else Ctx.lookup recursive_param_ctx name |> snd
            in
            let ctx = Ctx.define ctx_after_ctors name nominal_ty nominal in
            let type_entries =
              StructField (name, kind, nominal_ty) :: List.map (fun (c, ty) -> StructField (c, kind, ty)) ctor_types
            in
            go ctx
              (TypeBind (name, kind, nominal, ctor_values) :: acc_binds,
               List.rev_append type_entries acc_entries)
              rest
      in
      let core_bindings, extra_entries = go binding_ctx ([], []) bindings in
      let result_con_fields = List.map (fun (n, c, _) -> (n, c)) con_cores in
      let type_entries =
        List.map (fun (n, _, ty) -> StructField (n, Field, ty)) con_cores
        @ extra_entries
      in
      (Struct { con_fields = result_con_fields; bindings = core_bindings; partial = false },
       VStruct { entries = type_entries; partial = false })
  | Open (name, body) ->
      let ix, ty = Ctx.lookup ctx name in
      let value = Ctx.eval ctx (Var ix) in
      (match (Nbe.force ctx.metas ty, Nbe.force ctx.metas value) with
      | VModule _, VModule _ ->
          let body_core, body_ty = ops.infer (open_module_value ctx ty value) body in
          (Open (Var ix, body_core), body_ty)
      | _ -> raise (ElabError (UnboundVariable name (* not a module *))))
  | RecordTypeDef { name; params; fields; body } ->
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
              (Surface.Struct { con_fields = rewritten_fields; bindings = [] })
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
  | TypeDef { name; params; ctors; body } ->
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
      (* For parameterized types, build an Explicit VPi chain so Option I64 works.
         For nullary types, just bind with VU as before. *)
      let body_ctx =
        if num_params = 0 then
          Ctx.define param_ctx name VU nominal
        else begin
          (* Push VNominal first so NomRef evaluation can find it *)
          let body_ctx = { param_ctx with
            env = nominal :: param_ctx.env;
            types = VU :: param_ctx.types;
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
  | EffectDef { name; params; ops = eff_ops; body } ->
      let num_params = List.length params in
      let effect_id, eff, eff_ty, elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let body_ctx = Ctx.define ctx name eff_ty eff in
      let body_core, body_ty = ops.infer body_ctx body in
      (EffectDef { id = effect_id; name; num_params; ops = elaborated_ops; body = body_core },
       body_ty)
  | TraitDef { name; params; fields; body } ->
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let body_ctx = Ctx.add_trait (Ctx.define ctx name VU trait_ty) trait_info in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (U, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name }, body_core), body_ty)
  | ImplDef { trait_path = []; trait_name; args; fields; body } ->
      let body_ctx, _impl_effects, _evidence, impl_ty, impl_core = elaborate_impl ops ctx trait_name args fields in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (Ctx.quote ctx impl_ty, impl_core, body_core), body_ty)
  | ImplDef { trait_path = _ :: _; trait_name; _ } ->
      raise (ElabError (UnknownTrait trait_name))
  | Match (scrutinee, branches) ->
      let scrut_core, scrut_ty = ops.infer ctx scrutinee in
      let value_branches = surface_value_branches branches in
      let effect_branches = surface_effect_branches branches in
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
  | MacroDef _ | MacroCall _ | SyntaxOperatorUse _ ->
      failwith "macro-only syntax should not reach elaboration"
