open Core
include Elab_error
open Elab_common

module Ctx = Elab_ctx.Ctx

open Elab_resolve

let term_mentions_var target term =
  let rec go target = function
    | Var ix -> ix = target
    | Lam body | Fix body -> go (target + 1) body
    | Ap (f, _, a) -> go target f || go target a
    | Let (ty, def, body) -> go target ty || go target def || go (target + 1) body
    | Pi { domain; effects; codomain; _ } ->
        go target domain
        || List.exists (go (target + 1)) effects.effects
        || (match effects.tail with Some tail -> go (target + 1) tail | None -> false)
        || go (target + 1) codomain
    | If (cond, then_, else_) -> go target cond || go target then_ || go target else_
    | Prod elems | ProdTy elems -> List.exists (go target) elems
    | EffectRowTy -> false
    | EffectRowLit row ->
        List.exists (go target) row.effects
        || Option.fold ~none:false ~some:(go target) row.tail
    | RefTy a | RefNew a | RefGet a -> go target a
    | RefSet (r, e) -> go target r || go target e
    | Proj (e, _) | Dot (e, _) | Open (e, _) -> go target e
    | Module { bindings } ->
        List.exists
          (function
            | LetBind (_, _, value) -> go target value
            | ImplBind (_, value, _) -> go target value
            | TypeBind _ | EffectBind _ -> false)
          bindings
    | Struct { con_fields; bindings; _ } ->
        List.exists (fun (_, ty) -> go target ty) con_fields
        || List.exists
             (function
               | LetBind (_, _, value) -> go target value
               | ImplBind (_, value, _) -> go target value
               | TypeBind _ | EffectBind _ -> false)
             bindings
    | RecordConstruct { typ; fields } ->
        go target typ || List.exists (fun (_, value) -> go target value) fields
    | NomRef (_, params) | EffectRef (_, params) -> List.exists (go target) params
    | TraitRef _ -> false
    | TraitDictTy { args; fields; _ } ->
        List.exists (go target) args || List.exists (fun (_, value) -> go target value) fields
    | SelfTypeRef args -> List.exists (go target) args
    | Ctor { spine; nominal_spine; _ } ->
        List.exists (go target) spine || List.exists (go target) nominal_spine
    | Match (scrut, branches) ->
        go target scrut
        || List.exists
             (function
               | ValueBranch (_, body) -> go target body
               | EffectBranch { body; _ } -> go target body)
             branches
    | NominalDef { ctors; body; _ } ->
        List.exists (fun (_, payload) -> match payload with Some payload -> go target payload | None -> false) ctors || go target body
    | EffectDef { ops; body; _ } ->
        List.exists (fun (_, input, output) -> go target input || go target output) ops || go target body
    | Perform { eff; arg; _ } -> go target eff || go target arg
    | Stx _ -> false
    | Atom _ | AtomTy _ | U | Prim _ | Meta _ | InsertedMeta _ | Con _ -> false
  in
  go target term

let rec subst_value_var (mc : MetaContext.t) (target : lvl) (replacement : value) (v : value) : value =
  match Nbe.force mc v with
  | VRigid { lvl; spine } when lvl = target ->
      List.fold_left (Nbe.apply mc) replacement spine
  | VPi { explicitness; domain; effects; codomain } ->
      let domain = subst_value_var mc target replacement domain in
      let effects = subst_effect_row_closure_var mc target replacement effects in
      VPi { explicitness; domain; effects; codomain = subst_closure_var mc target replacement codomain }
  | VProd elems -> VProd (List.map (subst_value_var mc target replacement) elems)
  | VProdTy elems -> VProdTy (List.map (subst_value_var mc target replacement) elems)
  | VEffectRow row ->
      VEffectRow
        { effect_values = List.map (subst_value_var mc target replacement) row.effect_values;
          tail_value = Option.map (subst_value_var mc target replacement) row.tail_value }
  | VModule { entries; partial } ->
      let entries =
        List.map
          (function
            | ModuleField (name, kind, value) -> ModuleField (name, kind, subst_value_var mc target replacement value)
            | ModuleImpl (kind, ty, value) ->
                ModuleImpl (kind, subst_value_var mc target replacement ty, subst_value_var mc target replacement value))
          entries
      in
      VModule { entries; partial }
  | VStruct { entries; partial } ->
      let entries =
        List.map
          (function
            | StructField (name, kind, value) -> StructField (name, kind, subst_value_var mc target replacement value)
            | StructImpl (kind, ty, value) ->
                StructImpl (kind, subst_value_var mc target replacement ty, subst_value_var mc target replacement value))
          entries
      in
      VStruct { entries; partial }
  | VRecord { typ; fields } ->
      VRecord { typ = subst_value_var mc target replacement typ; fields = List.map (fun (name, value) -> (name, subst_value_var mc target replacement value)) fields }
  | VNominal n -> VNominal { n with params = List.map (subst_value_var mc target replacement) n.params }
  | VEffect e -> VEffect { e with params = List.map (subst_value_var mc target replacement) e.params }
  | VTrait _ as v -> v
  | VTraitDict d ->
      VTraitDict
        { d with
          args = List.map (subst_value_var mc target replacement) d.args;
          fields = List.map (fun (name, value) -> (name, subst_value_var mc target replacement value)) d.fields }
  | VSelfType args -> VSelfType (List.map (subst_value_var mc target replacement) args)
  | VRefTy a -> VRefTy (subst_value_var mc target replacement a)
  | VRef _ as v -> v
  | VCon c -> VCon { c with spine = List.map (subst_value_var mc target replacement) c.spine; nominal = subst_value_var mc target replacement c.nominal }
  | VNeutral { ty; neutral } ->
      VNeutral { ty = subst_value_var mc target replacement ty; neutral = subst_neutral_var mc target replacement neutral }
  | VFlex { id; spine } -> VFlex { id; spine = List.map (subst_value_var mc target replacement) spine }
  | VRigid { lvl; spine } -> VRigid { lvl; spine = List.map (subst_value_var mc target replacement) spine }
  | VLam _ | VFix _ | VCont _ | VStx _ as v -> v
  | VU | VEffectRowTy | VAtom _ | VAtomTy _ as v -> v

and subst_closure_var mc target replacement clo =
  { clo with env = List.map (subst_value_var mc target replacement) clo.env }

and subst_effect_row_closure_var mc target replacement row =
  { row with env = List.map (subst_value_var mc target replacement) row.env }

and subst_neutral_var mc target replacement neutral =
  let frames =
    List.map
      (function
        | FApp value -> FApp (subst_value_var mc target replacement value)
        | FIf { then_; else_ } -> FIf { then_ = subst_closure_var mc target replacement then_; else_ = subst_closure_var mc target replacement else_ }
        | FProj _ as frame -> frame
        | FDot _ as frame -> frame
        | FRefGet as frame -> frame
        | FRefSet value -> FRefSet (subst_value_var mc target replacement value)
        | FMatch branches -> FMatch (List.map (fun (pat, clo) -> (pat, subst_closure_var mc target replacement clo)) branches))
      neutral.frames
  in
  { neutral with frames }

let rec branch_type_refinement = function
  | Surface.PatType atom_ty -> Some (VAtomTy atom_ty)
  | Surface.PatOr (lhs, rhs) -> (
      match branch_type_refinement lhs with Some _ as found -> found | None -> branch_type_refinement rhs)
  | _ -> None

let refinement_target_of_scrutinee ctx scrut_core =
  match scrut_core with
  | Var ix -> Some (ctx.Ctx.lvl - ix - 1)
  | _ -> None

let refine_context_type_var ctx target replacement =
  let substitute = subst_value_var ctx.Ctx.metas target replacement in
  {
    ctx with
    Ctx.types = List.map substitute ctx.Ctx.types;
    name_table = NameMap.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.name_table;
    self_entry = Option.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.self_entry;
    resume_entry = Option.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.resume_entry;
  }

let close_recursive_payload_term nominal_name num_params =
  let rec collect_apps acc = function
    | Ap (f, Explicit, a) -> collect_apps (a :: acc) f
    | f -> (f, acc)
  in
  let rec go cutoff term =
    match collect_apps [] term with
    | Var ix, args when ix = cutoff && List.length args = num_params ->
        NomRef (nominal_name, List.map (go cutoff) args)
    | _ -> (
        match term with
        | Var ix when ix = cutoff ->
            NomRef (nominal_name, List.init num_params (fun i -> Var (num_params - 1 - i)))
        | Var ix when ix > cutoff -> Var (ix - 1)
        | Var ix -> Var ix
        | Lam body -> Lam (go (cutoff + 1) body)
        | Ap (f, expl, a) -> Ap (go cutoff f, expl, go cutoff a)
        | Let (ty, def, body) -> Let (go cutoff ty, go cutoff def, go (cutoff + 1) body)
        | Pi { explicitness; domain; effects; codomain } ->
            Pi
              { explicitness;
                domain = go cutoff domain;
                effects =
                  { effects = List.map (go (cutoff + 1)) effects.effects;
                    tail = Option.map (go (cutoff + 1)) effects.tail };
                codomain = go (cutoff + 1) codomain }
        | If (cond, then_, else_) -> If (go cutoff cond, go cutoff then_, go cutoff else_)
        | Prod elems -> Prod (List.map (go cutoff) elems)
        | ProdTy elems -> ProdTy (List.map (go cutoff) elems)
        | EffectRowTy -> EffectRowTy
        | EffectRowLit row ->
            EffectRowLit
              { effects = List.map (go cutoff) row.effects;
                tail = Option.map (go cutoff) row.tail }
        | RefTy a -> RefTy (go cutoff a)
        | RefNew e -> RefNew (go cutoff e)
        | RefGet e -> RefGet (go cutoff e)
        | RefSet (r, e) -> RefSet (go cutoff r, go cutoff e)
        | Proj (e, i) -> Proj (go cutoff e, i)
        | Dot (e, field) -> Dot (go cutoff e, field)
        | Module { bindings } ->
            let binding = function
              | LetBind (field, kind, value) -> LetBind (field, kind, go cutoff value)
              | TypeBind (field, kind, nominal, ctors) -> TypeBind (field, kind, nominal, ctors)
              | EffectBind (field, kind, eff) -> EffectBind (field, kind, eff)
              | ImplBind (kind, value, ty) -> ImplBind (kind, go cutoff value, ty)
            in
            Module { bindings = List.map binding bindings }
        | Struct { con_fields; bindings; partial } ->
            let con_fields = List.map (fun (field, ty) -> (field, go cutoff ty)) con_fields in
            let binding = function
              | LetBind (field, kind, value) -> LetBind (field, kind, go cutoff value)
              | TypeBind (field, kind, nominal, ctors) -> TypeBind (field, kind, nominal, ctors)
              | EffectBind (field, kind, eff) -> EffectBind (field, kind, eff)
              | ImplBind (kind, value, ty) -> ImplBind (kind, go cutoff value, ty)
            in
            Struct { con_fields; bindings = List.map binding bindings; partial }
        | RecordConstruct { typ; fields } ->
            RecordConstruct { typ = go cutoff typ; fields = List.map (fun (field, value) -> (field, go cutoff value)) fields }
        | Open (s, body) -> Open (go cutoff s, go cutoff body)
        | Fix body -> Fix (go (cutoff + 1) body)
        | NomRef (name, params) -> NomRef (name, List.map (go cutoff) params)
        | EffectRef (name, params) -> EffectRef (name, List.map (go cutoff) params)
        | TraitRef _ as term -> term
        | TraitDictTy { trait_id; trait_name; args; fields } ->
            TraitDictTy
              { trait_id;
                trait_name;
                args = List.map (go cutoff) args;
                fields = List.map (fun (name, value) -> (name, go cutoff value)) fields }
        | SelfTypeRef args -> SelfTypeRef (List.map (go cutoff) args)
        | Ctor { name; spine; nominal_name; nominal_spine } ->
            Ctor { name; spine = List.map (go cutoff) spine; nominal_name; nominal_spine = List.map (go cutoff) nominal_spine }
        | Match (scrut, branches) ->
            let go_branch = function
              | ValueBranch (pat, body) -> ValueBranch (pat, go cutoff body)
              | EffectBranch { eff; op; arg_pat; body } ->
                  EffectBranch { eff; op; arg_pat; body = go cutoff body }
            in
            Match (go cutoff scrut, List.map go_branch branches)
        | NominalDef { id; name; num_params; ctors; body } ->
            NominalDef
              { id; name; num_params;
                ctors = List.map (fun (ctor, payload) -> (ctor, Option.map (go cutoff) payload)) ctors;
                body = go cutoff body }
        | EffectDef { id; name; num_params; ops; body } ->
            EffectDef
              { id; name; num_params;
                ops = List.map (fun (op, input, output) -> (op, go cutoff input, go cutoff output)) ops;
                body = go cutoff body }
        | Perform { eff; op; arg } -> Perform { eff = go cutoff eff; op; arg = go cutoff arg }
        | Atom _ | AtomTy _ | U | Prim _ | Meta _ | InsertedMeta _ | Con _ | Stx _ as term -> term)
  in
  go 0

let rec refinement_for_nominal_head ctx = function
  | Surface.PatCon (path, name, _) -> (
      match find_nominal_template_opt ctx path name with
      | Some (VNominal n) -> Some (VNominal { n with params = List.init n.num_params (fun _ -> Ctx.raw_meta ctx) })
      | Some _ | None -> None)
  | Surface.PatOr (lhs, rhs) -> (
      match refinement_for_nominal_head ctx lhs with
      | Some _ as found -> found
      | None -> refinement_for_nominal_head ctx rhs)
  | _ -> None

let refine_branch_context ctx refinement_target pat =
  match (branch_type_refinement pat, refinement_target) with
  | Some replacement, Some target -> refine_context_type_var ctx target replacement
  | None, Some target -> (
      match refinement_for_nominal_head ctx pat with
      | Some replacement -> refine_context_type_var ctx target replacement
      | None -> ctx)
  | _ -> ctx

let refine_branch_expected ctx refinement_target pat expected =
  match (branch_type_refinement pat, refinement_target) with
  | Some replacement, Some target -> subst_value_var ctx.Ctx.metas target replacement expected
  | None, Some target -> (
      match refinement_for_nominal_head ctx pat with
      | Some replacement -> subst_value_var ctx.Ctx.metas target replacement expected
      | None -> expected)
  | _ -> expected
