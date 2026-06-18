open Core

type ops = {
  force : MetaContext.t -> value -> value;
  closure_apply : MetaContext.t -> closure -> value -> value;
  eval_effect_row_closure : MetaContext.t -> effect_row_closure -> value -> effect_row_value;
  eval : MetaContext.t -> env -> term -> value;
  apply : MetaContext.t -> value -> value -> value;
}

let lvl_to_ix (depth : lvl) (l : lvl) : ix = depth - l - 1

let rec conv_pat (p1 : core_pat) (p2 : core_pat) : bool =
  match (p1, p2) with
  | CPatWild, CPatWild -> true
  | CPatBind, CPatBind -> true
  | CPatAtom a1, CPatAtom a2 -> Atom.equal a1 a2
  | CPatProd ps1, CPatProd ps2 ->
      List.length ps1 = List.length ps2 && List.for_all2 conv_pat ps1 ps2
  | CPatOr (l1, r1), CPatOr (l2, r2) -> conv_pat l1 l2 && conv_pat r1 r2
  | ( CPatRecord { fields = fs1; partial = p1 },
      CPatRecord { fields = fs2; partial = p2 } ) ->
      p1 = p2
      && List.length fs1 = List.length fs2
      && List.for_all2
           (fun (n1, p1) (n2, p2) -> String.equal n1 n2 && conv_pat p1 p2)
           fs1 fs2
  | ( CPatStructType { fields = fs1; partial = p1 },
      CPatStructType { fields = fs2; partial = p2 } ) ->
      p1 = p2
      && List.length fs1 = List.length fs2
      && List.for_all2
           (fun (n1, p1) (n2, p2) -> String.equal n1 n2 && conv_pat p1 p2)
           fs1 fs2
  | CPatCon (n1, a1, ps1), CPatCon (n2, a2, ps2) ->
      String.equal n1 n2 && a1 = a2
      && List.length ps1 = List.length ps2
      && List.for_all2 conv_pat ps1 ps2
  | ( CPatNominalHead { id = id1; num_params = n1; param_pats = ps1; _ },
      CPatNominalHead { id = id2; num_params = n2; param_pats = ps2; _ } ) ->
      id1 = id2 && n1 = n2
      && List.length ps1 = List.length ps2
      && List.for_all2 conv_pat ps1 ps2
  | _ -> false

let rec quote ops (mc : MetaContext.t) (depth : lvl) (v : value) : term =
  match ops.force mc v with
  | VLam { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      Lam (quote ops mc (depth + 1) (ops.closure_apply mc clo var))
  | VPi { explicitness; domain; effects; codomain } ->
      let var = VRigid { lvl = depth; spine = [] } in
      let row_value = ops.eval_effect_row_closure mc effects var in
      let row =
        { effects = List.map (quote ops mc (depth + 1)) row_value.effect_values;
          tail = Option.map (quote ops mc (depth + 1)) row_value.tail_value }
      in
      Pi
        { explicitness;
          domain = quote ops mc depth domain;
          effects = row;
          codomain = quote ops mc (depth + 1) (ops.closure_apply mc codomain var) }
  | VU -> U
  | VEffectRowTy -> EffectRowTy
  | VEffectRow row ->
      EffectRowLit
        { effects = List.map (quote ops mc depth) row.effect_values;
          tail = Option.map (quote ops mc depth) row.tail_value }
  | VAtom a -> Atom a
  | VAtomTy t -> AtomTy t
  | VProd elems -> Prod (List.map (quote ops mc depth) elems)
  | VProdTy elems -> ProdTy (List.map (quote ops mc depth) elems)
  | VFix { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      Fix (quote ops mc (depth + 1) (ops.closure_apply mc clo var))
  | VModule { entries; partial = _ } ->
      let fields = module_entry_fields entries in
      let bindings =
        List.map
          (function
            | ModuleField (n, k, v) -> (
                match (k, ops.force mc v) with
                | Public, VEffect _ -> EffectBind (n, Public, v)
                | Private, VEffect _ -> EffectBind (n, Private, v)
                | Public, _ -> LetBind (n, Public, quote ops mc depth v)
                | Private, _ -> LetBind (n, Private, quote ops mc depth v)
                | Method, _ | PrivateMethod, _ | Field, _ ->
                    validate_module_fields fields;
                    failwith "unreachable")
            | ModuleImpl (kind, ty, value) ->
                ImplBind (kind, quote ops mc depth value, ty))
          entries
      in
      Module { bindings }
  | VStruct { entries; partial } ->
      let con_fields =
        List.filter_map
          (function StructField (n, Field, v) -> Some (n, quote ops mc depth v) | _ -> None)
          entries
      in
      let bindings =
        List.filter_map
          (function
            | StructField (n, k, v) -> (
                match (k, ops.force mc v) with
                | Public, VEffect _ -> Some (EffectBind (n, Public, v))
                | Private, VEffect _ -> Some (EffectBind (n, Private, v))
                | Public, _ -> Some (LetBind (n, Public, quote ops mc depth v))
                | Private, _ -> Some (LetBind (n, Private, quote ops mc depth v))
                | Method, _ -> Some (LetBind (n, Method, quote ops mc depth v))
                | PrivateMethod, _ -> Some (LetBind (n, PrivateMethod, quote ops mc depth v))
                | _ -> None)
            | StructImpl (kind, ty, value) -> Some (ImplBind (kind, quote ops mc depth value, ty)))
          entries
      in
      Struct { con_fields; bindings; partial }
  | VRecord { typ; fields } ->
      RecordConstruct
        { typ = quote ops mc depth typ;
          fields = List.map (fun (name, value) -> (name, quote ops mc depth value)) fields }
  | VNominal n ->
      if n.params = [] then Con n.name else NomRef (n.name, List.map (quote ops mc depth) n.params)
  | VEffect e -> EffectRef (e.name, List.map (quote ops mc depth) e.params)
  | VTrait t -> TraitRef { trait_id = t.trait_id; trait_name = t.trait_name }
  | VTraitDict d ->
      TraitDictTy
        { trait_id = d.trait_id;
          trait_name = d.trait_name;
          args = List.map (quote ops mc depth) d.args;
          fields = List.map (fun (name, value) -> (name, quote ops mc depth value)) d.fields }
  | VSelfType args -> SelfTypeRef (List.map (quote ops mc depth) args)
  | VRefTy a -> RefTy (quote ops mc depth a)
  | VStx (StxExpr stx) -> Stx stx
  | VStx _ -> raise (Nbe_error.EvalError "cannot quote non-expression syntax object")
  | VRef _ -> raise (Nbe_error.EvalError "cannot quote ref")
  | VPatternSyn _ -> raise (Nbe_error.EvalError "cannot quote pattern synonym")
  | VCon { name; spine; _ } -> quote_spine ops mc depth (Con name) spine
  | VCont _ -> raise (Nbe_error.EvalError "cannot quote continuation")
  | VNeutral { neutral = neu; _ } -> quote_neutral ops mc depth neu
  | VFlex { id; spine = sp } -> quote_spine ops mc depth (Meta id) sp
  | VRigid { lvl = l; spine = sp } -> quote_spine ops mc depth (Var (lvl_to_ix depth l)) sp

and quote_neutral ops (mc : MetaContext.t) (depth : lvl) (neu : neutral) : term =
  let head_term =
    match neu.head with
    | HVar l -> Var (lvl_to_ix depth l)
    | HMeta id -> Meta id
    | HPrim name -> Prim name
  in
  quote_frames ops mc depth head_term neu.frames

and quote_spine ops (mc : MetaContext.t) (depth : lvl) (head : term) (sp : spine) : term =
  List.fold_left (fun acc v -> Ap (acc, Explicit, quote ops mc depth v)) head sp

and quote_frames ops (mc : MetaContext.t) (depth : lvl) (head : term) (frames : frame list) : term =
  List.fold_left
    (fun acc frame ->
      match frame with
      | FApp v -> Ap (acc, Explicit, quote ops mc depth v)
      | FIf { then_; else_ } ->
          If
            ( acc,
              quote ops mc depth (ops.eval mc then_.env then_.body),
              quote ops mc depth (ops.eval mc else_.env else_.body) )
      | FProj i -> Proj (acc, i)
      | FDot name -> Dot (acc, name)
      | FRefGet -> RefGet acc
      | FRefSet value -> RefSet (acc, quote ops mc depth value)
      | FMatch branches ->
          Match
            ( acc,
              List.map
                (fun (p, clo) -> ValueBranch (p, quote ops mc depth (ops.eval mc clo.env clo.body)))
                branches ))
    head frames

let rec conv ops (mc : MetaContext.t) (depth : lvl) (v1 : value) (v2 : value) : bool =
  let v1 = ops.force mc v1 in
  let v2 = ops.force mc v2 in
  match (v1, v2) with
  | VU, VU -> true
  | VEffectRowTy, VEffectRowTy -> true
  | VEffectRow row1, VEffectRow row2 -> conv_effect_rows ops mc depth row1 row2
  | VAtom a1, VAtom a2 -> Atom.equal a1 a2
  | VAtomTy t1, VAtomTy t2 -> Atom_ty.equal t1 t2
  | ( VPi { explicitness = e1; domain = a1; effects = effs1; codomain = clo1 },
      VPi { explicitness = e2; domain = a2; effects = effs2; codomain = clo2 } )
    when e1 = e2 ->
      conv ops mc depth a1 a2
      &&
      let var = VRigid { lvl = depth; spine = [] } in
      conv_effect_rows ops mc (depth + 1)
        (ops.eval_effect_row_closure mc effs1 var)
        (ops.eval_effect_row_closure mc effs2 var)
      && conv ops mc (depth + 1) (ops.closure_apply mc clo1 var) (ops.closure_apply mc clo2 var)
  | VLam { body = clo1; _ }, VLam { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      conv ops mc (depth + 1) (ops.closure_apply mc clo1 var) (ops.closure_apply mc clo2 var)
  | VLam { body = clo; _ }, v | v, VLam { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      conv ops mc (depth + 1) (ops.closure_apply mc clo var) (ops.apply mc v var)
  | VProd elems1, VProd elems2 | VProdTy elems1, VProdTy elems2 ->
      List.length elems1 = List.length elems2 && List.for_all2 (conv ops mc depth) elems1 elems2
  | VRefTy a1, VRefTy a2 -> conv ops mc depth a1 a2
  | VRigid { lvl = l1; spine = sp1 }, VRigid { lvl = l2; spine = sp2 } ->
      l1 = l2 && conv_spine ops mc depth sp1 sp2
  | VFlex { id = id1; spine = sp1 }, VFlex { id = id2; spine = sp2 } ->
      id1 = id2 && conv_spine ops mc depth sp1 sp2
  | VNeutral { neutral = n1; _ }, VNeutral { neutral = n2; _ } -> conv_neutral ops mc depth n1 n2
  | VFix { body = clo1; _ }, VFix { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      conv ops mc (depth + 1) (ops.closure_apply mc clo1 var) (ops.closure_apply mc clo2 var)
  | ( VModule { entries = es1; partial = p1 }, VModule { entries = es2; partial = p2 } ) ->
      let fs1 = module_entry_fields es1 in
      let fs2 = module_entry_fields es2 in
      validate_module_fields fs1;
      validate_module_fields fs2;
      let visible fs = List.filter (fun (_, k, _) -> Nbe_support.visible_kind k) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      let conv_field (name, kind, ty) fields =
        match List.find_opt (fun (n, k, _) -> String.equal n name && k = kind) fields with
        | Some (_, _, other_ty) -> conv ops mc depth ty other_ty
        | None -> false
      in
      if (not p1) && not p2 then
        List.length vs1 = List.length vs2 && List.for_all (fun field -> conv_field field vs2) vs1
      else
        let required, available =
          if p1 && not p2 then (vs1, vs2)
          else if p2 && not p1 then (vs2, vs1)
          else if List.length vs1 <= List.length vs2 then (vs1, vs2)
          else (vs2, vs1)
        in
        List.for_all (fun field -> conv_field field available) required
  | ( VStruct { entries = es1; partial = p1 }, VStruct { entries = es2; partial = p2 } ) ->
      let fs1 = struct_entry_fields es1 in
      let fs2 = struct_entry_fields es2 in
      let visible fs = List.filter (fun (_, k, _) -> Nbe_support.visible_kind k) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      let conv_field (name, kind, ty) fields =
        match List.find_opt (fun (n, k, _) -> String.equal n name && k = kind) fields with
        | Some (_, _, other_ty) -> conv ops mc depth ty other_ty
        | None -> false
      in
      if (not p1) && not p2 then
        List.length vs1 = List.length vs2
        && List.for_all2
             (fun (n1, k1, v1) (n2, k2, v2) ->
               String.equal n1 n2 && k1 = k2 && conv ops mc depth v1 v2)
             vs1 vs2
      else
        let required, available =
          if p1 && not p2 then (vs1, vs2)
          else if p2 && not p1 then (vs2, vs1)
          else if List.length vs1 <= List.length vs2 then (vs1, vs2)
          else (vs2, vs1)
        in
        List.for_all (fun field -> conv_field field available) required
  | VRecord r1, VRecord r2 ->
      conv ops mc depth r1.typ r2.typ
      && List.length r1.fields = List.length r2.fields
      && List.for_all2
           (fun (n1, v1) (n2, v2) -> String.equal n1 n2 && conv ops mc depth v1 v2)
           r1.fields r2.fields
  | VNominal n1, VNominal n2 ->
      n1.id = n2.id
      && List.length n1.params = List.length n2.params
      && List.for_all2 (conv ops mc depth) n1.params n2.params
  | VEffect e1, VEffect e2 ->
      e1.id = e2.id
      && List.length e1.params = List.length e2.params
      && List.for_all2 (conv ops mc depth) e1.params e2.params
  | VTraitDict d1, VTraitDict d2 ->
      d1.trait_id = d2.trait_id
      && List.length d1.args = List.length d2.args
      && List.for_all2 (conv ops mc depth) d1.args d2.args
      && List.length d1.fields = List.length d2.fields
      && List.for_all2
           (fun (n1, v1) (n2, v2) -> String.equal n1 n2 && conv ops mc depth v1 v2)
           d1.fields d2.fields
  | VSelfType args1, VSelfType args2 ->
      List.length args1 = List.length args2 && List.for_all2 (conv ops mc depth) args1 args2
  | VCon c1, VCon c2 ->
      String.equal c1.name c2.name
      && List.length c1.spine = List.length c2.spine
      && List.for_all2 (conv ops mc depth) c1.spine c2.spine
  | _ -> false

and conv_spine ops (mc : MetaContext.t) (depth : lvl) (sp1 : spine) (sp2 : spine) : bool =
  List.length sp1 = List.length sp2 && List.for_all2 (conv ops mc depth) sp1 sp2

and conv_effect_rows ops (mc : MetaContext.t) (depth : lvl) (row1 : effect_row_value)
    (row2 : effect_row_value) : bool =
  let rec remove_match eff = function
    | [] -> None
    | candidate :: rest when conv ops mc depth eff candidate -> Some rest
    | candidate :: rest -> Option.map (fun rest -> candidate :: rest) (remove_match eff rest)
  in
  List.length row1.effect_values = List.length row2.effect_values
  && Option.is_some
       (List.fold_left
          (fun remaining eff -> Option.bind remaining (remove_match eff))
          (Some row2.effect_values) row1.effect_values)
  && match (row1.tail_value, row2.tail_value) with
     | None, None -> true
     | Some (VFlex { spine = []; _ }), None | None, Some (VFlex { spine = []; _ }) -> true
     | Some (VFlex { spine = []; _ }), Some (VFlex { spine = []; _ }) -> true
     | Some lhs, Some rhs -> conv ops mc depth lhs rhs
     | _ -> false

and conv_neutral ops (mc : MetaContext.t) (depth : lvl) (n1 : neutral) (n2 : neutral) : bool =
  let heads_eq =
    match (n1.head, n2.head) with
    | HVar l1, HVar l2 -> l1 = l2
    | HMeta id1, HMeta id2 -> id1 = id2
    | HPrim n1, HPrim n2 -> String.equal n1 n2
    | _ -> false
  in
  heads_eq && conv_frames ops mc depth n1.frames n2.frames

and conv_frames ops (mc : MetaContext.t) (depth : lvl) (fs1 : frame list) (fs2 : frame list) : bool =
  match (fs1, fs2) with
  | [], [] -> true
  | FApp v1 :: rest1, FApp v2 :: rest2 ->
      conv ops mc depth v1 v2 && conv_frames ops mc depth rest1 rest2
  | ( FIf { then_ = t1; else_ = e1 } :: rest1,
      FIf { then_ = t2; else_ = e2 } :: rest2 ) ->
      conv ops mc depth (ops.eval mc t1.env t1.body) (ops.eval mc t2.env t2.body)
      && conv ops mc depth (ops.eval mc e1.env e1.body) (ops.eval mc e2.env e2.body)
      && conv_frames ops mc depth rest1 rest2
  | FProj i1 :: rest1, FProj i2 :: rest2 -> i1 = i2 && conv_frames ops mc depth rest1 rest2
  | FDot n1 :: rest1, FDot n2 :: rest2 ->
      String.equal n1 n2 && conv_frames ops mc depth rest1 rest2
  | FRefGet :: rest1, FRefGet :: rest2 -> conv_frames ops mc depth rest1 rest2
  | FRefSet v1 :: rest1, FRefSet v2 :: rest2 ->
      conv ops mc depth v1 v2 && conv_frames ops mc depth rest1 rest2
  | FMatch bs1 :: rest1, FMatch bs2 :: rest2 ->
      List.length bs1 = List.length bs2
      && List.for_all2
           (fun (p1, c1) (p2, c2) ->
             conv_pat p1 p2
             && conv ops mc depth (ops.eval mc c1.env c1.body) (ops.eval mc c2.env c2.body))
           bs1 bs2
      && conv_frames ops mc depth rest1 rest2
  | _ -> false
