open Core

exception EvalError of string

let rec closure_apply (mc : MetaContext.t) (c : closure) (v : value) : value =
  eval mc (v :: c.env) c.body

and eval (mc : MetaContext.t) (env : env) (t : term) : value =
  match t with
  | Var ix -> List.nth env ix
  | Lam body -> VLam { body = { env; body } }
  | Ap (f, a) ->
      let vf = eval mc env f in
      let va = eval mc env a in
      apply mc vf va
  | Let (_, def, body) ->
      let vdef = eval mc env def in
      eval mc (vdef :: env) body
  | Pi (a, b) ->
      VPi { domain = eval mc env a; codomain = { env; body = b } }
  | U -> VU
  | Atom a -> VAtom a
  | AtomTy t -> VAtomTy t
  | If (cond, then_, else_) ->
      let vc = eval mc env cond in
      eval_if mc env vc then_ else_
  | Prod elems -> VProd (List.map (eval mc env) elems)
  | ProdTy elems -> VProdTy (List.map (eval mc env) elems)
  | Struct { con_fields; bindings; partial } ->
      (* con_fields: all at same scope, no sequential dependency *)
      let con_vals =
        List.map (fun (name, ty) -> (name, Field, eval mc env ty)) con_fields
      in
      let env = List.fold_left (fun e (_, _, v) -> v :: e) env con_vals in
      (* bindings: sequential, TypeBind stores values directly *)
      let rec eval_binds env acc = function
        | [] -> env, List.rev acc
        | LetBind (name, kind, def) :: rest ->
            let vdef = eval mc env def in
            eval_binds (vdef :: env) ((name, kind, vdef) :: acc) rest
        | TypeBind (name, kind, nominal, ctors) :: rest ->
            let ctor_vals = List.map (fun (n, v) -> (n, kind, v)) ctors in
            let env = List.fold_left (fun e (_, _, v) -> v :: e) env ctor_vals in
            let env = nominal :: env in
            eval_binds env ((name, kind, nominal) :: ctor_vals @ acc) rest
      in
      let _env, bind_vals = eval_binds env [] bindings in
      VStruct { fields = con_vals @ bind_vals; partial }
  | Proj (e, i) ->
      let vs = eval mc env e in
      (match vs with
      | VProd elems -> List.nth elems i
      | VNeutral { neutral; _ } ->
          VNeutral
            { ty = VU;
              neutral = { neutral with frames = neutral.frames @ [ FProj i ] } }
      | VFlex { id; spine = sp } ->
          let frames = List.map (fun v -> FApp v) sp in
          VNeutral { ty = VU; neutral = { head = HMeta id; frames = frames @ [ FProj i ] } }
      | VRigid { lvl; spine = sp } ->
          let frames = List.map (fun v -> FApp v) sp in
          VNeutral { ty = VU; neutral = { head = HVar lvl; frames = frames @ [ FProj i ] } }
      | _ -> raise (EvalError "projection of non-product"))
  | Dot (e, name) ->
      let vs = eval mc env e in
      (match vs with
      | VStruct { fields; _ } ->
          (* Dot: all non-Private fields accessible *)
          (match List.find_opt (fun (n, k, _) -> String.equal n name && k <> Private) fields with
          | Some (_, _, v) -> v
          | None -> raise (EvalError "field not found"))
      | VNeutral { neutral; _ } ->
          VNeutral
            { ty = VU;
              neutral = { neutral with frames = neutral.frames @ [ FDot name ] } }
      | VFlex { id; spine = sp } ->
          let frames = List.map (fun v -> FApp v) sp in
          VNeutral
            { ty = VU; neutral = { head = HMeta id; frames = frames @ [ FDot name ] } }
      | VRigid { lvl; spine = sp } ->
          let frames = List.map (fun v -> FApp v) sp in
          VNeutral
            { ty = VU; neutral = { head = HVar lvl; frames = frames @ [ FDot name ] } }
      | _ -> raise (EvalError "field access on non-struct"))
  | Open (s, body) ->
      let vs = eval mc env s in
      (match vs with
      | VStruct { fields; _ } ->
          (* Open: only Public (pub-let) fields *)
          let vals = List.filter_map (fun (_, k, v) -> if k = Public then Some v else None) fields in
          let env' = List.fold_left (fun e v -> v :: e) env vals in
          eval mc env' body
      | _ -> raise (EvalError "open of non-struct"))
  | Fix body -> VFix { body = { env; body } }
  | Con name -> eval_con env name
  | NomRef (name, params) ->
      (match eval_con env name with
      | VNominal n ->
          let param_vals = List.map (eval mc env) params in
          VNominal { n with params = param_vals }
      | _ -> raise (EvalError ("NomRef is not VNominal: " ^ name)))
  | Ctor { name; spine; nominal_name; nominal_spine } ->
      let spine_vals = List.map (eval mc env) spine in
      let nom_spine_vals = List.map (eval mc env) nominal_spine in
      (match eval_con env nominal_name with
      | VNominal n ->
          VCon
            { name;
              spine = spine_vals;
              nominal = VNominal { n with params = nom_spine_vals } }
      | _ -> raise (EvalError ("Ctor nominal is not VNominal: " ^ nominal_name)))
  | Prim name ->
      VNeutral { ty = VU; neutral = { head = HPrim name; frames = [] } }
  | Meta id -> eval_meta mc id
  | InsertedMeta (id, bds) -> eval_inserted_meta mc env id bds
  | Match (scrut, branches) ->
      let vs = eval mc env scrut in
      eval_match mc env vs branches

and apply (mc : MetaContext.t) (vf : value) (va : value) : value =
  match vf with
  | VLam { body = clo; _ } -> eval mc (va :: clo.env) clo.body
  | VFix { body = clo; _ } ->
      let rec fix_val =
        lazy (eval mc (Lazy.force fix_val :: clo.env) clo.body)
      in
      apply mc (Lazy.force fix_val) va
  | VNeutral { ty; neutral = neu } ->
      let cod = apply_ty mc ty va in
      VNeutral
        {
          ty = cod;
          neutral = { head = neu.head; frames = neu.frames @ [ FApp va ] };
        }
  | VFlex { id; spine = sp } -> VFlex { id; spine = sp @ [ va ] }
  | VRigid { lvl; spine = sp } -> VRigid { lvl; spine = sp @ [ va ] }
  | _ -> raise (EvalError "applying non-function")

and apply_ty (mc : MetaContext.t) (ty : value) (va : value) : value =
  match ty with
  | VPi { codomain = clo; _ } -> eval mc (va :: clo.env) clo.body
  | _ -> VU

and eval_if (mc : MetaContext.t) (env : env) (vc : value) (then_ : term)
    (else_ : term) : value =
  match vc with
  | VAtom (Bool true) -> eval mc env then_
  | VAtom (Bool false) -> eval mc env else_
  | _ ->
      let head, base_frames = stuck_head_frames vc in
      let if_frame =
        FIf { then_ = { env; body = then_ }; else_ = { env; body = else_ } }
      in
      VNeutral
        { ty = VU; neutral = { head; frames = base_frames @ [ if_frame ] } }

(* Extract the head and existing frames from a stuck value *)
and stuck_head_frames (v : value) : head * frame list =
  match v with
  | VNeutral { neutral = neu; _ } -> (neu.head, neu.frames)
  | VFlex { id; spine = sp } ->
      let frames = List.map (fun v -> FApp v) sp in
      (HMeta id, frames)
  | VRigid { lvl = l; spine = sp } ->
      let frames = List.map (fun v -> FApp v) sp in
      (HVar l, frames)
  | _ -> raise (EvalError "if condition is not a boolean or stuck term")

and eval_meta (mc : MetaContext.t) (id : meta_id) : value =
  match MetaContext.lookup mc id with
  | Solved v -> v
  | Unsolved -> VFlex { id; spine = [] }

(* Apply an InsertedMeta to the bound variables in scope, skipping defined ones *)
and eval_inserted_meta (mc : MetaContext.t) (env : env) (id : meta_id)
    (bds : bd list) : value =
  let base = eval_meta mc id in
  let rec go v e bds =
    match (e, bds) with
    | [], [] -> v
    | val_ :: env_rest, bd :: bds_rest -> (
        match bd with
        | Bound -> go (apply mc v val_) env_rest bds_rest
        | Defined -> go v env_rest bds_rest)
    | _ -> raise (EvalError "bd mask length mismatch")
  in
  go base (List.rev env) (List.rev bds)

(* Scan the environment for a VCon or VNominal with the given name.
   Head = most-recently-bound, so first match wins (correct shadowing). *)
and eval_con (env : env) (name : string) : value =
  let rec go = function
    | [] -> raise (EvalError ("unbound constructor/type: " ^ name))
    | VCon c :: _ when String.equal c.name name -> VCon c
    | VNominal n :: _ when String.equal n.name name -> VNominal n
    | _ :: rest -> go rest
  in
  go env

(* Pattern matching: dispatch on the scrutinee value. For a VCon, find the
   matching branch by constructor name and bind payload elements via sub-patterns.
   For a stuck scrutinee, accumulate an FMatch frame. *)
and eval_match (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : value =
  let scrutinee = force mc scrutinee in
  match scrutinee with
  | VCon { name; spine; nominal = _ } ->
      let rec try_branch = function
        | [] -> raise (EvalError ("no matching branch for constructor: " ^ name))
        | (CPatCon (cname, num_type_params, sub_pats), body) :: rest ->
            if String.equal cname name then begin
              let payload = List.drop num_type_params spine in
              let env' = bind_pats mc env sub_pats payload in
              eval mc env' body
            end else
              try_branch rest
        | (CPatWild, body) :: _ -> eval mc env body
        | (CPatBind, body) :: _ -> eval mc (scrutinee :: env) body
      in
      try_branch branches
  | _ ->
      let head, base_frames = stuck_head_frames scrutinee in
      VNeutral
        { ty = VU;
          neutral = { head; frames = base_frames @ [ FMatch
              (List.map (fun (p, body) -> (p, { env; body })) branches) ] } }

(* Bind pattern variables: match sub-patterns against payload values.
   Recursively handles nested constructor patterns. *)
and bind_pats (mc : MetaContext.t) (env : env) (pats : core_pat list)
    (vals : value list) : env =
  match (pats, vals) with
  | [], [] -> env
  | CPatWild :: ps, _ :: vs -> bind_pats mc env ps vs
  | CPatBind :: ps, v :: vs -> bind_pats mc (v :: env) ps vs
  | CPatCon (name, num_type_params, sub_pats) :: ps, v :: vs ->
      (match force mc v with
      | VCon { name = cname; spine; _ } when String.equal cname name ->
          let payload = List.drop num_type_params spine in
          let env = bind_pats mc env sub_pats payload in
          bind_pats mc env ps vs
      | _ -> raise (EvalError "nested constructor pattern mismatch"))
  | _ -> raise (EvalError "pattern/payload arity mismatch")

and conv_pat (p1 : core_pat) (p2 : core_pat) : bool =
  match (p1, p2) with
  | CPatWild, CPatWild -> true
  | CPatBind, CPatBind -> true
  | CPatCon (n1, a1, ps1), CPatCon (n2, a2, ps2) ->
      String.equal n1 n2 && a1 = a2
      && List.length ps1 = List.length ps2
      && List.for_all2 conv_pat ps1 ps2
  | _ -> false

and force (mc : MetaContext.t) (v : value) : value =
  match v with
  | VFlex { id; spine = sp } -> (
      match MetaContext.lookup mc id with
      | Solved v ->
          let applied = List.fold_left (fun f a -> apply mc f a) v sp in
          force mc applied
      | Unsolved -> VFlex { id; spine = sp })
  | _ -> v

let lvl_to_ix (depth : lvl) (l : lvl) : ix = depth - l - 1

(* Convert a value / stucked term back to AST normal form *)
let rec quote (mc : MetaContext.t) (depth : lvl) (v : value) : term =
  match force mc v with
  | VLam { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      Lam (quote mc (depth + 1) (closure_apply mc clo var))
  | VPi { domain = a; codomain = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      Pi (quote mc depth a, quote mc (depth + 1) (closure_apply mc clo var))
  | VU -> U
  | VAtom a -> Atom a
  | VAtomTy t -> AtomTy t
  | VProd elems -> Prod (List.map (quote mc depth) elems)
  | VProdTy elems -> ProdTy (List.map (quote mc depth) elems)
  | VFix { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      Fix (quote mc (depth + 1) (closure_apply mc clo var))
  | VStruct { fields; partial } ->
      let con_fields =
        List.filter_map (fun (n, k, v) ->
          if k = Field then Some (n, quote mc depth v) else None) fields
      in
      let bindings =
        List.filter_map (fun (n, k, v) ->
          match k with
          | Public -> Some (LetBind (n, Public, quote mc depth v))
          | Private -> Some (LetBind (n, Private, quote mc depth v))
          | _ -> None) fields
      in
      Struct { con_fields; bindings; partial }
  | VNominal n -> Con n.name
  | VCon { name; spine; _ } ->
      quote_spine mc depth (Con name) spine
  | VNeutral { neutral = neu; _ } -> quote_neutral mc depth neu
  | VFlex { id; spine = sp } -> quote_spine mc depth (Meta id) sp
  | VRigid { lvl = l; spine = sp } ->
      quote_spine mc depth (Var (lvl_to_ix depth l)) sp

and quote_neutral (mc : MetaContext.t) (depth : lvl) (neu : neutral) : term =
  let head_term =
    match neu.head with
    | HVar l -> Var (lvl_to_ix depth l)
    | HMeta id -> Meta id
    | HPrim name -> Prim name
  in
  quote_frames mc depth head_term neu.frames

and quote_spine (mc : MetaContext.t) (depth : lvl) (head : term) (sp : spine) :
    term =
  List.fold_left (fun acc v -> Ap (acc, quote mc depth v)) head sp

and quote_frames (mc : MetaContext.t) (depth : lvl) (head : term)
    (frames : frame list) : term =
  List.fold_left
    (fun acc frame ->
      match frame with
      | FApp v -> Ap (acc, quote mc depth v)
      | FIf { then_; else_ } ->
          If
            ( acc,
              quote mc depth (eval mc then_.env then_.body),
              quote mc depth (eval mc else_.env else_.body) )
      | FProj i -> Proj (acc, i)
      | FDot name -> Dot (acc, name)
      | FMatch branches ->
          Match
            ( acc,
              List.map
                (fun (p, clo) -> (p, quote mc depth (eval mc clo.env clo.body)))
                branches ))
    head frames

let rec conv (mc : MetaContext.t) (depth : lvl) (v1 : value) (v2 : value) : bool
    =
  let v1 = force mc v1 in
  let v2 = force mc v2 in
  match (v1, v2) with
  | VU, VU -> true
  | VAtom a1, VAtom a2 -> Syntax.Ast.Atom.equal a1 a2
  | VAtomTy t1, VAtomTy t2 -> equal_atom_ty t1 t2
  | ( VPi { domain = a1; codomain = clo1; _ },
      VPi { domain = a2; codomain = clo2; _ } ) ->
      conv mc depth a1 a2
      &&
      let var = VRigid { lvl = depth; spine = [] } in
      conv mc (depth + 1)
        (closure_apply mc clo1 var)
        (closure_apply mc clo2 var)
  | VLam { body = clo1; _ }, VLam { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      conv mc (depth + 1)
        (closure_apply mc clo1 var)
        (closure_apply mc clo2 var)
  | VLam { body = clo; _ }, v | v, VLam { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      conv mc (depth + 1) (closure_apply mc clo var) (apply mc v var)
  | VProd elems1, VProd elems2 | VProdTy elems1, VProdTy elems2 ->
      List.length elems1 = List.length elems2
      && List.for_all2 (conv mc depth) elems1 elems2
  | VRigid { lvl = l1; spine = sp1 }, VRigid { lvl = l2; spine = sp2 } ->
      l1 = l2 && conv_spine mc depth sp1 sp2
  | VFlex { id = id1; spine = sp1 }, VFlex { id = id2; spine = sp2 } ->
      id1 = id2 && conv_spine mc depth sp1 sp2
  | VNeutral { neutral = n1; _ }, VNeutral { neutral = n2; _ } ->
      conv_neutral mc depth n1 n2
  | VFix { body = clo1; _ }, VFix { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      conv mc (depth + 1)
        (closure_apply mc clo1 var)
        (closure_apply mc clo2 var)
  | VStruct { fields = fs1; _ }, VStruct { fields = fs2; _ } ->
      let visible fs = List.filter (fun (_, k, _) -> k <> Private) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      List.length vs1 = List.length vs2
      && List.for_all2
           (fun (n1, k1, v1) (n2, k2, v2) ->
             String.equal n1 n2 && k1 = k2 && conv mc depth v1 v2)
           vs1 vs2
  | VNominal n1, VNominal n2 ->
      n1.id = n2.id
      && List.length n1.params = List.length n2.params
      && List.for_all2 (conv mc depth) n1.params n2.params
  | VCon c1, VCon c2 ->
      String.equal c1.name c2.name
      && List.length c1.spine = List.length c2.spine
      && List.for_all2 (conv mc depth) c1.spine c2.spine
  | _ -> false

and conv_spine (mc : MetaContext.t) (depth : lvl) (sp1 : spine) (sp2 : spine) :
    bool =
  List.length sp1 = List.length sp2 && List.for_all2 (conv mc depth) sp1 sp2

and conv_neutral (mc : MetaContext.t) (depth : lvl) (n1 : neutral)
    (n2 : neutral) : bool =
  let heads_eq =
    match (n1.head, n2.head) with
    | HVar l1, HVar l2 -> l1 = l2
    | HMeta id1, HMeta id2 -> id1 = id2
    | HPrim n1, HPrim n2 -> String.equal n1 n2
    | _ -> false
  in
  heads_eq && conv_frames mc depth n1.frames n2.frames

and conv_frames (mc : MetaContext.t) (depth : lvl) (fs1 : frame list)
    (fs2 : frame list) : bool =
  match (fs1, fs2) with
  | [], [] -> true
  | FApp v1 :: rest1, FApp v2 :: rest2 ->
      conv mc depth v1 v2 && conv_frames mc depth rest1 rest2
  | ( FIf { then_ = t1; else_ = e1 } :: rest1,
      FIf { then_ = t2; else_ = e2 } :: rest2 ) ->
      conv mc depth (eval mc t1.env t1.body) (eval mc t2.env t2.body)
      && conv mc depth (eval mc e1.env e1.body) (eval mc e2.env e2.body)
      && conv_frames mc depth rest1 rest2
  | FProj i1 :: rest1, FProj i2 :: rest2 ->
      i1 = i2 && conv_frames mc depth rest1 rest2
  | FDot n1 :: rest1, FDot n2 :: rest2 ->
      String.equal n1 n2 && conv_frames mc depth rest1 rest2
  | FMatch bs1 :: rest1, FMatch bs2 :: rest2 ->
      List.length bs1 = List.length bs2
      && List.for_all2
           (fun (p1, c1) (p2, c2) ->
             conv_pat p1 p2
             && conv mc depth
                  (eval mc c1.env c1.body)
                  (eval mc c2.env c2.body))
           bs1 bs2
      && conv_frames mc depth rest1 rest2
  | _ -> false
