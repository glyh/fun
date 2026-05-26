open Core

type unify_error =
  | NonLinearSpine
  | NonVariableInSpine
  | VarNotInSpine of lvl
  | NeutralVarNotInSpine of lvl
  | OccursCheck
  | CannotUnify
  | TupleLengthMismatch
  | SpineLengthMismatch
  | NeutralHeadMismatch
  | FrameMismatch
  | StructFieldMismatch
  | NominalMismatch of string * string
  | EffectMismatch of string * string
  | EffectRowMismatch

exception UnifyError of unify_error

(** Renaming from original variable levels to solution lambda levels.

    In [λa. λb. λc. ?M a c], the environment is [c=2, b=1, a=0]. The spine
    is [[#0, #2]] (a then c, in application order). [of_spine] produces:

      [(0, 0); (2, 1)]

    meaning: level 0 (a) → target level 0 (outermost lambda),
             level 2 (c) → target level 1 (inner lambda).

    The solution becomes [λx0. λx1. rhs'], so a binds to x0 and c to x1 —
    matching the application order [apply(apply(?M, a), c)]. *)
module Renaming = struct
  type t = (lvl * lvl) list
  (** Association list [(var_level, target_level)] mapping each spine
      variable's level to the de Bruijn level of its solution lambda
      (0 = outermost, 1 = next, …). *)

  let of_spine (sp : spine) : t =
    let rec go i sp =
      match sp with
      | [] -> []
      | v :: rest -> (
          match v with
          | VRigid { lvl = l; spine = [] } ->
              let ren = go (i + 1) rest in
              if List.exists (fun (l', _) -> l = l') ren then
                (* ?M[x, x] — same variable appears twice in the spine.
                   Multiple solutions exist (e.g. λa b. a vs λa b. b),
                   no unique most-general answer. *)
                raise (UnifyError NonLinearSpine)
              else (l, i) :: ren
          | _ ->
              (* ?M[5] — spine argument is not a rigid variable.
                 Pattern unification requires every argument to be a
                 distinct variable; constants block abstraction. *)
              raise (UnifyError NonVariableInSpine))
    in
    go 0 sp

  let find (ren : t) (l : lvl) : lvl option = List.assoc_opt l ren

  (** Wrap the renamed RHS in [sp_len] lambdas (inner to outer). Each
      lambda binds one argument from the metavariable's spine, so the
      solution [λx0. λx1. rhs'] matches the application order of [?M]. *)
  let wrap (ren : t) (rhs : term) : term =
    List.fold_left (fun body _ -> Lam body) rhs (List.rev ren)
end

(** Traverse the RHS value and replace each spine variable with a [Var]
    reference into the solution lambda chain.  [depth] is the number of
    lambda binders in the solution ([sp_len]), so the produced term's
    de Bruijn indices are relative to the solution evaluated at depth 0.

    Also performs the occurs check: if the metavariable [meta_id] appears
    anywhere in the RHS, unification would produce an infinite term. *)
let rename (mc : MetaContext.t) (meta_id : meta_id) (depth : lvl)
    (ren : Renaming.t) (v : value) : term =
  let rec go (d : lvl) (v : value) : term =
    match Nbe.force mc v with
    | VRigid { lvl = l; spine = sp } -> (
        match Renaming.find ren l with
        | Some target -> go_spine d (Var (Nbe.lvl_to_ix d target)) sp
        | None ->
            (* The RHS references a variable at level [l] that isn't in the
               metavariable's spine. The RHS would need to depend on a
               variable the solution's lambdas don't bind — unsolvable. *)
            raise (UnifyError (VarNotInSpine l)))
    | VFlex { id; _ } when id = meta_id ->
        (* ?M = pi(_, ?M, _) or similar — the metavariable appears in its
           own solution. Infinite term, no finite solution exists. *)
        raise (UnifyError OccursCheck)
    | VFlex { id; spine = sp } -> go_spine d (Meta id) sp
    | VLam { body = clo; _ } ->
        let var = VRigid { lvl = d; spine = [] } in
        Lam (go (d + 1) (Nbe.closure_apply mc clo var))
    | VPi { explicitness = expl; domain = a; effects; codomain = clo } ->
        let var = VRigid { lvl = d; spine = [] } in
        let row =
          match effects.tail with
          | Some _ -> raise (UnifyError EffectRowMismatch)
          | None ->
              { effects = List.map (fun eff -> go (d + 1) (Nbe.eval mc (var :: effects.env) eff)) effects.effects;
                tail = None }
        in
        Pi
          { explicitness = expl;
            domain = go d a;
            effects = row;
            codomain = go (d + 1) (Nbe.closure_apply mc clo var) }
    | VU -> U
    | VAtom a -> Atom a
    | VAtomTy t -> AtomTy t
    | VProd elems -> Prod (List.map (go d) elems)
    | VProdTy elems -> ProdTy (List.map (go d) elems)
    | VStruct { fields; partial } ->
        let con_fields =
          List.filter_map (fun (n, k, v) ->
            if k = Field then Some (n, go d v) else None) fields
        in
        let bindings =
          List.filter_map (fun (n, k, v) ->
            match k with
            | Public -> Some (LetBind (n, Public, go d v))
            | Private -> Some (LetBind (n, Private, go d v))
            | Method -> Some (LetBind (n, Method, go d v))
            | PrivateMethod -> Some (LetBind (n, PrivateMethod, go d v))
            | _ -> None) fields
        in
        Struct { con_fields; bindings; partial }
    | VRecord { typ; fields } ->
        RecordConstruct
          { typ = go d typ;
            fields = List.map (fun (name, value) -> (name, go d value)) fields }
    | VNominal n ->
        let params_terms = List.map (go d) n.params in
        List.fold_left (fun acc t -> Ap (acc, Explicit, t))
          (Con n.name) params_terms
    | VEffect e ->
        EffectRef (e.name, List.map (go d) e.params)
    | VCon { name; spine; _ } ->
        let spine_terms = List.map (go d) spine in
        List.fold_left (fun acc t -> Ap (acc, Explicit, t))
          (Con name) spine_terms
    | VFix { body = clo; _ } ->
        let var = VRigid { lvl = d; spine = [] } in
        Fix (go (d + 1) (Nbe.closure_apply mc clo var))
    | VNeutral { neutral = neu; _ } -> go_neutral d neu
  and go_spine (d : lvl) (head : term) (sp : spine) : term =
    List.fold_left (fun acc v -> Ap (acc, Explicit, go d v)) head sp
  and go_neutral (d : lvl) (neu : neutral) : term =
    let head =
      match neu.head with
      | HVar l -> (
          match Renaming.find ren l with
          | Some target -> Var (Nbe.lvl_to_ix d target)
          | None ->
              (* Same as [VarNotInSpine] but the variable appears inside a
                 neutral head (e.g. stuck in an FIf frame). *)
              raise (UnifyError (NeutralVarNotInSpine l)))
      | HMeta id ->
          if id = meta_id then
            raise (UnifyError OccursCheck) (* same occurs check, via neutral *)
          else Meta id
      | HPrim name -> Prim name
    in
    go_frames d head neu.frames
  and go_frames (d : lvl) (head : term) (frames : frame list) : term =
    List.fold_left
      (fun acc frame ->
        match frame with
        | FApp v -> Ap (acc, Explicit, go d v)
        | FIf { then_; else_ } ->
            If (acc, go d (Nbe.eval mc then_.env then_.body),
                     go d (Nbe.eval mc else_.env else_.body))
        | FProj i -> Proj (acc, i)
        | FDot name -> Dot (acc, name)
        | FMatch branches ->
            Match
              ( acc,
                List.map
                  (fun (p, clo) ->
                    (p, go d (Nbe.eval mc clo.env clo.body)))
                  branches ))
      head frames
  in
  go depth v

(** Solve [?M[sp] = rhs] via pattern unification (Miller).

    1. Build a renaming from the spine (map each spine variable to a
       solution lambda level).
    2. [rename] the RHS, replacing spine variables with de Bruijn indices
       that point into the solution lambda chain.
    3. [wrap] the renamed RHS in lambdas (one per spine argument).
    4. Evaluate the result at depth 0 and install it as the solution. *)
let solve (mc : MetaContext.t) (env : env) (id : meta_id) (sp : spine) (rhs : value) : unit =
  match sp with
  | [] ->
      let rec occurs_check v =
        match Nbe.force mc v with
        | VFlex { id = id'; spine } ->
            if id' = id then raise (UnifyError OccursCheck);
            List.iter occurs_check spine
        | VPi { domain = a; effects; codomain = clo; _ } ->
            occurs_check a;
            let var = VRigid { lvl = 0; spine = [] } in
            List.iter (fun eff -> occurs_check (Nbe.eval mc (var :: effects.env) eff)) effects.effects;
            occurs_check (Nbe.closure_apply mc clo var)
        | VProd elems | VProdTy elems -> List.iter occurs_check elems
        | VStruct { fields; _ } -> List.iter (fun (_, _, v) -> occurs_check v) fields
        | VRecord { typ; fields } ->
            occurs_check typ;
            List.iter (fun (_, v) -> occurs_check v) fields
        | VNominal n -> List.iter occurs_check n.params
        | VEffect e -> List.iter occurs_check e.params
        | VCon { spine; nominal; _ } -> List.iter occurs_check spine; occurs_check nominal
        | VNeutral { neutral = { frames; _ }; _ } ->
            List.iter (fun f -> match f with
              | FApp v -> occurs_check v
              | _ -> ()) frames
        | VU | VAtom _ | VAtomTy _ | VRigid _ | VLam _ | VFix _ -> ()
      in
      occurs_check rhs;
      MetaContext.solve mc id rhs
  | _ ->
      let ren = Renaming.of_spine sp in
      let sp_len = List.length sp in
      let rhs_term = rename mc id sp_len ren rhs in
      let solution = Renaming.wrap ren rhs_term in
      let v = Nbe.eval mc env solution in
      MetaContext.solve mc id v

(** Structural unification of two semantic values. Dispatches by constructor:

     - Identical atoms/types → success.
     - VPi / VLam / VFix → unify domains, then unify codomains extended
       with a fresh rigid variable (extensional equality).
     - VLam vs non-lambda → η-expand the non-lambda side by applying it
       to a fresh variable, then compare.
     - VProd / VProdTy → unify element-wise.
     - VRigid / VFlex → unify spines (same variable/meta).
     - VFlex vs anything → [solve] the metavariable.
     - VNeutral → decompose head + frames.
     - Otherwise → [CannotUnify]. *)
let rec unify (mc : MetaContext.t) (env : env) (depth : lvl) (v1 : value) (v2 : value) : unit =
  let v1 = Nbe.force mc v1 in
  let v2 = Nbe.force mc v2 in
  match (v1, v2) with
  | VU, VU -> ()
  | VAtom a1, VAtom a2 when Atom.equal a1 a2 -> ()
  | VAtomTy t1, VAtomTy t2 when equal_atom_ty t1 t2 -> ()
  | ( VPi { explicitness = e1; domain = a1; effects = effs1; codomain = clo1 },
      VPi { explicitness = e2; domain = a2; effects = effs2; codomain = clo2 } )
    when e1 = e2 ->
      unify mc env depth a1 a2;
      let var = VRigid { lvl = depth; spine = [] } in
      unify_effect_rows mc env (depth + 1)
        (Nbe.eval_effect_row_closure mc effs1 var)
        (Nbe.eval_effect_row_closure mc effs2 var);
      unify mc env (depth + 1)
        (Nbe.closure_apply mc clo1 var)
        (Nbe.closure_apply mc clo2 var)
  | VLam { body = clo1; _ }, VLam { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc env (depth + 1)
        (Nbe.closure_apply mc clo1 var)
        (Nbe.closure_apply mc clo2 var)
  | VLam { body = clo; _ }, v | v, VLam { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc env (depth + 1) (Nbe.closure_apply mc clo var) (Nbe.apply mc v var)
  | VProd elems1, VProd elems2 | VProdTy elems1, VProdTy elems2 ->
      if List.length elems1 <> List.length elems2 then
        raise (UnifyError TupleLengthMismatch);
      List.iter2 (unify mc env depth) elems1 elems2
  | VStruct { fields = fs1; partial = p1 }, VStruct { fields = fs2; partial = p2 } ->
      let visible fs = List.filter (fun (_, k, _) -> k <> Private && k <> PrivateMethod) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      if (not p1) && (not p2) then begin
        if List.length vs1 <> List.length vs2 then
          raise (UnifyError TupleLengthMismatch);
        List.iter2
          (fun (n1, k1, v1) (n2, k2, v2) ->
            if not (String.equal n1 n2) then raise (UnifyError StructFieldMismatch);
            if k1 <> k2 then raise (UnifyError StructFieldMismatch);
            unify mc env depth v1 v2)
          vs1 vs2
      end else
        let small, large = if List.length vs1 <= List.length vs2 then vs1, vs2 else vs2, vs1 in
        List.iter
          (fun (name, kind, ty) ->
            match List.find_opt (fun (n, k, _) ->
              String.equal n name && (k = kind || kind = Field && k = Public || kind = Public && k = Field))
              large
            with
            | Some (_, _, large_ty) -> unify mc env depth ty large_ty
            | None -> raise (UnifyError StructFieldMismatch))
          small
  | VRecord r1, VRecord r2 ->
      unify mc env depth r1.typ r2.typ;
      if List.length r1.fields <> List.length r2.fields then
        raise (UnifyError StructFieldMismatch);
      List.iter2
        (fun (n1, v1) (n2, v2) ->
          if not (String.equal n1 n2) then raise (UnifyError StructFieldMismatch);
          unify mc env depth v1 v2)
        r1.fields r2.fields
  | VNominal n1, VNominal n2 ->
      if n1.id <> n2.id then
        raise (UnifyError (NominalMismatch (n1.name, n2.name)));
      if List.length n1.params <> List.length n2.params then
        raise (UnifyError (NominalMismatch (n1.name, n2.name)));
      List.iter2 (unify mc env depth) n1.params n2.params
  | VEffect e1, VEffect e2 ->
      if e1.id <> e2.id then
        raise (UnifyError (EffectMismatch (e1.name, e2.name)));
      if List.length e1.params <> List.length e2.params then
        raise (UnifyError (EffectMismatch (e1.name, e2.name)));
      List.iter2 (unify mc env depth) e1.params e2.params
  | VRigid { lvl = l1; spine = sp1 }, VRigid { lvl = l2; spine = sp2 } when l1 = l2 ->
      unify_spine mc env depth sp1 sp2
  | VFlex { id = id1; spine = sp1 }, VFlex { id = id2; spine = sp2 } when id1 = id2 ->
      unify_spine mc env depth sp1 sp2
  | VFlex { id; spine = sp }, v | v, VFlex { id; spine = sp } -> solve mc env id sp v
  | VCon c1, VCon c2 ->
      if not (String.equal c1.name c2.name) then raise (UnifyError CannotUnify);
      unify_spine mc env depth c1.spine c2.spine
  | VNeutral { neutral = n1; _ }, VNeutral { neutral = n2; _ } -> unify_neutral mc env depth n1 n2
  | VFix { body = clo1; _ }, VFix { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc env (depth + 1)
        (Nbe.closure_apply mc clo1 var)
        (Nbe.closure_apply mc clo2 var)
  | _ ->
      raise
        (UnifyError CannotUnify)

(** Unify two argument lists pointwise. Both spines must have the same
    length — a variable with different arities can't be unified. *)
and unify_spine (mc : MetaContext.t) (env : env) (depth : lvl) (sp1 : spine) (sp2 : spine) :
    unit =
  if List.length sp1 <> List.length sp2 then
    raise (UnifyError SpineLengthMismatch);
  List.iter2 (unify mc env depth) sp1 sp2

and unify_effect_rows (mc : MetaContext.t) (env : env) (depth : lvl)
    (row1 : value list) (row2 : value list) : unit =
  let rec remove_match eff = function
    | [] -> raise (UnifyError EffectRowMismatch)
    | candidate :: rest -> (
        try
          unify mc env depth eff candidate;
          rest
        with UnifyError _ -> candidate :: remove_match eff rest)
  in
  if List.length row1 <> List.length row2 then raise (UnifyError EffectRowMismatch);
  ignore (List.fold_left (fun remaining eff -> remove_match eff remaining) row2 row1)

(** Unify two stuck computations. First check the heads match (same
    variable level, same metavariable id, or same primitive name),
    then recurse on the frames. *)
and unify_neutral (mc : MetaContext.t) (env : env) (depth : lvl) (n1 : neutral) (n2 : neutral) :
    unit =
  (match (n1.head, n2.head) with
  | HVar l1, HVar l2 when l1 = l2 -> ()
  | HMeta id1, HMeta id2 when id1 = id2 -> ()
  | HPrim n1, HPrim n2 when String.equal n1 n2 -> ()
  | _ ->
      raise (UnifyError NeutralHeadMismatch));
  unify_frames mc env depth n1.frames n2.frames

(** Unify two frame stacks element-wise. FApp frames unify their arguments;
    FIf frames unify the then-branches and else-branches (via [eval] to
    unfold closures); FProj frames unify the projection index. *)
and unify_frames (mc : MetaContext.t) (env : env) (depth : lvl) (fs1 : frame list)
    (fs2 : frame list) : unit =
  match (fs1, fs2) with
  | [], [] -> ()
  | FApp v1 :: rest1, FApp v2 :: rest2 ->
      unify mc env depth v1 v2;
      unify_frames mc env depth rest1 rest2
  | FIf { then_ = t1; else_ = e1 } :: rest1, FIf { then_ = t2; else_ = e2 } :: rest2
    ->
      unify mc env depth
        (Nbe.eval mc t1.env t1.body)
        (Nbe.eval mc t2.env t2.body);
      unify mc env depth
        (Nbe.eval mc e1.env e1.body)
        (Nbe.eval mc e2.env e2.body);
      unify_frames mc env depth rest1 rest2
  | FProj i1 :: rest1, FProj i2 :: rest2 when i1 = i2 ->
      unify_frames mc env depth rest1 rest2
  | FDot n1 :: rest1, FDot n2 :: rest2 when String.equal n1 n2 ->
      unify_frames mc env depth rest1 rest2
  | FMatch bs1 :: rest1, FMatch bs2 :: rest2 ->
      if List.length bs1 <> List.length bs2 then
        raise (UnifyError FrameMismatch);
      List.iter2
        (fun (p1, c1) (p2, c2) ->
          if not (Nbe.conv_pat p1 p2) then raise (UnifyError FrameMismatch);
          unify mc env depth
            (Nbe.eval mc c1.env c1.body)
            (Nbe.eval mc c2.env c2.body))
        bs1 bs2;
      unify_frames mc env depth rest1 rest2
  | _ ->
      raise (UnifyError FrameMismatch)
