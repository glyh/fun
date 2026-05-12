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
    | VPi { domain = a; codomain = clo; _ } ->
        let var = VRigid { lvl = d; spine = [] } in
        Pi (go d a, go (d + 1) (Nbe.closure_apply mc clo var))
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
            | _ -> None) fields
        in
        Struct { con_fields; bindings; partial }
    | VNominal n -> Con n.name
    | VCon { name; spine; _ } -> go_spine d (Con name) spine
    | VFix { body = clo; _ } ->
        let var = VRigid { lvl = d; spine = [] } in
        Fix (go (d + 1) (Nbe.closure_apply mc clo var))
    | VNeutral { neutral = neu; _ } -> go_neutral d neu
  and go_spine (d : lvl) (head : term) (sp : spine) : term =
    List.fold_left (fun acc v -> Ap (acc, go d v)) head sp
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
        | FApp v -> Ap (acc, go d v)
        | FIf { then_; else_ } ->
            If (acc, go d (Nbe.eval mc then_.env then_.body),
                     go d (Nbe.eval mc else_.env else_.body))
        | FProj i -> Proj (acc, i)
        | FDot name -> Dot (acc, name))
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
let solve (mc : MetaContext.t) (id : meta_id) (sp : spine) (rhs : value) : unit =
  let ren = Renaming.of_spine sp in
  let sp_len = List.length sp in
  let rhs_term = rename mc id sp_len ren rhs in
  let solution = Renaming.wrap ren rhs_term in
  let v = Nbe.eval mc [] solution in
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
let rec unify (mc : MetaContext.t) (depth : lvl) (v1 : value) (v2 : value) : unit =
  let v1 = Nbe.force mc v1 in
  let v2 = Nbe.force mc v2 in
  match (v1, v2) with
  | VU, VU -> ()
  | VAtom a1, VAtom a2 when Syntax.Ast.Atom.equal a1 a2 -> ()
  | VAtomTy t1, VAtomTy t2 when equal_atom_ty t1 t2 -> ()
  | VPi { domain = a1; codomain = clo1; _ }, VPi { domain = a2; codomain = clo2; _ } ->
      unify mc depth a1 a2;
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc (depth + 1)
        (Nbe.closure_apply mc clo1 var)
        (Nbe.closure_apply mc clo2 var)
  | VLam { body = clo1; _ }, VLam { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc (depth + 1)
        (Nbe.closure_apply mc clo1 var)
        (Nbe.closure_apply mc clo2 var)
  | VLam { body = clo; _ }, v | v, VLam { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc (depth + 1) (Nbe.closure_apply mc clo var) (Nbe.apply mc v var)
  | VProd elems1, VProd elems2 | VProdTy elems1, VProdTy elems2 ->
      if List.length elems1 <> List.length elems2 then
        (* (a, b) vs (a, b, c) — different arities; no substitution can
           make a 2-tuple equal to a 3-tuple. *)
        raise (UnifyError TupleLengthMismatch);
      List.iter2 (unify mc depth) elems1 elems2
  | VStruct { fields = fs1; partial = p1 }, VStruct { fields = fs2; partial = p2 } ->
      let visible fs = List.filter (fun (_, k, _) -> k <> Private) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      if (not p1) && (not p2) then begin
        (* Both concrete: strict equality *)
        if List.length vs1 <> List.length vs2 then
          raise (UnifyError TupleLengthMismatch);
        List.iter2
          (fun (n1, k1, v1) (n2, k2, v2) ->
            if not (String.equal n1 n2) then raise (UnifyError StructFieldMismatch);
            if k1 <> k2 then raise (UnifyError StructFieldMismatch);
            unify mc depth v1 v2)
          vs1 vs2
      end else
        (* At least one partial: smaller constrains larger *)
        let small, large = if List.length vs1 <= List.length vs2 then vs1, vs2 else vs2, vs1 in
        List.iter
          (fun (name, kind, ty) ->
            match List.find_opt (fun (n, k, _) ->
              String.equal n name && (k = kind || kind = Field && k = Public || kind = Public && k = Field))
              large
            with
            | Some (_, _, large_ty) -> unify mc depth ty large_ty
            | None -> raise (UnifyError StructFieldMismatch))
          small
  | VNominal n1, VNominal n2 ->
      if n1.id <> n2.id then
        raise (UnifyError (NominalMismatch (n1.name, n2.name)));
      if List.length n1.params <> List.length n2.params then
        raise (UnifyError (NominalMismatch (n1.name, n2.name)));
      List.iter2 (unify mc depth) n1.params n2.params
  | VRigid { lvl = l1; spine = sp1 }, VRigid { lvl = l2; spine = sp2 } when l1 = l2 ->
      unify_spine mc depth sp1 sp2
  | VFlex { id = id1; spine = sp1 }, VFlex { id = id2; spine = sp2 } when id1 = id2 ->
      unify_spine mc depth sp1 sp2
  | VFlex { id; spine = sp }, v | v, VFlex { id; spine = sp } -> solve mc id sp v
  | VCon c1, VCon c2 ->
      if not (String.equal c1.name c2.name) then raise (UnifyError CannotUnify);
      unify_spine mc depth c1.spine c2.spine
  | VNeutral { neutral = n1; _ }, VNeutral { neutral = n2; _ } -> unify_neutral mc depth n1 n2
  | VFix { body = clo1; _ }, VFix { body = clo2; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      unify mc (depth + 1)
        (Nbe.closure_apply mc clo1 var)
        (Nbe.closure_apply mc clo2 var)
  | _ ->
      (* Mismatched head constructors, e.g. VU vs VAtom, or VAtomTy TI64
         vs VAtomTy TBool. No possible substitution can make these equal. *)
      raise
        (UnifyError CannotUnify)

(** Unify two argument lists pointwise. Both spines must have the same
    length — a variable with different arities can't be unified. *)
and unify_spine (mc : MetaContext.t) (depth : lvl) (sp1 : spine) (sp2 : spine) :
    unit =
  if List.length sp1 <> List.length sp2 then
    (* #0[5] vs #0[5, true] — same variable, different number of arguments. *)
    raise (UnifyError SpineLengthMismatch);
  List.iter2 (unify mc depth) sp1 sp2

(** Unify two stuck computations. First check the heads match (same
    variable level, same metavariable id, or same primitive name),
    then recurse on the frames. *)
and unify_neutral (mc : MetaContext.t) (depth : lvl) (n1 : neutral) (n2 : neutral) :
    unit =
  (match (n1.head, n2.head) with
  | HVar l1, HVar l2 when l1 = l2 -> ()
  | HMeta id1, HMeta id2 when id1 = id2 -> ()
  | HPrim n1, HPrim n2 when String.equal n1 n2 -> ()
  | _ ->
      (* + vs ==, or HVar 0 vs HVar 1 — different primitives or different
         variable levels. *)
      raise (UnifyError NeutralHeadMismatch));
  unify_frames mc depth n1.frames n2.frames

(** Unify two frame stacks element-wise. FApp frames unify their arguments;
    FIf frames unify the then-branches and else-branches (via [eval] to
    unfold closures); FProj frames unify the projection index. *)
and unify_frames (mc : MetaContext.t) (depth : lvl) (fs1 : frame list)
    (fs2 : frame list) : unit =
  match (fs1, fs2) with
  | [], [] -> ()
  | FApp v1 :: rest1, FApp v2 :: rest2 ->
      unify mc depth v1 v2;
      unify_frames mc depth rest1 rest2
  | FIf { then_ = t1; else_ = e1 } :: rest1, FIf { then_ = t2; else_ = e2 } :: rest2
    ->
      unify mc depth
        (Nbe.eval mc t1.env t1.body)
        (Nbe.eval mc t2.env t2.body);
      unify mc depth
        (Nbe.eval mc e1.env e1.body)
        (Nbe.eval mc e2.env e2.body);
      unify_frames mc depth rest1 rest2
  | FProj i1 :: rest1, FProj i2 :: rest2 when i1 = i2 ->
      unify_frames mc depth rest1 rest2
  | FDot n1 :: rest1, FDot n2 :: rest2 when String.equal n1 n2 ->
      unify_frames mc depth rest1 rest2
  | _ ->
      (* FApp vs FIf, or FIf branches differ — frame stacks don't match
         structurally. *)
      raise (UnifyError FrameMismatch)
