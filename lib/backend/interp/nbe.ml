open Core

exception EvalError = Nbe_error.EvalError

let make_cont = Nbe_support.make_cont
let bind_result = Nbe_support.bind_result
let visible_kind = Nbe_support.visible_kind
let dot_value = Nbe_support.dot_value
let result_value = Nbe_support.result_value
let atom_ty_of_atom = Nbe_prim.atom_ty_of_atom
let prim_table = Nbe_prim.prim_table
let stx_prim_table = Nbe_prim.stx_prim_table

let rec closure_apply (mc : MetaContext.t) (c : closure) (v : value) : value =
  eval mc (v :: c.env) c.body

and eval (mc : MetaContext.t) (env : env) (t : term) : value =
  result_value mc (eval_result mc env t)

and sequence_values mc env terms k =
  match terms with
  | [] -> k []
  | term :: rest ->
      bind_result (eval_result mc env term) (fun value ->
          sequence_values mc env rest (fun values -> k (value :: values)))

and eval_result (mc : MetaContext.t) (env : env) (t : term) : result =
  match t with
  | Var ix -> Done (List.nth env ix)
  | Lam body -> Done (VLam { body = { env; body } })
  | Ap (f, _, a) ->
      bind_result (eval_result mc env f) (fun vf ->
          bind_result (eval_result mc env a) (fun va -> apply_result mc vf va))
  | Let (_, def, body) ->
      bind_result (eval_result mc env def) (fun vdef ->
          eval_result mc (vdef :: env) body)
  | Pi { explicitness; domain; effects; codomain } ->
      Done
        (VPi
           {
             explicitness;
             domain = eval mc env domain;
             effects = effect_row_closure env effects;
             codomain = { env; body = codomain };
           })
  | U -> Done VU
  | EffectRowTy -> Done VEffectRowTy
  | EffectRowLit row -> Done (eval_effect_row_literal mc env row)
  | Atom a -> Done (VAtom a)
  | AtomTy t -> Done (VAtomTy t)
  | Stx stx -> Done (VStx (StxExpr stx))
  | RefTy a -> bind_result (eval_result mc env a) (fun a -> Done (VRefTy a))
  | RefNew e ->
      bind_result (eval_result mc env e) (fun value -> Done (VRef (ref value)))
  | RefGet r ->
      bind_result (eval_result mc env r) (fun ref_value ->
          match force mc ref_value with
          | VRef cell -> Done !cell
          | VNeutral { ty; neutral } -> (
              match force mc ty with
              | VRefTy elem_ty ->
                  Done
                    (VNeutral
                       {
                         ty = elem_ty;
                         neutral =
                           {
                             neutral with
                             frames = neutral.frames @ [ FRefGet ];
                           };
                       })
              | _ -> raise (EvalError "deref of non-ref"))
          | VFlex { id; spine = sp } ->
              let frames = List.map (fun v -> FApp v) sp in
              Done
                (VNeutral
                   {
                     ty = VU;
                     neutral =
                       { head = HMeta id; frames = frames @ [ FRefGet ] };
                   })
          | VRigid { lvl; spine = sp } ->
              let frames = List.map (fun v -> FApp v) sp in
              Done
                (VNeutral
                   {
                     ty = VU;
                     neutral =
                       { head = HVar lvl; frames = frames @ [ FRefGet ] };
                   })
          | _ -> raise (EvalError "deref of non-ref"))
  | RefSet (r, e) ->
      bind_result (eval_result mc env r) (fun ref_value ->
          bind_result (eval_result mc env e) (fun value ->
              match force mc ref_value with
              | VRef cell ->
                  cell := value;
                  Done (VAtom Unit)
              | VNeutral { neutral; _ } ->
                  Done
                    (VNeutral
                       {
                         ty = VAtomTy Atom_ty.TUnit;
                         neutral =
                           {
                             neutral with
                             frames = neutral.frames @ [ FRefSet value ];
                           };
                       })
              | VFlex { id; spine = sp } ->
                  let frames = List.map (fun v -> FApp v) sp in
                  Done
                    (VNeutral
                       {
                         ty = VAtomTy Atom_ty.TUnit;
                         neutral =
                           {
                             head = HMeta id;
                             frames = frames @ [ FRefSet value ];
                           };
                       })
              | VRigid { lvl; spine = sp } ->
                  let frames = List.map (fun v -> FApp v) sp in
                  Done
                    (VNeutral
                       {
                         ty = VAtomTy Atom_ty.TUnit;
                         neutral =
                           {
                             head = HVar lvl;
                             frames = frames @ [ FRefSet value ];
                           };
                       })
              | _ -> raise (EvalError "assignment to non-ref")))
  | If (cond, then_, else_) ->
      bind_result (eval_result mc env cond) (fun vc ->
          eval_if mc env vc then_ else_)
  | Prod elems ->
      sequence_values mc env elems (fun values -> Done (VProd values))
  | ProdTy elems ->
      sequence_values mc env elems (fun values -> Done (VProdTy values))
  | Module { bindings } ->
      let rec eval_binds env acc = function
        | [] -> (env, List.rev acc)
        | LetBind (name, kind, def) :: rest ->
            let vdef = eval mc env def in
            eval_binds (vdef :: env)
              (ModuleField (name, kind, vdef) :: acc)
              rest
        | TypeBind (name, kind, nominal, ctors) :: rest ->
            let ctor_entries =
              List.map (fun (n, v) -> ModuleField (n, kind, v)) ctors
            in
            let env =
              match nominal with
              | VNominal { num_params; _ } when num_params > 0 ->
                  List.fold_left
                    (fun e _ -> (VAtomTy Atom_ty.TUnit) :: e)
                    env (List.init num_params (fun _ -> ()))
              | _ -> env
            in
            let env = List.fold_left (fun e (_, v) -> v :: e) env ctors in
            let env = nominal :: env in
            eval_binds env
              (List.rev_append ctor_entries
                 (ModuleField (name, kind, nominal) :: acc))
              rest
        | EffectBind (name, kind, eff) :: rest ->
            eval_binds (eff :: env) (ModuleField (name, kind, eff) :: acc) rest
        | ImplBind (kind, def, ty) :: rest ->
            let vdef = eval mc env def in
            eval_binds (vdef :: env) (ModuleImpl (kind, ty, vdef) :: acc) rest
        | PatternSynBind (name, kind, syn) :: rest ->
            eval_binds (syn :: env) (ModuleField (name, kind, syn) :: acc) rest
      in
      let _env, entries = eval_binds env [] bindings in
      Done (VModule { entries; partial = false })
  | Struct { con_fields; bindings; partial } ->
      (* con_fields: all at same scope, no sequential dependency *)
      let con_vals =
        List.map (fun (name, ty) -> (name, Field, eval mc env ty)) con_fields
      in
      (* bindings: sequential, TypeBind stores values directly *)
      let rec eval_binds env acc_entries = function
        | [] -> (env, List.rev acc_entries)
        | LetBind (name, kind, def) :: rest ->
            let vdef = eval mc env def in
            eval_binds (vdef :: env)
              (StructField (name, kind, vdef) :: acc_entries)
              rest
        | TypeBind (name, kind, nominal, ctors) :: rest ->
            let ctor_entries =
              List.map (fun (n, v) -> StructField (n, kind, v)) ctors
            in
            let env =
              match nominal with
              | VNominal { num_params; _ } when num_params > 0 ->
                  List.fold_left
                    (fun e _ -> (VAtomTy Atom_ty.TUnit) :: e)
                    env (List.init num_params (fun _ -> ()))
              | _ -> env
            in
            let env = List.fold_left (fun e (_, v) -> v :: e) env ctors in
            let env = nominal :: env in
            eval_binds env
              (List.rev_append ctor_entries
                 (StructField (name, kind, nominal) :: acc_entries))
              rest
        | EffectBind (name, kind, eff) :: rest ->
            eval_binds (eff :: env)
              (StructField (name, kind, eff) :: acc_entries)
              rest
        | ImplBind (kind, def, ty) :: rest ->
            let vdef = eval mc env def in
            eval_binds (vdef :: env)
              (StructImpl (kind, ty, vdef) :: acc_entries)
              rest
        | PatternSynBind (name, kind, syn) :: rest ->
            eval_binds (syn :: env) (StructField (name, kind, syn) :: acc_entries) rest
      in
      let _env, bind_entries = eval_binds env [] bindings in
      let con_entries =
        List.map
          (fun (name, kind, value) -> StructField (name, kind, value))
          con_vals
      in
      Done (VStruct { entries = con_entries @ bind_entries; partial })
  | RecordConstruct { typ; fields } ->
      bind_result (eval_result mc env typ) (fun typ ->
          let rec go acc = function
            | [] -> Done (VRecord { typ; fields = List.rev acc })
            | (name, value) :: rest ->
                bind_result (eval_result mc env value) (fun value ->
                    go ((name, value) :: acc) rest)
          in
          go [] fields)
  | Proj (e, i) ->
      bind_result (eval_result mc env e) (fun vs ->
          Done
            (match vs with
            | VProd elems -> List.nth elems i
            | VNeutral { neutral; _ } ->
                VNeutral
                  {
                    ty = VU;
                    neutral =
                      { neutral with frames = neutral.frames @ [ FProj i ] };
                  }
            | VFlex { id; spine = sp } ->
                let frames = List.map (fun v -> FApp v) sp in
                VNeutral
                  {
                    ty = VU;
                    neutral = { head = HMeta id; frames = frames @ [ FProj i ] };
                  }
            | VRigid { lvl; spine = sp } ->
                let frames = List.map (fun v -> FApp v) sp in
                VNeutral
                  {
                    ty = VU;
                    neutral = { head = HVar lvl; frames = frames @ [ FProj i ] };
                  }
            | _ -> raise (EvalError "projection of non-product")))
  | Dot (e, name) ->
      bind_result (eval_result mc env e) (fun value ->
          Done (dot_value value name))
  | Open (s, body) ->
      bind_result (eval_result mc env s) (fun vs ->
          match vs with
          | VModule { entries; partial = _ } ->
              let vals =
                List.filter_map
                  (function
                    | ModuleField (_, k, v) when k = Public || k = Method ->
                        Some v
                    | ModuleImpl (k, _, v) when k = Public -> Some v
                    | _ -> None)
                  entries
              in
              let env' = List.fold_left (fun e v -> v :: e) env vals in
              eval_result mc env' body
          | _ -> raise (EvalError "open of non-module"))
  | Fix body -> Done (VFix { body = { env; body } })
  | Con name -> Done (eval_con env name)
  | NomRef (name, params) -> (
      match eval_con env name with
      | VNominal _ as nom ->
          sequence_values mc env params (fun param_vals ->
              Done (List.fold_left (fun acc v -> apply mc acc v) nom param_vals))
      | _ -> raise (EvalError ("NomRef is not VNominal: " ^ name)))
  | EffectRef (name, params) -> (
      match eval_eff env name with
      | VEffect _ as eff ->
          sequence_values mc env params (fun param_vals ->
              Done (List.fold_left (fun acc v -> apply mc acc v) eff param_vals))
      | _ -> raise (EvalError ("EffectRef is not VEffect: " ^ name)))
  | TraitRef { trait_id; trait_name } -> Done (VTrait { trait_id; trait_name })
  | TraitDictTy { trait_id; trait_name; args; fields } ->
      sequence_values mc env args (fun arg_vals ->
          let rec eval_fields acc = function
            | [] ->
                Done
                  (VTraitDict
                     {
                       trait_id;
                       trait_name;
                       args = arg_vals;
                       fields = List.rev acc;
                     })
            | (name, field) :: rest ->
                bind_result (eval_result mc env field) (fun value ->
                    eval_fields ((name, value) :: acc) rest)
          in
          eval_fields [] fields)
  | SelfTypeRef args ->
      sequence_values mc env args (fun arg_vals -> Done (VSelfType arg_vals))
  | Ctor { name; spine; nominal_spine; nominal_value; _ } ->
      sequence_values mc env spine (fun spine_vals ->
          sequence_values mc env nominal_spine (fun nom_spine_vals ->
              match force mc nominal_value with
              | VNominal n ->
                  Done
                    (VCon
                       {
                         name;
                         spine = spine_vals;
                         nominal = VNominal { n with params = nom_spine_vals };
                       })
              | _ ->
                  raise
                    (EvalError "Ctor nominal is not VNominal")))
  | Prim name ->
      Done (VNeutral { ty = VU; neutral = { head = HPrim name; frames = [] } })
  | Meta id -> Done (eval_meta mc id)
  | InsertedMeta (id, bds) -> Done (eval_inserted_meta mc env id bds)
  | NominalDef { id; name; num_params; ctors; body } ->
      let elaborated_ctors =
        List.map
          (fun (cname, payloads) ->
            (cname, List.map (fun t -> { env; body = t }) payloads))
          ctors
      in
      let nominal =
        VNominal
          { id; name; num_params; params = []; constructors = elaborated_ctors }
      in
      (* Add dummy entries for type params (body expects them in scope) *)
      let depth = List.length env in
      let env =
        let rec add_params env i =
          if i >= num_params then env
          else add_params (VRigid { lvl = depth + i; spine = [] } :: env) (i + 1)
        in
        add_params env 0
      in
      (* Push the nominal template (used by NomRef/eval_con) *)
      let env = nominal :: env in
      (* For parameterized types, elaborator also pushes a type-name binding *)
      let env = if num_params > 0 then nominal :: env else env in
      let env =
        List.fold_left
          (fun env (cname, payload_clos) ->
            let payload_count = List.length payload_clos in
            let total_args = num_params + payload_count in
            if total_args = 0 then
              VCon { name = cname; spine = []; nominal } :: env
            else
              let ctor_body =
                let param_vars =
                  List.init num_params (fun i -> Var (total_args - 1 - i))
                in
                let payload_vars = List.init payload_count (fun i -> Var (payload_count - 1 - i)) in
                Ctor
                  {
                    name = cname;
                    spine = param_vars @ payload_vars;
                    nominal_name = name;
                    nominal_spine = param_vars;
                    nominal_value = nominal;
                  }
              in
              let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
              let ctor_term = wrap total_args ctor_body in
              eval mc env ctor_term :: env)
          env elaborated_ctors
      in
      eval_result mc env body
  | EffectDef { id; name; ops; body; _ } ->
      let elaborated_ops =
        List.map
          (fun (op_name, input, output) ->
            (op_name, { env; body = input }, { env; body = output }))
          ops
      in
      let eff =
        VEffect { id; name; params = []; operations = elaborated_ops }
      in
      eval_result mc (eff :: env) body
  | Match (scrut, branches) -> eval_match_result mc env scrut branches
  | Perform { eff; op; arg } ->
      bind_result (eval_result mc env eff) (fun eff ->
          bind_result (eval_result mc env arg) (fun arg ->
              match force mc eff with
              | VEffect _ as eff ->
                  Effect { eff; op; arg; k = (fun v -> Done v) }
              | _ -> raise (EvalError "perform target is not an effect")))

and try_prim_reduce (head : head) (frames : frame list) : value option =
  match head with
  | HPrim "panic" when List.length frames >= 2 ->
      let msg =
        match List.nth frames 1 with
        | FApp (VAtom (String s)) -> s
        | _ -> "panic"
      in
      raise (EvalError msg)
  | HPrim name -> (
      let atoms =
        List.filter_map (function FApp (VAtom a) -> Some a | _ -> None) frames
      in
      if List.length atoms = List.length frames then
        match Hashtbl.find_opt prim_table name with
        | Some f -> Option.map (fun a -> VAtom a) (f atoms)
        | None ->
          let args = List.filter_map (function FApp v -> Some v | _ -> None) frames in
          Option.bind (Hashtbl.find_opt stx_prim_table name) (fun f -> f args)
      else
        let args = List.filter_map (function FApp v -> Some v | _ -> None) frames in
        if List.length args = List.length frames
        then Option.bind (Hashtbl.find_opt stx_prim_table name) (fun f -> f args)
        else None)
  | _ -> None

and eval_effect_row_literal (mc : MetaContext.t) (env : env) (row : effect_row) : value =
  let effects = List.map (eval mc env) row.effects in
  let tail = Option.map (eval mc env) row.tail in
  VEffectRow { effect_values = effects; tail_value = tail }

and apply_result (mc : MetaContext.t) (vf : value) (va : value) : result =
  match vf with
  | VLam { body = clo; _ } -> eval_result mc (va :: clo.env) clo.body
  | VFix { body = clo; _ } ->
      let self = VFix { body = clo } in
      let unfolded = eval mc (self :: clo.env) clo.body in
      apply_result mc unfolded va
  | VCont c ->
      let cont = c in
      if cont.used then raise (EvalError "continuation already used");
      cont.used <- true;
      cont.resume va
  | VNeutral { ty; neutral = neu } ->
      let cod = apply_ty mc ty va in
      let frames = neu.frames @ [ FApp va ] in
      Done
        (match try_prim_reduce neu.head frames with
        | Some v -> v
        | None -> VNeutral { ty = cod; neutral = { head = neu.head; frames } })
  | VFlex { id; spine = sp } -> Done (VFlex { id; spine = sp @ [ va ] })
  | VRigid { lvl; spine = sp } -> Done (VRigid { lvl; spine = sp @ [ va ] })
  | VNominal n -> Done (VNominal { n with params = n.params @ [ va ] })
  | VEffect e -> Done (VEffect { e with params = e.params @ [ va ] })
  | VTraitDict d -> Done (VTraitDict { d with args = d.args @ [ va ] })
  | VCon c -> Done (VCon { c with spine = c.spine @ [ va ] })
  | _ -> raise (EvalError "applying non-function")

and apply (mc : MetaContext.t) (vf : value) (va : value) : value =
  result_value mc (apply_result mc vf va)

and apply_ty (mc : MetaContext.t) (ty : value) (va : value) : value =
  match ty with
  | VPi { codomain = clo; _ } -> eval mc (va :: clo.env) clo.body
  | _ -> VU

and eval_effect_row_closure (mc : MetaContext.t) (row : effect_row_closure)
    (binder : value) : effect_row_value =
  let env = binder :: row.env in
  let row_value =
    { effect_values = List.map (eval mc env) row.effects;
      tail_value = Option.map (eval mc env) row.tail }
  in
  normalize_effect_row_value mc row_value

and normalize_effect_row_value (mc : MetaContext.t) (row : effect_row_value) : effect_row_value =
  match Option.map (force mc) row.tail_value with
  | Some (VEffectRow tail_row) ->
      let tail_row = normalize_effect_row_value mc tail_row in
      { effect_values = row.effect_values @ tail_row.effect_values;
        tail_value = tail_row.tail_value }
  | tail_value -> { row with tail_value }

and eval_if (mc : MetaContext.t) (env : env) (vc : value) (then_ : term)
    (else_ : term) : result =
  match vc with
  | VAtom (Bool true) -> eval_result mc env then_
  | VAtom (Bool false) -> eval_result mc env else_
  | _ ->
      let head, base_frames = stuck_head_frames vc in
      let if_frame =
        FIf { then_ = { env; body = then_ }; else_ = { env; body = else_ } }
      in
      Done
        (VNeutral
           { ty = VU; neutral = { head; frames = base_frames @ [ if_frame ] } })

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
  | exception Invalid_argument _ -> VFlex { id; spine = [] }

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

and eval_eff (env : env) (name : string) : value =
  let rec go = function
    | [] -> raise (EvalError ("unbound eff: " ^ name))
    | VEffect e :: _ when String.equal e.name name -> VEffect e
    | _ :: rest -> go rest
  in
  go env

and eval_match_result (mc : MetaContext.t) (env : env) (scrutinee : term)
    (branches : match_branch list) : result =
  let value_branches, effect_branches = close_match_branches env branches in
  let rec handle_scrutinee = function
    | Done v ->
        if List.is_empty effect_branches then
          Done (eval_match mc env v value_branches)
        else handle_body (eval_match_result_value mc env v value_branches)
    | Effect request -> handle_effect handle_scrutinee request
  and handle_body = function
    | Done v -> Done v
    | Effect request -> handle_effect handle_body request
  and handle_effect resume_with request =
    match
      find_effect_branch mc effect_branches request.eff request.op request.arg
    with
    | Some (arg_bindings, body) ->
        let k = make_cont request.k in
        handle_body
          (eval_result mc
             (k :: List.rev_append arg_bindings body.env)
             body.body)
    | None ->
        Effect
          { request with k = (fun resume -> resume_with (request.k resume)) }
  in
  handle_scrutinee (eval_result mc env scrutinee)

and close_match_branches env branches =
  let value_branches, effect_branches =
    List.fold_right
      (fun branch (values, effects) ->
        match branch with
        | ValueBranch (pat, body) -> ((pat, body) :: values, effects)
        | EffectBranch { eff; op; arg_pat; body } ->
            (values, (eff, op, arg_pat, { env; body }) :: effects))
      branches ([], [])
  in
  (value_branches, effect_branches)

and find_effect_branch mc branches eff op arg =
  List.find_map
    (fun (branch_eff, branch_op, arg_pat, body) ->
      let branch_eff = force mc branch_eff in
      if String.equal op branch_op && runtime_value_equal mc eff branch_eff then
        Option.map
          (fun bindings -> (bindings, body))
          (match_core_pat mc arg_pat arg)
      else None)
    branches

and runtime_value_equal mc lhs rhs =
  match (force mc lhs, force mc rhs) with
  | VEffect e1, VEffect e2 ->
      e1.id = e2.id
      && List.length e1.params = List.length e2.params
      && List.for_all2 (runtime_value_equal mc) e1.params e2.params
  | VNominal n1, VNominal n2 ->
      n1.id = n2.id
      && List.length n1.params = List.length n2.params
      && List.for_all2 (runtime_value_equal mc) n1.params n2.params
  | VAtom a, VAtom b -> Atom.equal a b
  | VAtomTy a, VAtomTy b -> Atom_ty.equal a b
  | VU, VU -> true
  | _ -> false

and core_pat_contains_struct_type = function
  | CPatStructType _ -> true
  | CPatProd pats -> List.exists core_pat_contains_struct_type pats
  | CPatOr (lhs, rhs) ->
      core_pat_contains_struct_type lhs || core_pat_contains_struct_type rhs
  | CPatRecord { fields; _ } ->
      List.exists (fun (_, pat) -> core_pat_contains_struct_type pat) fields
  | CPatCon (_, _, pats) -> List.exists core_pat_contains_struct_type pats
  | CPatNominalHead { param_pats; _ } ->
      List.exists core_pat_contains_struct_type param_pats
  | CPatWild | CPatBind | CPatAtom _ | CPatType _ | CPatSyn _ -> false

and core_pat_contains_nominal_head = function
  | CPatNominalHead _ -> true
  | CPatProd pats -> List.exists core_pat_contains_nominal_head pats
  | CPatOr (lhs, rhs) ->
      core_pat_contains_nominal_head lhs || core_pat_contains_nominal_head rhs
  | CPatRecord { fields; _ } ->
      List.exists (fun (_, pat) -> core_pat_contains_nominal_head pat) fields
  | CPatCon (_, _, pats) -> List.exists core_pat_contains_nominal_head pats
  | CPatStructType { fields; _ } ->
      List.exists (fun (_, pat) -> core_pat_contains_nominal_head pat) fields
  | CPatWild | CPatBind | CPatAtom _ | CPatType _ | CPatSyn _ -> false

and struct_type_fields fields =
  List.filter_map
    (fun (name, kind, ty) -> if kind = Field then Some (name, ty) else None)
    fields

and match_core_pat mc pat value =
  match (pat, force mc value) with
  | CPatWild, _ -> Some []
  | CPatBind, v -> Some [ v ]
  | CPatAtom expected, VAtom actual when Atom.equal expected actual -> Some []
  | CPatType expected, VAtomTy actual when Atom_ty.equal expected actual ->
      Some []
  | CPatProd pats, VProd values when List.length pats = List.length values ->
      match_core_pats mc pats values
  | CPatSyn { rhs; _ }, v -> match_core_pat mc rhs v
  | CPatCon (name, num_type_params, sub_pats), VCon { name = actual; spine; _ }
    when String.equal name actual ->
      let payload = List.drop num_type_params spine in
      if List.length sub_pats = List.length payload then
        match_core_pats mc sub_pats payload
      else None
  | CPatNominalHead { id; param_pats; _ }, VNominal n when n.id = id ->
      if List.length param_pats = List.length n.params then
        match_core_pats mc param_pats n.params
      else None
  | CPatRecord { fields; _ }, VRecord { fields = values; _ } ->
      let rec go acc = function
        | [] -> Some (List.rev acc)
        | (name, pat) :: rest -> (
            match List.assoc_opt name values with
            | Some value -> (
                match match_core_pat mc pat value with
                | Some bindings -> go (List.rev_append bindings acc) rest
                | None -> None)
            | None -> None)
      in
      go [] fields
  | CPatStructType { fields; partial }, VStruct { entries = struct_entries; _ }
    ->
      let struct_fields =
        struct_type_fields (struct_entry_fields struct_entries)
      in
      if (not partial) && List.length fields <> List.length struct_fields then
        None
      else
        let rec go acc = function
          | [] -> Some (List.rev acc)
          | (name, pat) :: rest -> (
              match List.assoc_opt name struct_fields with
              | Some field_ty -> (
                  match match_core_pat mc pat field_ty with
                  | Some bindings -> go (List.rev_append bindings acc) rest
                  | None -> None)
              | None -> None)
        in
        go [] fields
  | CPatOr (lhs, rhs), v -> (
      match match_core_pat mc lhs v with
      | Some _ as matched -> matched
      | None -> match_core_pat mc rhs v)
  | _ -> None

and match_core_pats mc pats values =
  let rec go acc pats values =
    match (pats, values) with
    | [], [] -> Some (List.rev acc)
    | pat :: pats, value :: values -> (
        match match_core_pat mc pat value with
        | Some bindings -> go (List.rev_append bindings acc) pats values
        | None -> None)
    | _ -> None
  in
  go [] pats values

(* Pattern matching: compile to decision tree, then interpret.
   For a stuck scrutinee, accumulate an FMatch frame. *)
and eval_match_result_value (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : result =
  let scrutinee = force mc scrutinee in
  if
    List.exists
      (fun (pat, _) ->
        core_pat_contains_struct_type pat || core_pat_contains_nominal_head pat)
      branches
  then eval_match_direct_result mc env scrutinee branches
  else
    match scrutinee with
    | VCon _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VCon { nominal; _ }) ->
              Core_match_compile.Nominal (nominal_constructors mc nominal)
          | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let pats = List.map fst branches in
        let dt =
          Core_match_compile.compile_with_domains ~domain_of_occurrence pats
        in
        eval_decision_tree_result mc env scrutinee branches dt
    | VAtom atom ->
        let domain = Core_match_compile.Atom (atom_ty_of_atom atom) in
        let pats = List.map fst branches in
        let dt = Core_match_compile.compile ~domain pats in
        eval_decision_tree_result mc env scrutinee branches dt
    | VAtomTy _ | VNominal _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
          | Some (VAtomTy _) | Some (VNominal _) -> Type
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let pats = List.map fst branches in
        let dt =
          Core_match_compile.compile_with_domains ~domain_of_occurrence pats
        in
        eval_decision_tree_result mc env scrutinee branches dt
    | VProd _ | VRecord _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VCon { nominal; _ }) ->
              Core_match_compile.Nominal (nominal_constructors mc nominal)
          | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let pats = List.map fst branches in
        let dt =
          Core_match_compile.compile_with_domains ~domain_of_occurrence pats
        in
        eval_decision_tree_result mc env scrutinee branches dt
    | _ ->
        let head, base_frames = stuck_head_frames scrutinee in
        Done
          (VNeutral
             {
               ty = VU;
               neutral =
                 {
                   head;
                   frames =
                     base_frames
                     @ [
                         FMatch
                           (List.map
                              (fun (p, body) -> (p, { env; body }))
                              branches);
                       ];
                 };
             })

and eval_match (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : value =
  let scrutinee = force mc scrutinee in
  if List.exists (fun (pat, _) -> core_pat_contains_struct_type pat) branches
  then
    match branches with
    | [] -> raise (EvalError "non-exhaustive match at runtime")
    | _ -> eval_match_direct mc env scrutinee branches
  else
    match scrutinee with
    | VCon _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VCon { nominal; _ }) ->
              Core_match_compile.Nominal (nominal_constructors mc nominal)
          | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let pats = List.map fst branches in
        let dt =
          Core_match_compile.compile_with_domains ~domain_of_occurrence pats
        in
        eval_decision_tree mc env scrutinee branches dt
    | VAtom atom ->
        let domain = Core_match_compile.Atom (atom_ty_of_atom atom) in
        let pats = List.map fst branches in
        let dt = Core_match_compile.compile ~domain pats in
        eval_decision_tree mc env scrutinee branches dt
    | VAtomTy _ | VNominal _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
          | Some (VAtomTy _) | Some (VNominal _) -> Type
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let pats = List.map fst branches in
        let dt =
          Core_match_compile.compile_with_domains ~domain_of_occurrence pats
        in
        eval_decision_tree mc env scrutinee branches dt
    | VProd _ | VRecord _ ->
        let domain_of_occurrence occ =
          match resolve_occurrence_opt mc scrutinee occ with
          | Some (VCon { nominal; _ }) ->
              Core_match_compile.Nominal (nominal_constructors mc nominal)
          | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
          | Some (VProd elems) -> Product (List.length elems)
          | Some (VRecord { typ = VStruct { entries; _ }; _ }) ->
              Record
                (List.filter_map
                   (fun (n, k, _) -> if k = Field then Some n else None)
                   (struct_entry_fields entries))
          | _ -> Unknown
        in
        let pats = List.map fst branches in
        let dt =
          Core_match_compile.compile_with_domains ~domain_of_occurrence pats
        in
        eval_decision_tree mc env scrutinee branches dt
    | _ ->
        let head, base_frames = stuck_head_frames scrutinee in
        VNeutral
          {
            ty = VU;
            neutral =
              {
                head;
                frames =
                  base_frames
                  @ [
                      FMatch
                        (List.map
                           (fun (p, body) -> (p, { env; body }))
                           branches);
                    ];
              };
          }

and eval_match_direct (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : value =
  result_value mc (eval_match_direct_result mc env scrutinee branches)

and eval_match_direct_result (mc : MetaContext.t) (env : env)
    (scrutinee : value) (branches : (core_pat * term) list) : result =
  match branches with
  | [] -> raise (EvalError "non-exhaustive match at runtime")
  | (pat, body) :: rest -> (
      match match_core_pat mc pat scrutinee with
      | Some bindings -> eval_result mc (List.rev_append bindings env) body
      | None -> eval_match_direct_result mc env scrutinee rest)

and nominal_constructors (mc : MetaContext.t) (nom : value) :
    (string * int * int) list =
  match force mc nom with
  | VNominal { params; constructors; _ } ->
      let ntp = List.length params in
      List.map
        (fun (name, payloads) -> (name, ntp, List.length payloads))
        constructors
  | _ -> raise (EvalError "match scrutinee type is not a nominal")

and eval_decision_tree (mc : MetaContext.t) (env : env) (root : value)
    (branches : (core_pat * term) list) (dt : Core_decision_tree.t) : value =
  result_value mc (eval_decision_tree_result mc env root branches dt)

and eval_decision_tree_result (mc : MetaContext.t) (env : env) (root : value)
    (branches : (core_pat * term) list) (dt : Core_decision_tree.t) : result =
  match dt.content with
  | Leaf { branch; bindings } ->
      let env' =
        List.fold_left
          (fun e occ -> resolve_occurrence mc root occ :: e)
          env bindings
      in
      let _, body = List.nth branches branch in
      eval_result mc env' body
  | Destruct { occurrence; cases; default } -> (
      let v = resolve_occurrence mc root occurrence |> force mc in
      match v with
      | VCon { name; _ } -> (
          match
            List.find_opt (fun (cn, _, _) -> String.equal cn name) cases
          with
          | Some (_, _, sub) ->
              eval_decision_tree_result mc env root branches sub
          | None -> (
              match default with
              | Some d -> eval_decision_tree_result mc env root branches d
              | None -> raise (EvalError "non-exhaustive match at runtime")))
      | _ -> raise (EvalError "match on non-constructor value"))
  | Switch { cases; default; occurrence } -> (
      let v = resolve_occurrence mc root occurrence |> force mc in
      let key =
        match v with
        | VAtom atom -> Core_decision_tree.KAtom atom
        | VAtomTy atom_ty -> KType atom_ty
        | VNominal n -> KNominal n.id
        | _ -> raise (EvalError "switch on non-constant value")
      in
      match
        List.find_opt
          (fun (case_key, _) ->
            Core_decision_tree.switch_key_equal case_key key)
          cases
      with
      | Some (_, sub) -> eval_decision_tree_result mc env root branches sub
      | None -> eval_decision_tree_result mc env root branches default)

and resolve_occurrence (mc : MetaContext.t) (root : value)
    (occ : Core_decision_tree.occurrence) : value =
  match resolve_occurrence_opt mc root occ with
  | Some v -> v
  | None -> raise (EvalError "resolve_occurrence: invalid occurrence")

and resolve_occurrence_opt (mc : MetaContext.t) (root : value)
    (occ : Core_decision_tree.occurrence) : value option =
  match occ with
  | OBase -> Some root
  | OChild { parent; index } -> (
      match Option.map (force mc) (resolve_occurrence_opt mc root parent) with
      | Some (VCon { spine; _ }) -> List.nth_opt spine index
      | Some (VNominal { params; _ }) -> List.nth_opt params index
      | Some (VProd elems) -> List.nth_opt elems index
      | _ -> None)
  | OField { parent; name } -> (
      match Option.map (force mc) (resolve_occurrence_opt mc root parent) with
      | Some (VRecord { fields; _ }) -> List.assoc_opt name fields
      | _ -> None)

and force (mc : MetaContext.t) (v : value) : value =
  match v with
  | VFlex { id; spine = sp } -> (
      match MetaContext.lookup mc id with
      | Solved v ->
          let applied = List.fold_left (fun f a -> apply mc f a) v sp in
          force mc applied
      | Unsolved -> VFlex { id; spine = sp }
      | exception Invalid_argument _ -> VFlex { id; spine = sp })
  | _ -> v

let quote_ops : Nbe_quote.ops =
  { force;
    closure_apply;
    eval_effect_row_closure;
    eval;
    apply }

let lvl_to_ix = Nbe_quote.lvl_to_ix
let conv_pat = Nbe_quote.conv_pat
let quote mc depth value = Nbe_quote.quote quote_ops mc depth value
let conv mc depth lhs rhs = Nbe_quote.conv quote_ops mc depth lhs rhs
