open Core

exception EvalError of string

type result =
  | Done of value
  | Effect of {
      eff : value;
      op : string;
      arg : value;
      k : value -> result;
    }

type continuation = { mutable used : bool; resume : value -> result }

let make_cont resume = VCont (Obj.repr { used = false; resume })
let get_cont obj : continuation = Obj.obj obj

let rec bind_result result f =
  match result with
  | Done v -> f v
  | Effect e -> Effect { e with k = (fun v -> bind_result (e.k v) f) }

module Prim = struct
  open Atom

  type reducer = t list -> t option

  let i64_binop (f : int64 -> int64 -> int64) : reducer = function
    | [ I64 a; I64 b ] -> Some (I64 (f a b))
    | _ -> None

  let i64_cmp (f : int64 -> int64 -> bool) : reducer = function
    | [ I64 a; I64 b ] -> Some (Bool (f a b))
    | _ -> None

  let bool_unop (f : bool -> bool) : reducer = function
    | [ Bool b ] -> Some (Bool (f b))
    | _ -> None

  let bool_cmp (f : bool -> bool -> bool) : reducer = function
    | [ Bool a; Bool b ] -> Some (Bool (f a b))
    | _ -> None

  let char_cmp (f : char -> char -> bool) : reducer = function
    | [ Char a; Char b ] -> Some (Bool (f a b))
    | _ -> None

  let unit_cmp (f : unit -> unit -> bool) : reducer = function
    | [ Unit; Unit ] -> Some (Bool (f () ()))
    | _ -> None

  let string_cmp (f : string -> string -> bool) : reducer = function
    | [ String a; String b ] -> Some (Bool (f a b))
    | _ -> None
end

let atom_ty_of_atom = function
  | Atom.I64 _ -> TI64
  | Bool _ -> TBool
  | Unit -> TUnit
  | Char _ -> TChar
  | String _ -> TString

let prim_table : (string, Prim.reducer) Hashtbl.t =
  let open Prim in
  [
    "+",  i64_binop Int64.add;
    "-",  i64_binop Int64.sub;
    "*",  i64_binop Int64.mul;
    "/",  i64_binop Int64.div;
    "%",  i64_binop Int64.rem;
    "eq_i64", i64_cmp Int64.equal;
    "neq_i64", i64_cmp (fun a b -> not (Int64.equal a b));
    "eq_bool", bool_cmp Bool.equal;
    "neq_bool", bool_cmp (fun a b -> not (Bool.equal a b));
    "eq_char", char_cmp Char.equal;
    "neq_char", char_cmp (fun a b -> not (Char.equal a b));
    "eq_unit", unit_cmp (fun () () -> true);
    "neq_unit", unit_cmp (fun () () -> false);
    "eq_string", string_cmp String.equal;
    "neq_string", string_cmp (fun a b -> not (String.equal a b));
    "<",  i64_cmp (fun a b -> Int64.compare a b < 0);
    ">",  i64_cmp (fun a b -> Int64.compare a b > 0);
    "<=", i64_cmp (fun a b -> Int64.compare a b <= 0);
    ">=", i64_cmp (fun a b -> Int64.compare a b >= 0);
    "not", bool_unop not;
  ]
  |> List.to_seq |> Hashtbl.of_seq

let visible_kind = function
  | Private | PrivateMethod -> false
  | Field | Public | Method -> true

let dot_value (value : value) (name : string) : value =
  match value with
  | VModule { fields; partial = _ } ->
      validate_module_fields fields;
      (match List.find_opt (fun (n, k, _) -> String.equal n name && visible_kind k) fields with
      | Some (_, _, v) -> v
      | None -> raise (EvalError "field not found"))
  | VStruct { fields; _ } -> (
      match List.find_opt (fun (n, k, _) -> String.equal n name && visible_kind k) fields with
      | Some (_, _, v) -> v
      | None -> raise (EvalError "field not found"))
  | VRecord { fields; _ } -> (
      match List.find_opt (fun (n, _) -> String.equal n name) fields with
      | Some (_, v) -> v
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
  | _ -> raise (EvalError "field access on non-struct")

let unhandled_effect_error eff op =
  match eff with
  | VEffect e -> raise (EvalError ("unhandled effect " ^ e.name ^ "." ^ op ^ "; handlers are not implemented"))
  | _ -> raise (EvalError "perform target is not an effect")

let result_value _mc = function
  | Done v -> v
  | Effect { eff; op; _ } -> unhandled_effect_error eff op

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
      bind_result (eval_result mc env def) (fun vdef -> eval_result mc (vdef :: env) body)
  | Pi { explicitness; domain; effects; codomain } ->
      Done
        (VPi
           { explicitness;
             domain = eval mc env domain;
             effects = effect_row_closure env effects;
             codomain = { env; body = codomain } })
  | U -> Done VU
  | Atom a -> Done (VAtom a)
  | AtomTy t -> Done (VAtomTy t)
  | If (cond, then_, else_) ->
      bind_result (eval_result mc env cond) (fun vc -> eval_if mc env vc then_ else_)
  | Prod elems -> sequence_values mc env elems (fun values -> Done (VProd values))
  | ProdTy elems -> sequence_values mc env elems (fun values -> Done (VProdTy values))
  | Module { bindings } ->
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
        | EffectBind (name, kind, eff) :: rest ->
            eval_binds (eff :: env) ((name, kind, eff) :: acc) rest
      in
      let _env, fields = eval_binds env [] bindings in
      Done (VModule { fields; partial = false })
  | Struct { con_fields; bindings; partial } ->
      (* con_fields: all at same scope, no sequential dependency *)
      let con_vals =
        List.map (fun (name, ty) -> (name, Field, eval mc env ty)) con_fields
      in
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
        | EffectBind (name, kind, eff) :: rest ->
            eval_binds (eff :: env) ((name, kind, eff) :: acc) rest
      in
      let _env, bind_vals = eval_binds env [] bindings in
      Done (VStruct { fields = con_vals @ bind_vals; partial })
  | RecordConstruct { typ; fields } ->
      bind_result (eval_result mc env typ) (fun typ ->
        let rec go acc = function
          | [] -> Done (VRecord { typ; fields = List.rev acc })
          | (name, value) :: rest ->
              bind_result (eval_result mc env value) (fun value -> go ((name, value) :: acc) rest)
        in
        go [] fields)
  | Proj (e, i) ->
      bind_result (eval_result mc env e) (fun vs ->
        Done
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
          | _ -> raise (EvalError "projection of non-product")))
  | Dot (e, name) -> bind_result (eval_result mc env e) (fun value -> Done (dot_value value name))
  | Open (s, body) ->
      bind_result (eval_result mc env s) (fun vs ->
        match vs with
        | VModule { fields; partial = _ } ->
            let vals = List.filter_map (fun (_, k, v) -> if k = Public || k = Method then Some v else None) fields in
            let env' = List.fold_left (fun e v -> v :: e) env vals in
            eval_result mc env' body
        | _ -> raise (EvalError "open of non-module"))
  | Fix body -> Done (VFix { body = { env; body } })
  | Con name -> Done (eval_con env name)
  | NomRef (name, params) ->
      (match eval_con env name with
      | VNominal _ as nom ->
          sequence_values mc env params (fun param_vals ->
            Done (List.fold_left (fun acc v -> apply mc acc v) nom param_vals))
      | _ -> raise (EvalError ("NomRef is not VNominal: " ^ name)))
  | EffectRef (name, params) ->
      (match eval_eff env name with
      | VEffect _ as eff ->
          sequence_values mc env params (fun param_vals ->
            Done (List.fold_left (fun acc v -> apply mc acc v) eff param_vals))
      | _ -> raise (EvalError ("EffectRef is not VEffect: " ^ name)))
  | Ctor { name; spine; nominal_name; nominal_spine } ->
      sequence_values mc env spine (fun spine_vals ->
        sequence_values mc env nominal_spine (fun nom_spine_vals ->
          match eval_con env nominal_name with
          | VNominal n ->
              Done
                (VCon
                   { name;
                     spine = spine_vals;
                     nominal = VNominal { n with params = nom_spine_vals } })
          | _ -> raise (EvalError ("Ctor nominal is not VNominal: " ^ nominal_name))))
  | Prim name ->
      Done (VNeutral { ty = VU; neutral = { head = HPrim name; frames = [] } })
  | Meta id -> Done (eval_meta mc id)
  | InsertedMeta (id, bds) -> Done (eval_inserted_meta mc env id bds)
  | NominalDef { id; name; num_params; ctors; body } ->
      let elaborated_ctors =
        List.map (fun (cname, payload_opt) ->
          (cname, Option.map (fun t -> { env; body = t }) payload_opt))
        ctors
      in
      let nominal = VNominal {
        id; name; num_params; params = [];
        constructors = elaborated_ctors
      } in
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
      let env =
        if num_params > 0 then nominal :: env
        else env
      in
      let env =
        List.fold_left (fun env (cname, payload_clo_opt) ->
          let has_payload = Option.is_some payload_clo_opt in
          let total_args = num_params + (if has_payload then 1 else 0) in
          if total_args = 0 then
            VCon { name = cname; spine = []; nominal } :: env
          else
            let ctor_body =
              let param_vars = List.init num_params (fun i ->
                Var (total_args - 1 - i)) in
              let payload_var = if has_payload then [ Var 0 ] else [] in
              Ctor { name = cname; spine = param_vars @ payload_var;
                     nominal_name = name;
                     nominal_spine = param_vars }
            in
            let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
            let ctor_term = wrap total_args ctor_body in
            eval mc env ctor_term :: env)
        env elaborated_ctors
      in
      eval_result mc env body
  | EffectDef { id; name; ops; body; _ } ->
      let elaborated_ops =
        List.map (fun (op_name, input, output) ->
          (op_name, { env; body = input }, { env; body = output }))
          ops
      in
      let eff = VEffect { id; name; params = []; operations = elaborated_ops } in
      eval_result mc (eff :: env) body
  | Match (scrut, branches) ->
      eval_match_result mc env (eval_result mc env scrut) branches
  | Perform { eff; op; arg } ->
      bind_result (eval_result mc env eff) (fun eff ->
        bind_result (eval_result mc env arg) (fun arg ->
          match force mc eff with
          | VEffect _ as eff -> Effect { eff; op; arg; k = (fun v -> Done v) }
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
      let atoms = List.filter_map (function FApp (VAtom a) -> Some a | _ -> None) frames in
      if List.length atoms <> List.length frames then None
      else
        match Hashtbl.find_opt prim_table name with
        | Some f -> Option.map (fun a -> VAtom a) (f atoms)
        | None -> None)
  | _ -> None

and apply_result (mc : MetaContext.t) (vf : value) (va : value) : result =
  match vf with
  | VLam { body = clo; _ } -> eval_result mc (va :: clo.env) clo.body
  | VFix { body = clo; _ } ->
      let self = VFix { body = clo } in
      let unfolded = eval mc (self :: clo.env) clo.body in
      apply_result mc unfolded va
  | VCont obj ->
      let cont = get_cont obj in
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
  | VCon c -> Done (VCon { c with spine = c.spine @ [ va ] })
  | _ -> raise (EvalError "applying non-function")

and apply (mc : MetaContext.t) (vf : value) (va : value) : value =
  result_value mc (apply_result mc vf va)

and apply_ty (mc : MetaContext.t) (ty : value) (va : value) : value =
  match ty with
  | VPi { codomain = clo; _ } -> eval mc (va :: clo.env) clo.body
  | _ -> VU

and eval_effect_row_closure (mc : MetaContext.t) (row : effect_row_closure)
    (binder : value) : value list =
  match row.tail with
  | Some _ -> raise (EvalError "open effect rows are not implemented")
  | None -> List.map (eval mc (binder :: row.env)) row.effects

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

and eval_match_result (mc : MetaContext.t) (env : env) (scrutinee : result)
    (branches : match_branch list) : result =
  let value_branches, effect_branches = close_match_branches mc env branches in
  let rec handle = function
    | Done v -> Done (eval_match mc env v value_branches)
    | Effect e -> (
        match find_effect_branch mc effect_branches e.eff e.op e.arg with
        | Some (arg_bindings, body) ->
            let k = make_cont e.k in
            handle (eval_result mc (k :: arg_bindings @ body.env) body.body)
        | None ->
            Effect { e with k = (fun resume -> handle (e.k resume)) })
  in
  handle scrutinee

and close_match_branches mc env branches =
  let value_branches, effect_branches =
    List.fold_right
      (fun branch (values, effects) ->
        match branch with
        | ValueBranch (pat, body) -> ((pat, body) :: values, effects)
        | EffectBranch { eff; op; arg_pat; body } ->
            (values, (eval mc env eff, op, arg_pat, { env; body }) :: effects))
      branches ([], [])
  in
  (value_branches, effect_branches)

and find_effect_branch mc branches eff op arg =
  List.find_map
    (fun (branch_eff, branch_op, arg_pat, body) ->
      let branch_eff = force mc branch_eff in
      if String.equal op branch_op && runtime_value_equal mc eff branch_eff then
        Option.map (fun bindings -> (bindings, body)) (match_core_pat mc arg_pat arg)
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
  | VAtomTy a, VAtomTy b -> equal_atom_ty a b
  | VU, VU -> true
  | _ -> false

and core_pat_contains_struct_type = function
  | CPatStructType _ -> true
  | CPatProd pats -> List.exists core_pat_contains_struct_type pats
  | CPatOr (lhs, rhs) -> core_pat_contains_struct_type lhs || core_pat_contains_struct_type rhs
  | CPatRecord { fields; _ } -> List.exists (fun (_, pat) -> core_pat_contains_struct_type pat) fields
  | CPatCon (_, _, pats) -> List.exists core_pat_contains_struct_type pats
  | CPatNominalHead { param_pats; _ } -> List.exists core_pat_contains_struct_type param_pats
  | CPatWild | CPatBind | CPatAtom _ | CPatType _ -> false

and struct_type_fields fields =
  List.filter_map (fun (name, kind, ty) -> if kind = Field then Some (name, ty) else None) fields

and match_core_pat mc pat value =
  match (pat, force mc value) with
  | CPatWild, _ -> Some []
  | CPatBind, v -> Some [ v ]
  | CPatAtom expected, VAtom actual when Atom.equal expected actual -> Some []
  | CPatType expected, VAtomTy actual when equal_atom_ty expected actual -> Some []
  | CPatProd pats, VProd values when List.length pats = List.length values ->
      match_core_pats mc pats values
  | CPatCon (name, num_type_params, sub_pats), VCon { name = actual; spine; _ }
    when String.equal name actual ->
      let payload = List.drop num_type_params spine in
      if List.length sub_pats = List.length payload then match_core_pats mc sub_pats payload else None
  | CPatNominalHead { id; param_pats; _ }, VNominal n when n.id = id ->
      if List.length param_pats = List.length n.params then match_core_pats mc param_pats n.params else None
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
  | CPatStructType { fields; partial }, VStruct { fields = struct_fields; _ } ->
      let struct_fields = struct_type_fields struct_fields in
      if (not partial) && List.length fields <> List.length struct_fields then None
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
      match match_core_pat mc lhs v with Some _ as matched -> matched | None -> match_core_pat mc rhs v)
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
and eval_match (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : value =
  let scrutinee = force mc scrutinee in
  if List.exists (fun (pat, _) -> core_pat_contains_struct_type pat) branches then
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
        | Some (VRecord { typ = VStruct { fields; _ }; _ }) ->
            Record (List.filter_map (fun (n, k, _) -> if k = Field then Some n else None) fields)
        | _ -> Unknown
      in
      let pats = List.map fst branches in
      let dt = Core_match_compile.compile_with_domains ~domain_of_occurrence pats in
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
        | Some (VRecord { typ = VStruct { fields; _ }; _ }) ->
            Record (List.filter_map (fun (n, k, _) -> if k = Field then Some n else None) fields)
        | _ -> Unknown
      in
      let pats = List.map fst branches in
      let dt = Core_match_compile.compile_with_domains ~domain_of_occurrence pats in
      eval_decision_tree mc env scrutinee branches dt
  | VProd _ | VRecord _ ->
      let domain_of_occurrence occ =
        match resolve_occurrence_opt mc scrutinee occ with
        | Some (VCon { nominal; _ }) ->
            Core_match_compile.Nominal (nominal_constructors mc nominal)
        | Some (VAtom atom) -> Core_match_compile.Atom (atom_ty_of_atom atom)
        | Some (VProd elems) -> Product (List.length elems)
        | Some (VRecord { typ = VStruct { fields; _ }; _ }) ->
            Record (List.filter_map (fun (n, k, _) -> if k = Field then Some n else None) fields)
        | _ -> Unknown
      in
      let pats = List.map fst branches in
      let dt = Core_match_compile.compile_with_domains ~domain_of_occurrence pats in
      eval_decision_tree mc env scrutinee branches dt
  | _ ->
      let head, base_frames = stuck_head_frames scrutinee in
      VNeutral
        { ty = VU;
          neutral = { head; frames = base_frames @ [ FMatch
              (List.map (fun (p, body) -> (p, { env; body })) branches) ] } }

and eval_match_direct (mc : MetaContext.t) (env : env) (scrutinee : value)
    (branches : (core_pat * term) list) : value =
  match branches with
  | [] -> raise (EvalError "non-exhaustive match at runtime")
  | (pat, body) :: rest -> (
      match match_core_pat mc pat scrutinee with
      | Some bindings -> eval mc (List.rev_append bindings env) body
      | None -> eval_match_direct mc env scrutinee rest)

and nominal_constructors (mc : MetaContext.t) (nom : value) :
    (string * int * bool) list =
  match force mc nom with
  | VNominal { params; constructors; _ } ->
      let ntp = List.length params in
      List.map (fun (name, payload) -> (name, ntp, Option.is_some payload))
        constructors
  | _ -> raise (EvalError "match scrutinee type is not a nominal")

and eval_decision_tree (mc : MetaContext.t) (env : env) (root : value)
    (branches : (core_pat * term) list)
    (dt : Core_decision_tree.t) : value =
  match dt.content with
  | Leaf { branch; bindings } ->
      let env' =
        List.fold_left
          (fun e occ -> resolve_occurrence mc root occ :: e)
          env bindings
      in
      let _, body = List.nth branches branch in
      eval mc env' body
  | Destruct { occurrence; cases; default } ->
      let v = resolve_occurrence mc root occurrence |> force mc in
      (match v with
      | VCon { name; _ } -> (
          match
            List.find_opt (fun (cn, _, _) -> String.equal cn name) cases
          with
          | Some (_, _, sub) ->
              eval_decision_tree mc env root branches sub
          | None -> (
              match default with
              | Some d -> eval_decision_tree mc env root branches d
              | None -> raise (EvalError "non-exhaustive match at runtime")))
      | _ -> raise (EvalError "match on non-constructor value"))
  | Switch { cases; default; occurrence } ->
      let v = resolve_occurrence mc root occurrence |> force mc in
      let key =
        match v with
        | VAtom atom -> Core_decision_tree.KAtom atom
        | VAtomTy atom_ty -> KType atom_ty
        | VNominal n -> KNominal n.id
        | _ -> raise (EvalError "switch on non-constant value")
      in
      (match List.find_opt (fun (case_key, _) -> Core_decision_tree.switch_key_equal case_key key) cases with
      | Some (_, sub) -> eval_decision_tree mc env root branches sub
      | None -> eval_decision_tree mc env root branches default)

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

and conv_pat (p1 : core_pat) (p2 : core_pat) : bool =
  match (p1, p2) with
  | CPatWild, CPatWild -> true
  | CPatBind, CPatBind -> true
  | CPatAtom a1, CPatAtom a2 -> Atom.equal a1 a2
  | CPatProd ps1, CPatProd ps2 ->
      List.length ps1 = List.length ps2 && List.for_all2 conv_pat ps1 ps2
  | CPatOr (l1, r1), CPatOr (l2, r2) ->
      conv_pat l1 l2 && conv_pat r1 r2
  | CPatRecord { fields = fs1; partial = p1 }, CPatRecord { fields = fs2; partial = p2 } ->
      p1 = p2
      && List.length fs1 = List.length fs2
      && List.for_all2
           (fun (n1, p1) (n2, p2) -> String.equal n1 n2 && conv_pat p1 p2)
           fs1 fs2
  | CPatStructType { fields = fs1; partial = p1 }, CPatStructType { fields = fs2; partial = p2 } ->
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
      id1 = id2 && n1 = n2 && List.length ps1 = List.length ps2 && List.for_all2 conv_pat ps1 ps2
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
  | VPi { explicitness; domain; effects; codomain } ->
      let var = VRigid { lvl = depth; spine = [] } in
      let row =
        { effects = List.map (quote mc (depth + 1)) (eval_effect_row_closure mc effects var);
          tail = None }
      in
      Pi
        { explicitness;
          domain = quote mc depth domain;
          effects = row;
          codomain = quote mc (depth + 1) (closure_apply mc codomain var) }
  | VU -> U
  | VAtom a -> Atom a
  | VAtomTy t -> AtomTy t
  | VProd elems -> Prod (List.map (quote mc depth) elems)
  | VProdTy elems -> ProdTy (List.map (quote mc depth) elems)
  | VFix { body = clo; _ } ->
      let var = VRigid { lvl = depth; spine = [] } in
      Fix (quote mc (depth + 1) (closure_apply mc clo var))
  | VModule { fields; partial = _ } ->
      let bindings =
        List.filter_map (fun (n, k, v) ->
          match k, force mc v with
          | Public, VEffect _ -> Some (EffectBind (n, Public, v))
          | Private, VEffect _ -> Some (EffectBind (n, Private, v))
          | Public, _ -> Some (LetBind (n, Public, quote mc depth v))
          | Private, _ -> Some (LetBind (n, Private, quote mc depth v))
          | Method, _ | PrivateMethod, _ | Field, _ ->
              validate_module_fields fields;
              failwith "unreachable") fields
      in
      Module { bindings }
  | VStruct { fields; partial } ->
      let con_fields =
        List.filter_map (fun (n, k, v) ->
          if k = Field then Some (n, quote mc depth v) else None) fields
      in
      let bindings =
        List.filter_map (fun (n, k, v) ->
          match k, force mc v with
          | Public, VEffect _ -> Some (EffectBind (n, Public, v))
          | Private, VEffect _ -> Some (EffectBind (n, Private, v))
          | Public, _ -> Some (LetBind (n, Public, quote mc depth v))
          | Private, _ -> Some (LetBind (n, Private, quote mc depth v))
          | Method, _ -> Some (LetBind (n, Method, quote mc depth v))
          | PrivateMethod, _ -> Some (LetBind (n, PrivateMethod, quote mc depth v))
          | _ -> None) fields
      in
      Struct { con_fields; bindings; partial }
  | VRecord { typ; fields } ->
      RecordConstruct
        { typ = quote mc depth typ;
          fields = List.map (fun (name, value) -> (name, quote mc depth value)) fields }
  | VNominal n ->
      if n.params = [] then Con n.name
      else NomRef (n.name, List.map (quote mc depth) n.params)
  | VEffect e -> EffectRef (e.name, List.map (quote mc depth) e.params)
  | VCon { name; spine; _ } ->
      quote_spine mc depth (Con name) spine
  | VCont _ -> raise (EvalError "cannot quote continuation")
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
  List.fold_left (fun acc v -> Ap (acc, Explicit, quote mc depth v)) head sp

and quote_frames (mc : MetaContext.t) (depth : lvl) (head : term)
    (frames : frame list) : term =
  List.fold_left
    (fun acc frame ->
      match frame with
      | FApp v -> Ap (acc, Explicit, quote mc depth v)
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
                (fun (p, clo) -> ValueBranch (p, quote mc depth (eval mc clo.env clo.body)))
                branches ))
    head frames

let rec conv (mc : MetaContext.t) (depth : lvl) (v1 : value) (v2 : value) : bool
    =
  let v1 = force mc v1 in
  let v2 = force mc v2 in
  match (v1, v2) with
  | VU, VU -> true
  | VAtom a1, VAtom a2 -> Atom.equal a1 a2
  | VAtomTy t1, VAtomTy t2 -> equal_atom_ty t1 t2
  | ( VPi { explicitness = e1; domain = a1; effects = effs1; codomain = clo1 },
      VPi { explicitness = e2; domain = a2; effects = effs2; codomain = clo2 } )
    when e1 = e2 ->
      conv mc depth a1 a2
      &&
      let var = VRigid { lvl = depth; spine = [] } in
      conv_effect_rows mc (depth + 1)
        (eval_effect_row_closure mc effs1 var)
        (eval_effect_row_closure mc effs2 var)
      && conv mc (depth + 1)
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
  | VModule { fields = fs1; partial = p1 }, VModule { fields = fs2; partial = p2 } ->
      validate_module_fields fs1;
      validate_module_fields fs2;
      let visible fs = List.filter (fun (_, k, _) -> visible_kind k) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      let conv_field (name, kind, ty) fields =
        match List.find_opt (fun (n, k, _) -> String.equal n name && k = kind) fields with
        | Some (_, _, other_ty) -> conv mc depth ty other_ty
        | None -> false
      in
      if (not p1) && (not p2) then
        List.length vs1 = List.length vs2 && List.for_all (fun field -> conv_field field vs2) vs1
      else
        let required, available = if p1 && not p2 then vs1, vs2 else if p2 && not p1 then vs2, vs1 else if List.length vs1 <= List.length vs2 then vs1, vs2 else vs2, vs1 in
        List.for_all (fun field -> conv_field field available) required
  | VStruct { fields = fs1; _ }, VStruct { fields = fs2; _ } ->
      let visible fs = List.filter (fun (_, k, _) -> visible_kind k) fs in
      let vs1 = visible fs1 and vs2 = visible fs2 in
      List.length vs1 = List.length vs2
      && List.for_all2
           (fun (n1, k1, v1) (n2, k2, v2) ->
             String.equal n1 n2 && k1 = k2 && conv mc depth v1 v2)
           vs1 vs2
  | VRecord r1, VRecord r2 ->
      conv mc depth r1.typ r2.typ
      && List.length r1.fields = List.length r2.fields
      && List.for_all2
           (fun (n1, v1) (n2, v2) -> String.equal n1 n2 && conv mc depth v1 v2)
           r1.fields r2.fields
  | VNominal n1, VNominal n2 ->
      n1.id = n2.id
      && List.length n1.params = List.length n2.params
      && List.for_all2 (conv mc depth) n1.params n2.params
  | VEffect e1, VEffect e2 ->
      e1.id = e2.id
      && List.length e1.params = List.length e2.params
      && List.for_all2 (conv mc depth) e1.params e2.params
  | VCon c1, VCon c2 ->
      String.equal c1.name c2.name
      && List.length c1.spine = List.length c2.spine
      && List.for_all2 (conv mc depth) c1.spine c2.spine
  | _ -> false

and conv_spine (mc : MetaContext.t) (depth : lvl) (sp1 : spine) (sp2 : spine) :
    bool =
  List.length sp1 = List.length sp2 && List.for_all2 (conv mc depth) sp1 sp2

and conv_effect_rows (mc : MetaContext.t) (depth : lvl) (row1 : value list)
    (row2 : value list) : bool =
  let rec remove_match eff = function
    | [] -> None
    | candidate :: rest when conv mc depth eff candidate -> Some rest
    | candidate :: rest -> Option.map (fun rest -> candidate :: rest) (remove_match eff rest)
  in
  List.length row1 = List.length row2
  && Option.is_some
       (List.fold_left
          (fun remaining eff -> Option.bind remaining (remove_match eff))
          (Some row2) row1)

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
