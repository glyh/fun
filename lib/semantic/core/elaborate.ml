open Core

(* Surface.explicitness and Core.explicitness are separate types with the same
   constructor names. This helper maps between them. *)
let expl_of_surface = function Surface.Explicit -> Explicit | Surface.Implicit -> Implicit

type elab_error =
  | UnboundVariable of string
  | ApplyingNonFunction
  | TupleLengthMismatch
  | NotANominalType
  | UnknownConstructor of string
  | PatternArityMismatch

exception ElabError of elab_error

module NameMap = Map.Make (String)

type name_entry = { level : lvl; ty : value }

(** Elaboration context: tracks de Bruijn environment, name→index mapping,
    metavariables, and which binders are [Bound] vs [Defined] for [InsertedMeta]. *)
module Ctx = struct
  type t = {
    env : env;
    types : value list;
    lvl : lvl;
    metas : MetaContext.t;
    bds : bd list;
    name_table : name_entry NameMap.t;
  }

  (** Create an empty elaboration context with a fresh meta store. *)
  let empty () : t =
    let metas = MetaContext.create () in
    { env = []; types = []; lvl = 0; metas; bds = []; name_table = NameMap.empty }

  (** Extend the context with a [Bound] binder (lambda/pi parameter). *)
  let bind (ctx : t) (name : string) (ty : value) : t =
    let var = VRigid { lvl = ctx.lvl; spine = [] } in
    {
      env = var :: ctx.env;
      types = ty :: ctx.types;
      lvl = ctx.lvl + 1;
      metas = ctx.metas;
      bds = Bound :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table;
    }

  (** Extend the context with a [Defined] binder (let/define). *)
  let define (ctx : t) (name : string) (ty : value) (v : value) : t =
    {
      env = v :: ctx.env;
      types = ty :: ctx.types;
      lvl = ctx.lvl + 1;
      metas = ctx.metas;
      bds = Defined :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table;
    }

  (** Resolve a user-written name to its de Bruijn index and type.
      Converts the stored de Bruijn level to an index using the current
      context depth: [index = depth - level - 1]. *)
  let lookup (ctx : t) (name : string) : ix * value =
    match NameMap.find_opt name ctx.name_table with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable name))

  (** Create a fresh metavariable, recording the current [bd] mask so
      [eval_inserted_meta] applies it only to [Bound] variables. *)
  let fresh_meta (ctx : t) : term =
    let id = MetaContext.fresh ctx.metas in
    InsertedMeta (id, ctx.bds)

  let raw_meta (ctx : t) : value =
    VFlex { id = MetaContext.fresh ctx.metas; spine = [] }

  let eval (ctx : t) (t : term) : value = Nbe.eval ctx.metas ctx.env t
  let quote (ctx : t) (v : value) : term = Nbe.quote ctx.metas ctx.lvl v

  let unify (ctx : t) (v1 : value) (v2 : value) : unit =
    Unify.unify ctx.metas ctx.lvl v1 v2
end

(** Build a closed [VPi] value (for primitive types). *)
let ( ^-> ) = fun lhs rhs -> VPi { explicitness = Explicit; domain = lhs; codomain = { env = []; body = rhs } }
(** Build a [Pi] term (for primitive type schemas). *)
let ( ^->> ) = fun lhs rhs -> Pi (Explicit, lhs, rhs)

let prims =
  let arithemetic = VAtomTy TI64 ^-> AtomTy TI64 ^->> AtomTy TI64 in
  let comparator = VAtomTy TI64 ^-> AtomTy TI64 ^->> AtomTy TBool in
  [
    ("+", arithemetic);
    ("-", arithemetic);
    ("*", arithemetic);
    ("/", arithemetic);
    ("%", arithemetic);
    ("==", comparator);
    ("!=", comparator);
    ("<", comparator);
    (">", comparator);
    ("<=", comparator);
    (">=", comparator);
    ("not", VAtomTy TBool ^-> AtomTy TBool);
  ]
  |> NameMap.of_list

(** Bidirectional type inference: given a surface expression, produce a
    core term and its type. *)
let rec infer (ctx : Ctx.t) (expr : Surface.t) : term * value =
  match expr with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy TI64)
  | Atom (Bool b) -> (Atom (Bool b), VAtomTy TBool)
  | Atom Unit -> (Atom Unit, VAtomTy TUnit)
  | Var name ->
      let ix, ty = Ctx.lookup ctx name in
      (Var ix, ty)
  | Ap (f, Surface.Explicit, a) -> infer_ap ctx f a
  | Ap (f, Surface.Implicit, a) -> infer_ap_implicit ctx f a
  | Let { name; type_; value; body } -> infer_let ctx name type_ value body
  | If { cond; then_; else_ } -> infer_if ctx cond then_ else_
  | Lam (param, body) -> infer_lam ctx param body
  | Annotated { inner; typ } ->
      let ty_core, ty_ty = infer ctx typ in
      Ctx.unify ctx ty_ty VU;
      let ty_val = Ctx.eval ctx ty_core in
      let core = check ctx inner ty_val in
      (core, ty_val)
  | Prod elems ->
      let cores_tys = List.map (infer ctx) elems in
      let cores = List.map fst cores_tys in
      let tys = List.map snd cores_tys in
      (Prod cores, VProdTy tys)
  | Arrow (expl, a, b) ->
      let a_core, a_ty = infer ctx a in
      Ctx.unify ctx a_ty VU;
      let a_val = Ctx.eval ctx a_core in
      let ctx' = Ctx.bind ctx "_" a_val in
      let b_core, b_ty = infer ctx' b in
      Ctx.unify ctx' b_ty VU;
      (Pi (expl_of_surface expl, a_core, b_core), VU)
  | FieldAccess (e, name) ->
      let e_core, e_ty = infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VStruct { fields; _ } ->
          (match List.find_opt (fun (n, k, _) -> String.equal n name && k <> Private) fields with
          | Some (_, _, field_ty) -> (Dot (e_core, name), field_ty)
          | None -> raise (ElabError (UnboundVariable name)))
      | VFlex _ | VRigid _ | VNeutral _ ->
          let result_ty = Ctx.raw_meta ctx in
          let constraint_ty =
            VStruct { fields = [ (name, Field, result_ty) ]; partial = true }
          in
          Ctx.unify ctx e_ty constraint_ty;
          (Dot (e_core, name), result_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Proj (e, i) ->
      let e_core, e_ty = infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VProdTy tys ->
          if i < 0 || i >= List.length tys then
            raise (ElabError TupleLengthMismatch);
          (Proj (e_core, i), List.nth tys i)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Struct { con_fields; bindings } ->
      let con_cores =
        List.map (fun (name, ty_expr) ->
          let ty_core, ty_ty = infer ctx ty_expr in
          Ctx.unify ctx ty_ty VU;
          (name, ty_core, Ctx.eval ctx ty_core))
        con_fields
      in
      let rec go ctx (acc_binds, acc_fields) = function
        | [] -> (List.rev acc_binds, List.rev acc_fields)
        | Surface.LetBinding { name; value; public } :: rest ->
            let val_core, val_ty = infer ctx value in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            let field = if public then [ (name, kind, val_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               field @ acc_fields)
              rest
        | Surface.TypeBinding { name; params = _; ctors; public } :: rest ->
            let ctor_names = List.map fst ctors in
            let ctors' = List.map (fun c -> (c, None)) ctor_names in
            let nominal = VNominal { id = NominalId.fresh (); name; params = []; constructors = ctors' } in
            let kind = if public then Public else Private in
            let ctx =
              List.fold_left
                (fun ctx ctor_name ->
                  Ctx.define ctx ctor_name nominal
                    (VCon { name = ctor_name; spine = []; nominal }))
                ctx ctor_names
            in
            let ctx = Ctx.define ctx name VU nominal in
            let ctor_values =
              List.map (fun c ->
                (c, VCon { name = c; spine = []; nominal }))
              ctor_names
            in
            let type_fields =
              (name, kind, VU)
              :: List.map (fun c -> (c, kind, nominal)) ctor_names
            in
            go ctx
              (TypeBind (name, kind, nominal, ctor_values) :: acc_binds,
               type_fields @ acc_fields)
              rest
      in
      let core_bindings, extra_fields = go ctx ([], []) bindings in
      let result_con_fields = List.map (fun (n, c, _) -> (n, c)) con_cores in
      let type_fields =
        List.map (fun (n, _, ty) -> (n, Field, ty)) con_cores
        @ extra_fields
      in
      (Struct { con_fields = result_con_fields; bindings = core_bindings; partial = false },
       VStruct { fields = type_fields; partial = false })
  | Open (name, body) ->
      let ix, ty = Ctx.lookup ctx name in
      (match Nbe.force ctx.metas ty with
      | VStruct { fields; _ } ->
          let ctx' =
            List.fold_left
              (fun c (fname, k, fty) ->
                if k = Public then Ctx.define c fname fty fty else c)
              ctx fields
          in
          let body_core, body_ty = infer ctx' body in
          (Open (Var ix, body_core), body_ty)
      | _ -> raise (ElabError (UnboundVariable name (* not a struct *))))
  | TypeDef { name; params; ctors; body } ->
      (* Create fresh metas for type params and bind them *)
      let param_metas = List.map (fun _ -> Ctx.raw_meta ctx) params in
      let body_ctx =
        List.fold_left2
          (fun ctx param_name meta_val ->
            Ctx.define ctx param_name VU meta_val)
          ctx params param_metas
      in
      (* Elaborate constructor payload types in body_ctx *)
      let elaborated_ctors =
        List.map (fun (cname, payload_opt) ->
          match payload_opt with
          | None -> (cname, None)
          | Some payload_expr ->
              (* Elaborate payload type in body_ctx (params + type name in scope) *)
              let payload_core, payload_ty = infer body_ctx payload_expr in
              Ctx.unify body_ctx payload_ty VU;
              (cname, Some (Ctx.eval body_ctx payload_core)))
        ctors
      in
      let nominal = VNominal { id = NominalId.fresh (); name; params = param_metas; constructors = elaborated_ctors } in
      let num_params = List.length param_metas in
      (* For parameterized types, build an Explicit VPi chain so Option I64 works.
         For nullary types, just bind with VU as before. *)
      let body_ctx =
        if num_params = 0 then
          Ctx.define body_ctx name VU nominal
        else begin
          (* Push VNominal first so NomRef evaluation can find it *)
          let body_ctx = { body_ctx with
            env = nominal :: body_ctx.env;
            types = VU :: body_ctx.types;
            lvl = body_ctx.lvl + 1;
            bds = Defined :: body_ctx.bds
          } in
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) param_metas in
          let type_body_term = NomRef (name, type_var_terms) in
          let type_core_term =
            List.fold_right (fun _ acc -> Lam acc) param_metas type_body_term
          in
          let type_val = Nbe.eval body_ctx.metas body_ctx.env type_core_term in
          let type_ty =
            let depth = List.length body_ctx.env in
            List.fold_right
              (fun _ acc ->
                VPi { explicitness = Explicit; domain = VU;
                      codomain = { env = body_ctx.env; body = Nbe.quote body_ctx.metas depth acc } })
              param_metas VU
          in
          Ctx.define body_ctx name type_ty type_val
        end
      in
      let env = body_ctx.env in
      let body_ctx =
        List.fold_left2
          (fun ctx (cname, _payload_surface) payload_val_opt ->
            let ctor_val, ctor_ty =
              build_ctor body_ctx.metas env name cname param_metas payload_val_opt in
            Ctx.define ctx cname ctor_ty ctor_val)
          body_ctx ctors (List.map snd elaborated_ctors)
      in
      infer body_ctx body
  | Match (scrutinee, branches) ->
      let scrut_core, scrut_ty = infer ctx scrutinee in
      (match Nbe.force ctx.metas scrut_ty with
      | VNominal _ ->
          let ret_ty = Ctx.raw_meta ctx in
          let branches' =
            List.map (fun (pat, body) ->
              let core_pat, ctx' = elaborate_pat ctx pat scrut_ty in
              let body_core = check ctx' body ret_ty in
              (core_pat, body_core))
              branches
          in
          (Match (scrut_core, branches'), ret_ty)
      | _ -> raise (ElabError NotANominalType))

(** Elaborate a surface pattern against a scrutinee type, producing a core
    pattern and extending the context with bound pattern variables. *)
and elaborate_pat (ctx : Ctx.t) (pat : Surface.pat) (scrutinee_ty : value)
    : core_pat * Ctx.t =
  match pat with
  | PatWild -> (CPatWild, ctx)
  | PatBind name -> (CPatBind, Ctx.bind ctx name scrutinee_ty)
  | PatCon (name, sub_pats) ->
      (match Nbe.force ctx.metas scrutinee_ty with
      | VNominal n ->
          (match List.find_opt (fun (cname, _) -> String.equal cname name) n.constructors with
          | Some (_, payload_opt) ->
              let num_type_params = List.length n.params in
              (match (sub_pats, payload_opt) with
              | [], None -> (CPatCon (name, num_type_params, []), ctx)
              | [sub_pat], Some payload_ty ->
                  let core_sub, ctx' = elaborate_pat ctx sub_pat payload_ty in
                  (CPatCon (name, num_type_params, [core_sub]), ctx')
              | _ ->
                  raise (ElabError PatternArityMismatch))
          | None -> raise (ElabError (UnknownConstructor name)))
      | _ -> raise (ElabError NotANominalType))

(** Application inference.
    Loops to insert fresh metas for implicit VPi domains before consuming
    the user's explicit argument. *)
and infer_ap (ctx : Ctx.t) (f : Surface.t) (a : Surface.t) : term * value =
  let f_core, f_ty = infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  (* Insert metas for leading implicit VPi layers *)
  let rec insert_implicits f_core f_ty =
    match f_ty with
    | VPi { explicitness = Implicit; domain = _; codomain = b_clo } ->
        let meta_core = Ctx.fresh_meta ctx in
        let meta_val = Ctx.eval ctx meta_core in
        let ret_ty = Nbe.closure_apply ctx.metas b_clo meta_val in
        insert_implicits (Ap (f_core, Implicit, meta_core)) ret_ty
    | _ -> (f_core, f_ty)
  in
  let f_core, f_ty = insert_implicits f_core f_ty in
  match f_ty with
  | VPi { explicitness = Explicit; domain = a_ty; codomain = b_clo } ->
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_ty = Nbe.closure_apply ctx.metas b_clo a_val in
      (Ap (f_core, Explicit, a_core), ret_ty)
  | VFlex _ | VRigid _ | VNeutral _ ->
      let a_ty = Ctx.raw_meta ctx in
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_meta = Ctx.raw_meta (Ctx.bind ctx "_" a_ty) in
      let expected_f_ty =
        VPi
          { explicitness = Explicit;
            domain = a_ty;
            codomain = { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta } }
      in
      Ctx.unify ctx f_ty expected_f_ty;
      let ret_ty =
        Nbe.closure_apply ctx.metas
          { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta }
          a_val
      in
      (Ap (f_core, Explicit, a_core), ret_ty)
  | _ -> raise (ElabError ApplyingNonFunction)

(** Explicit implicit application ([f {arg}]). *)
and infer_ap_implicit (ctx : Ctx.t) (f : Surface.t) (a : Surface.t) : term * value =
  let f_core, f_ty = infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  match f_ty with
  | VPi { explicitness = Implicit; domain = a_ty; codomain = b_clo } ->
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_ty = Nbe.closure_apply ctx.metas b_clo a_val in
      (Ap (f_core, Implicit, a_core), ret_ty)
  | VPi { explicitness = Explicit; _ } ->
      raise (ElabError ApplyingNonFunction)
  | VFlex _ | VRigid _ | VNeutral _ ->
      let a_ty = Ctx.raw_meta ctx in
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_meta = Ctx.raw_meta (Ctx.bind ctx "_" a_ty) in
      let expected_f_ty =
        VPi
          { explicitness = Implicit;
            domain = a_ty;
            codomain = { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta } }
      in
      Ctx.unify ctx f_ty expected_f_ty;
      let ret_ty =
        Nbe.closure_apply ctx.metas
          { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta }
          a_val
      in
      (Ap (f_core, Implicit, a_core), ret_ty)
  | _ -> raise (ElabError ApplyingNonFunction)

(** Let inference. *)
and infer_let (ctx : Ctx.t) (name : string) (type_ : Surface.t option)
    (value : Surface.t) (body : Surface.t) : term * value =
  let val_core, val_ty =
    match type_ with
    | Some ty_expr ->
        let ty_core, ty_ty = infer ctx ty_expr in
        Ctx.unify ctx ty_ty VU;
        let ty_val = Ctx.eval ctx ty_core in
        let core = check ctx value ty_val in
        (core, ty_val)
    | None -> infer ctx value
  in
  let val_val = Ctx.eval ctx val_core in
  let ty_term = Ctx.quote ctx val_ty in
  let ctx' = Ctx.define ctx name val_ty val_val in
  let body_core, body_ty = infer ctx' body in
  (Let (ty_term, val_core, body_core), body_ty)

(** If inference. *)
and infer_if (ctx : Ctx.t) (cond : Surface.t) (then_ : Surface.t)
    (else_ : Surface.t) : term * value =
  let cond_core = check ctx cond (VAtomTy TBool) in
  let then_core, then_ty = infer ctx then_ in
  let else_core = check ctx else_ then_ty in
  (If (cond_core, then_core, else_core), then_ty)

(** Build a constructor's VLam chain + VPi type from type params and optional payload. *)
and build_ctor (mc : MetaContext.t) (env : env) (nominal_name : string) (ctor_name : string)
    (param_metas : value list) (payload_val_opt : value option)
    : value * value =
  let num_params = List.length param_metas in
  let has_payload = Option.is_some payload_val_opt in
  let total_args = num_params + (if has_payload then 1 else 0) in
  (* Build spine Var terms: outer params first (higher ix), payload last (ix=0) *)
  let param_vars = List.mapi (fun i _ -> Var (total_args - 1 - i)) param_metas in
  let payload_var = if has_payload then [ Var 0 ] else [] in
  let all_spine_vars = param_vars @ payload_var in
  (* Innermost body: Ctor *)
  let body_term =
    Ctor { name = ctor_name; spine = all_spine_vars;
           nominal_name; nominal_spine = param_vars }
  in
  (* Wrap in Lam for payload, then Lam for each type param *)
  let core_term =
    let with_payload = if has_payload then Lam body_term else body_term in
    List.fold_right (fun _ acc -> Lam acc) param_metas with_payload
  in
  let ctor_val = Nbe.eval mc env core_term in
  (* Build Pi type: payload VPi has the actual domain, type params are VPi{U, ...}. *)
  let ret_type =
    let nom_param_vars =
      List.mapi (fun i _ -> Var (num_params - i + if has_payload then 0 else -1)) param_metas
    in
    let ret_term = NomRef (nominal_name, nom_param_vars) in
    let inner_val : value =
      match payload_val_opt with
      | Some p -> VPi { explicitness = Explicit; domain = p; codomain = { env; body = ret_term } }
      | None -> Nbe.eval mc env ret_term
    in
    let depth = List.length env in
    List.fold_right
      (fun _ acc ->
        VPi { explicitness = Implicit; domain = VU; codomain = { env; body = Nbe.quote mc depth acc } })
      param_metas inner_val
  in
  (ctor_val, ret_type)

(** Lambda inference. *)
and infer_lam (ctx : Ctx.t) (param : Surface.param) (body : Surface.t) :
    term * value =
  let a_ty =
    match param.type_ with
    | Some ty_expr ->
        let ty_core, ty_ty = infer ctx ty_expr in
        Ctx.unify ctx ty_ty VU;
        Ctx.eval ctx ty_core
    | None ->
        Ctx.raw_meta ctx
  in
  let ctx' = Ctx.bind ctx param.name a_ty in
  let body_core, body_ty = infer ctx' body in
  let body_ty_term = Ctx.quote ctx' body_ty in
  let pi_ty = VPi { explicitness = Explicit; domain = a_ty; codomain = { env = ctx.env; body = body_ty_term } } in
  (Lam body_core, pi_ty)

(** Bidirectional checking: verify [expr] against an [expected] type. *)
and check (ctx : Ctx.t) (expr : Surface.t) (expected : value) : term =
  let expected = Nbe.force ctx.metas expected in
  match (expr, expected) with
  | Lam (param, body), VPi { explicitness = _; domain = a_ty; codomain = b_clo } ->
      let ctx' = Ctx.bind ctx param.name a_ty in
      let b_ty = Nbe.closure_apply ctx.metas b_clo (VRigid { lvl = ctx.lvl; spine = [] }) in
      let body_core = check ctx' body b_ty in
      Lam body_core
  | If { cond; then_; else_ }, _ ->
      let cond_core = check ctx cond (VAtomTy TBool) in
      let then_core = check ctx then_ expected in
      let else_core = check ctx else_ expected in
      If (cond_core, then_core, else_core)
  | Prod elems, VProdTy tys ->
      if List.length elems <> List.length tys then
        raise (ElabError TupleLengthMismatch);
      let cores = List.map2 (check ctx) elems tys in
      Prod cores
  | Let { name; type_; value; body }, _ ->
      let val_core, val_ty =
        match type_ with
        | Some ty_expr ->
            let ty_core, ty_ty = infer ctx ty_expr in
            Ctx.unify ctx ty_ty VU;
            let ty_val = Ctx.eval ctx ty_core in
            let core = check ctx value ty_val in
            (core, ty_val)
        | None -> infer ctx value
      in
      let val_val = Ctx.eval ctx val_core in
      let ty_term = Ctx.quote ctx val_ty in
      let ctx' = Ctx.define ctx name val_ty val_val in
      let body_core = check ctx' body expected in
      Let (ty_term, val_core, body_core)
  | Match (scrutinee, branches), _ ->
      let scrut_core, scrut_ty = infer ctx scrutinee in
      (match Nbe.force ctx.metas scrut_ty with
      | VNominal _ ->
          let branches' =
            List.map (fun (pat, body) ->
              let core_pat, ctx' = elaborate_pat ctx pat scrut_ty in
              let body_core = check ctx' body expected in
              (core_pat, body_core))
              branches
          in
          Match (scrut_core, branches')
      | _ -> raise (ElabError NotANominalType))
  | _ ->
      let core, inferred = infer ctx expr in
      let rec wrap_implicits core ty =
        match Nbe.force ctx.metas ty with
        | VPi { explicitness = Implicit; codomain = b_clo; _ } ->
            let meta_core = Ctx.fresh_meta ctx in
            let meta_val = Ctx.eval ctx meta_core in
            let ret_ty = Nbe.closure_apply ctx.metas b_clo meta_val in
            wrap_implicits (Ap (core, Implicit, meta_core)) ret_ty
        | _ -> (core, ty)
      in
      let core, inferred = wrap_implicits core inferred in
      Ctx.unify ctx expected inferred;
      core

(** Build the initial elaboration context with built-in types ([I64],
    [Bool], [Unit], [Char], [Type]) and primitive operators. *)
let init_ctx () : Ctx.t =
  let ctx = Ctx.empty () in
  let add_type ctx name v = Ctx.define ctx name VU v in
  let ctx = add_type ctx "I64" (VAtomTy TI64) in
  let ctx = add_type ctx "Bool" (VAtomTy TBool) in
  let ctx = add_type ctx "Unit" (VAtomTy TUnit) in
  let ctx = add_type ctx "Char" (VAtomTy TChar) in
  let ctx = Ctx.define ctx "Type" VU VU in
  NameMap.fold
    (fun name ty ctx ->
      Ctx.define ctx name ty (VNeutral { ty; neutral = { head = HPrim name; frames = [] } }))
    prims ctx

(** Entry point: elaborate a surface expression in the initial context. *)
let on_expr (expr : Surface.t) : term * value =
  let ctx = init_ctx () in
  infer ctx expr
