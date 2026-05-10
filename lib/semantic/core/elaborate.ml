open Core

type elab_error =
  | UnboundVariable of string
  | ApplyingNonFunction
  | TupleLengthMismatch

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

  (** [fresh_meta] followed by immediate evaluation to a [VFlex] value. *)
  let fresh_meta_val (ctx : t) : value =
    let t = fresh_meta ctx in
    Nbe.eval ctx.metas ctx.env t

  let eval (ctx : t) (t : term) : value = Nbe.eval ctx.metas ctx.env t
  let quote (ctx : t) (v : value) : term = Nbe.quote ctx.metas ctx.lvl v

  let unify (ctx : t) (v1 : value) (v2 : value) : unit =
    Unify.unify ctx.metas ctx.lvl v1 v2
end

(** Build a closed [VPi] value (for primitive types). *)
let ( ^-> ) = fun lhs rhs -> VPi { domain = lhs; codomain = { env = []; body = rhs } }
(** Build a [Pi] term (for primitive type schemas). *)
let ( ^->> ) = fun lhs rhs -> Pi (lhs, rhs)

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
    core term and its type. Dispatches to [check] when an annotation
    provides the expected type; otherwise synthesizes via helper functions. *)
let rec infer (ctx : Ctx.t) (expr : Surface.t) : term * value =
  match expr with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy TI64)
  | Atom (Bool b) -> (Atom (Bool b), VAtomTy TBool)
  | Atom Unit -> (Atom Unit, VAtomTy TUnit)
  | Var name ->
      let ix, ty = Ctx.lookup ctx name in
      (Var ix, ty)
  | Ap (f, a) -> infer_ap ctx f a
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
  | Arrow (a, b) ->
      let a_core, a_ty = infer ctx a in
      Ctx.unify ctx a_ty VU;
      let a_val = Ctx.eval ctx a_core in
      let ctx' = Ctx.bind ctx "_" a_val in
      let b_core, b_ty = infer ctx' b in
      Ctx.unify ctx' b_ty VU;
      (Pi (a_core, b_core), VU)
  | FieldAccess (e, name) ->
      let e_core, e_ty = infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VStruct { fields; _ } ->
          (match List.find_opt (fun (n, k, _) -> String.equal n name && k <> Private) fields with
          | Some (_, _, field_ty) -> (Dot (e_core, name), field_ty)
          | None -> raise (ElabError (UnboundVariable name)))
      | VFlex _ | VRigid _ | VNeutral _ ->
          (* stuck type: raw meta (no Bound capture) for partial constraint *)
          let raw_id = MetaContext.fresh ctx.metas in
          let result_ty = VFlex { id = raw_id; spine = [] } in
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
      (* bindings: sequential, each sees previous lets only (not fields) *)
      let rec go ctx acc = function
        | [] -> List.rev acc
        | { Surface.name; value; public } :: rest ->
            let val_core, val_ty = infer ctx value in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            go ctx' ((name, kind, val_core, val_ty) :: acc) rest
      in
      let core_bindings = go ctx [] bindings in
      let result_con_fields = List.map (fun (n, c, _) -> (n, c)) con_cores in
      let result_bindings = List.map (fun (n, k, c, _) -> (n, k, c)) core_bindings in
      let type_fields =
        List.map (fun (n, _, ty) -> (n, Field, ty)) con_cores
        @ List.filter_map (fun (n, k, _, ty) ->
            if k = Public then Some (n, Public, ty) else None) core_bindings
      in
      (Struct { con_fields = result_con_fields; bindings = result_bindings; partial = false },
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

(** Application inference.  Two cases:
     - [f]'s type is known [VPi]: check [a] against the domain, compute
       the codomain via substitution (no metas needed).
     - [f]'s type is stuck: create a fresh domain meta and a fresh
       codomain meta, unify [f_ty = Pi(?dom, ?cod)], then proceed. *)
and infer_ap (ctx : Ctx.t) (f : Surface.t) (a : Surface.t) : term * value =
  let f_core, f_ty = infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  match f_ty with
  | VPi { domain = a_ty; codomain = b_clo; _ } ->
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_ty = Nbe.closure_apply ctx.metas b_clo a_val in
      (Ap (f_core, a_core), ret_ty)
  | VFlex _ | VRigid _ | VNeutral _ ->
      let a_ty = Ctx.fresh_meta_val ctx in
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_meta = Ctx.fresh_meta_val (Ctx.bind ctx "_" a_ty) in
      let expected_f_ty =
        VPi
          { domain = a_ty;
            codomain = { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta } }
      in
      Ctx.unify ctx f_ty expected_f_ty;
      let ret_ty =
        Nbe.closure_apply ctx.metas
          { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta }
          a_val
      in
      (Ap (f_core, a_core), ret_ty)
  | _ -> raise (ElabError ApplyingNonFunction)

(** Let inference.  If annotated, [check] the value against the annotation;
    otherwise [infer] the value.  Then [define] the result and infer the body. *)
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

(** If inference.  Check the condition against [Bool], infer the then-branch,
    and check the else-branch against the then-branch's type. *)
and infer_if (ctx : Ctx.t) (cond : Surface.t) (then_ : Surface.t)
    (else_ : Surface.t) : term * value =
  let cond_core = check ctx cond (VAtomTy TBool) in
  let then_core, then_ty = infer ctx then_ in
  let else_core = check ctx else_ then_ty in
  (If (cond_core, then_core, else_core), then_ty)

(** Lambda inference.  If the parameter has no annotation, create a fresh
    domain meta.  Then [bind] the parameter, infer the body, and return a
    [Lam] term with a [VPi] type whose codomain is the (quoted) body type. *)
and infer_lam (ctx : Ctx.t) (param : Surface.param) (body : Surface.t) :
    term * value =
  let a_ty =
    match param.type_ with
    | Some ty_expr ->
        let ty_core, ty_ty = infer ctx ty_expr in
        Ctx.unify ctx ty_ty VU;
        Ctx.eval ctx ty_core
    | None -> Ctx.fresh_meta_val ctx
  in
  let ctx' = Ctx.bind ctx param.name a_ty in
  let body_core, body_ty = infer ctx' body in
  let body_ty_term = Ctx.quote ctx' body_ty in
  let pi_ty = VPi { domain = a_ty; codomain = { env = ctx.env; body = body_ty_term } } in
  (Lam body_core, pi_ty)

(** Bidirectional checking: verify [expr] against an [expected] type.
     - [Lam] vs [VPi]: bind the parameter, check the body against the
       codomain (extended with a fresh rigid variable).
     - [If]: check condition against [Bool], then check both branches.
     - [Prod] vs [VProdTy]: check elements pointwise.
     - [Let]: infer/check value, define it, check body.
     - Fallback: [infer] and then [unify] inferred with expected. *)
and check (ctx : Ctx.t) (expr : Surface.t) (expected : value) : term =
  let expected = Nbe.force ctx.metas expected in
  match (expr, expected) with
  | Lam (param, body), VPi { domain = a_ty; codomain = b_clo; _ } ->
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
  | _ ->
      let core, inferred = infer ctx expr in
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
