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

  let raw_meta (ctx : t) : value =
    VFlex { id = MetaContext.fresh ctx.metas; spine = [] }

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

(** Bidirectional type inference: given a surface expression, produce an
    (possibly extended) context, a core term, and its type. *)
let rec infer (ctx : Ctx.t) (expr : Surface.t) : Ctx.t * term * value =
  match expr with
  | Atom (I64 n) -> (ctx, Atom (I64 n), VAtomTy TI64)
  | Atom (Bool b) -> (ctx, Atom (Bool b), VAtomTy TBool)
  | Atom Unit -> (ctx, Atom Unit, VAtomTy TUnit)
  | Var name ->
      let ix, ty = Ctx.lookup ctx name in
      (ctx, Var ix, ty)
  | Ap (f, a) -> infer_ap ctx f a
  | Let { name; type_; value; body } -> infer_let ctx name type_ value body
  | If { cond; then_; else_ } -> infer_if ctx cond then_ else_
  | Lam (param, body) -> infer_lam ctx param body
  | Annotated { inner; typ } ->
      let _ctx, ty_core, ty_ty = infer ctx typ in
      Ctx.unify ctx ty_ty VU;
      let ty_val = Ctx.eval ctx ty_core in
      let _, core = check ctx inner ty_val in
      (ctx, core, ty_val)
  | Prod elems ->
      let cores_tys = List.map (fun e -> let _, c, t = infer ctx e in (c, t)) elems in
      let cores = List.map fst cores_tys in
      let tys = List.map snd cores_tys in
      (ctx, Prod cores, VProdTy tys)
  | Arrow (a, b) ->
      let _, a_core, a_ty = infer ctx a in
      Ctx.unify ctx a_ty VU;
      let a_val = Ctx.eval ctx a_core in
      let ctx' = Ctx.bind ctx "_" a_val in
      let _, b_core, b_ty = infer ctx' b in
      Ctx.unify ctx' b_ty VU;
      (ctx, Pi (a_core, b_core), VU)
  | FieldAccess (e, name) ->
      let _, e_core, e_ty = infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VStruct { fields; _ } ->
          (match List.find_opt (fun (n, k, _) -> String.equal n name && k <> Private) fields with
          | Some (_, _, field_ty) -> (ctx, Dot (e_core, name), field_ty)
          | None -> raise (ElabError (UnboundVariable name)))
      | VFlex _ | VRigid _ | VNeutral _ ->
          let result_ty = Ctx.raw_meta ctx in
          let constraint_ty =
            VStruct { fields = [ (name, Field, result_ty) ]; partial = true }
          in
          Ctx.unify ctx e_ty constraint_ty;
          (ctx, Dot (e_core, name), result_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Proj (e, i) ->
      let _, e_core, e_ty = infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VProdTy tys ->
          if i < 0 || i >= List.length tys then
            raise (ElabError TupleLengthMismatch);
          (ctx, Proj (e_core, i), List.nth tys i)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Struct { con_fields; bindings } ->
      let con_cores =
        List.map (fun (name, ty_expr) ->
          let _, ty_core, ty_ty = infer ctx ty_expr in
          Ctx.unify ctx ty_ty VU;
          (name, ty_core, Ctx.eval ctx ty_core))
        con_fields
      in
      let rec go ctx (acc_binds, acc_fields) = function
        | [] -> ctx, (List.rev acc_binds, List.rev acc_fields)
        | Surface.LetBinding { name; value; public } :: rest ->
            let ctx, val_core, val_ty = infer ctx value in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            let field = if public then [ (name, kind, val_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               field @ acc_fields)
              rest
        | Surface.TypeBinding { name; ctors; public } :: rest ->
            let nominal = VNominal { id = NominalId.fresh (); name; constructors = ctors } in
            let kind = if public then Public else Private in
            (* Bind constructors + type name in context *)
            let ctx =
              List.fold_left
                (fun ctx ctor_name ->
                  Ctx.define ctx ctor_name nominal
                    (VCon { name = ctor_name; spine = []; nominal }))
                ctx ctors
            in
            let ctx = Ctx.define ctx name VU nominal in
            (* Pre-computed constructor values for the evaluator *)
            let ctor_values =
              List.map (fun c ->
                (c, VCon { name = c; spine = []; nominal }))
              ctors
            in
            (* Struct type fields: Color : VU, constructors : nominal *)
            let type_fields =
              (name, kind, VU)
              :: List.map (fun c -> (c, kind, nominal)) ctors
            in
            go ctx
              (TypeBind (name, kind, nominal, ctor_values) :: acc_binds,
               type_fields @ acc_fields)
              rest
      in
      let _ctx, (core_bindings, extra_fields) = go ctx ([], []) bindings in
      let result_con_fields = List.map (fun (n, c, _) -> (n, c)) con_cores in
      let type_fields =
        List.map (fun (n, _, ty) -> (n, Field, ty)) con_cores
        @ extra_fields
      in
      (ctx, Struct { con_fields = result_con_fields; bindings = core_bindings; partial = false },
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
          let _, body_core, body_ty = infer ctx' body in
          (ctx, Open (Var ix, body_core), body_ty)
      | _ -> raise (ElabError (UnboundVariable name (* not a struct *))))
  | TypeDef (name, ctors, body) ->
      let nominal = VNominal { id = NominalId.fresh (); name; constructors = ctors } in
      let body_ctx =
        List.fold_left
          (fun ctx ctor_name ->
            Ctx.define ctx ctor_name nominal
              (VCon { name = ctor_name; spine = []; nominal }))
          ctx ctors
      in
      let body_ctx = Ctx.define body_ctx name VU nominal in
      let _, body_core, body_ty = infer body_ctx body in
      (ctx, body_core, body_ty)

(** Application inference. *)
and infer_ap (ctx : Ctx.t) (f : Surface.t) (a : Surface.t) : Ctx.t * term * value =
  let ctx, f_core, f_ty = infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  match f_ty with
  | VPi { domain = a_ty; codomain = b_clo; _ } ->
      let a_core = check ctx a a_ty |> snd in
      let a_val = Ctx.eval ctx a_core in
      let ret_ty = Nbe.closure_apply ctx.metas b_clo a_val in
      (ctx, Ap (f_core, a_core), ret_ty)
  | VFlex _ | VRigid _ | VNeutral _ ->
      let a_ty = Ctx.raw_meta ctx in
      let a_core = check ctx a a_ty |> snd in
      let a_val = Ctx.eval ctx a_core in
      let ret_meta = Ctx.raw_meta (Ctx.bind ctx "_" a_ty) in
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
      (ctx, Ap (f_core, a_core), ret_ty)
  | _ -> raise (ElabError ApplyingNonFunction)

(** Let inference. *)
and infer_let (ctx : Ctx.t) (name : string) (type_ : Surface.t option)
    (value : Surface.t) (body : Surface.t) : Ctx.t * term * value =
  let ctx, val_core, val_ty =
    match type_ with
    | Some ty_expr ->
        let _, ty_core, ty_ty = infer ctx ty_expr in
        Ctx.unify ctx ty_ty VU;
        let ty_val = Ctx.eval ctx ty_core in
        let _, core = check ctx value ty_val in
        (ctx, core, ty_val)
    | None -> infer ctx value
  in
  let val_val = Ctx.eval ctx val_core in
  let ty_term = Ctx.quote ctx val_ty in
  let ctx' = Ctx.define ctx name val_ty val_val in
  let _, body_core, body_ty = infer ctx' body in
  (ctx, Let (ty_term, val_core, body_core), body_ty)

(** If inference. *)
and infer_if (ctx : Ctx.t) (cond : Surface.t) (then_ : Surface.t)
    (else_ : Surface.t) : Ctx.t * term * value =
  let _, cond_core = check ctx cond (VAtomTy TBool) in
  let _, then_core, then_ty = infer ctx then_ in
  let _, else_core = check ctx else_ then_ty in
  (ctx, If (cond_core, then_core, else_core), then_ty)

(** Lambda inference. *)
and infer_lam (ctx : Ctx.t) (param : Surface.param) (body : Surface.t) :
    Ctx.t * term * value =
  let a_ty =
    match param.type_ with
    | Some ty_expr ->
        let _, ty_core, ty_ty = infer ctx ty_expr in
        Ctx.unify ctx ty_ty VU;
        Ctx.eval ctx ty_core
    | None ->
        Ctx.raw_meta ctx
  in
  let ctx' = Ctx.bind ctx param.name a_ty in
  let _, body_core, body_ty = infer ctx' body in
  let body_ty_term = Ctx.quote ctx' body_ty in
  let pi_ty = VPi { domain = a_ty; codomain = { env = ctx.env; body = body_ty_term } } in
  (ctx, Lam body_core, pi_ty)

(** Bidirectional checking. Returns the (possibly extended) context
    and the checked term. *)
and check (ctx : Ctx.t) (expr : Surface.t) (expected : value) : Ctx.t * term =
  let expected = Nbe.force ctx.metas expected in
  match (expr, expected) with
  | Lam (param, body), VPi { domain = a_ty; codomain = b_clo; _ } ->
      let ctx' = Ctx.bind ctx param.name a_ty in
      let b_ty = Nbe.closure_apply ctx.metas b_clo (VRigid { lvl = ctx.lvl; spine = [] }) in
      let _, body_core = check ctx' body b_ty in
      (ctx, Lam body_core)
  | If { cond; then_; else_ }, _ ->
      let cond_core = check ctx cond (VAtomTy TBool) |> snd in
      let _, then_core = check ctx then_ expected in
      let _, else_core = check ctx else_ expected in
      (ctx, If (cond_core, then_core, else_core))
  | Prod elems, VProdTy tys ->
      if List.length elems <> List.length tys then
        raise (ElabError TupleLengthMismatch);
      let cores = List.map2 (fun e t -> check ctx e t |> snd) elems tys in
      (ctx, Prod cores)
  | Let { name; type_; value; body }, _ ->
      let ctx, val_core, val_ty =
        match type_ with
        | Some ty_expr ->
            let _, ty_core, ty_ty = infer ctx ty_expr in
            Ctx.unify ctx ty_ty VU;
            let ty_val = Ctx.eval ctx ty_core in
            let core = check ctx value ty_val |> snd in
            (ctx, core, ty_val)
        | None -> infer ctx value
      in
      let val_val = Ctx.eval ctx val_core in
      let ty_term = Ctx.quote ctx val_ty in
      let ctx' = Ctx.define ctx name val_ty val_val in
      let _, body_core = check ctx' body expected in
      (ctx, Let (ty_term, val_core, body_core))
  | _ ->
      let ctx, core, inferred = infer ctx expr in
      Ctx.unify ctx expected inferred;
      (ctx, core)

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
  let _, core, ty = infer ctx expr in
  (core, ty)
