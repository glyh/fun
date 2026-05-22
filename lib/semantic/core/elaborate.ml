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
  | NonExhaustive of string

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
    Unify.unify ctx.metas ctx.env ctx.lvl v1 v2

  let conv (ctx : t) (v1 : value) (v2 : value) : bool =
    Nbe.conv ctx.metas ctx.lvl v1 v2
end

(** Build a closed [VPi] value (for primitive types). *)
let ( ^-> ) = fun lhs rhs -> VPi { explicitness = Explicit; domain = lhs; codomain = { env = []; body = rhs } }
(** Build a [Pi] term (for primitive type schemas). *)
let ( ^->> ) = fun lhs rhs -> Pi (Explicit, lhs, rhs)

let atom_ty_of_atom = function
  | Syntax.Ast.Atom.I64 _ -> TI64
  | Bool _ -> TBool
  | Unit -> TUnit

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

let nominal_from_constructor_type ctx ctor_ty =
  let rec follow ty =
    match Nbe.force ctx.Ctx.metas ty with
    | VPi { codomain = b_clo; _ } ->
        follow
          (Nbe.closure_apply ctx.Ctx.metas b_clo
             (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
    | VNominal n ->
        let fresh_params = List.init (List.length n.params) (fun _ -> Ctx.raw_meta ctx) in
        VNominal { n with params = fresh_params }
    | _ -> raise (ElabError NotANominalType)
  in
  follow ctor_ty

let unify_scrutinee_ty ctx ty target =
  match Nbe.force ctx.Ctx.metas ty with
  | VFlex { id; spine = [] } -> MetaContext.solve ctx.Ctx.metas id target
  | _ -> Ctx.unify ctx ty target

let match_domain_of_ty ctx ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VNominal { params; constructors; _ } ->
      let ntp = List.length params in
      Core_match_compile.Nominal
        (List.map (fun (name, payload) -> (name, ntp, Option.is_some payload)) constructors)
  | VAtomTy atom_ty -> Atom atom_ty
  | _ -> Unknown

let rec type_at_occurrence ctx ty (occ : Core_decision_tree.occurrence) =
  match occ with
  | OBase -> Some ty
  | OChild { parent; index } -> (
      match type_at_occurrence ctx ty parent with
      | Some parent_ty -> (
          match Nbe.force ctx.Ctx.metas parent_ty with
          | VProdTy tys -> List.nth_opt tys index
          | VNominal { params; constructors; _ } ->
              let num_type_params = List.length params in
              let payload_index = index - num_type_params in
              if payload_index = 0 then
                constructors
                |> List.find_map (fun (_, payload_opt) -> payload_opt)
                |> Option.map (fun payload_clo ->
                     Nbe.eval ctx.Ctx.metas (List.rev params @ payload_clo.env) payload_clo.body)
              else None
          | _ -> None)
      | None -> None)

let domain_of_occurrence ctx scrut_ty occ =
  match type_at_occurrence ctx scrut_ty occ with
  | Some ty -> match_domain_of_ty ctx ty
  | None -> Unknown

let refine_match_scrutinee_ty ctx scrut_ty branches =
  let ty = Nbe.force ctx.Ctx.metas scrut_ty in
  match ty with
  | VNominal _ | VAtomTy _ | VProdTy _ -> ty
  | _ ->
      let rec find = function
        | [] -> raise (ElabError NotANominalType)
        | (Surface.PatCon (name, _), _) :: _ ->
            let _, ctor_ty = Ctx.lookup ctx name in
            let target = nominal_from_constructor_type ctx ctor_ty in
            unify_scrutinee_ty ctx ty target;
            Nbe.force ctx.Ctx.metas ty
        | (Surface.PatAtom atom, _) :: _ ->
            let target = VAtomTy (atom_ty_of_atom atom) in
            unify_scrutinee_ty ctx ty target;
            Nbe.force ctx.Ctx.metas ty
        | (Surface.PatProd ps, _) :: _ ->
            let target = VProdTy (List.map (fun _ -> Ctx.raw_meta ctx) ps) in
            unify_scrutinee_ty ctx ty target;
            Nbe.force ctx.Ctx.metas ty
        | _ :: rest -> find rest
      in
      find branches

let check_match_exhaustive ctx scrut_ty pats =
  let domain_of_occurrence = domain_of_occurrence ctx scrut_ty in
  try ignore (Core_match_compile.compile_with_domains ~domain_of_occurrence pats)
  with Core_match_compile.Non_exhaustive mp ->
    let rec pp_missing = function
      | Core_match_compile.MWild -> "_"
      | Core_match_compile.MCon (name, None) -> name
      | Core_match_compile.MCon (name, Some sub) ->
          name ^ "(" ^ pp_missing sub ^ ")"
    in
    raise (ElabError (NonExhaustive (pp_missing mp)))

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
  | Let { name; type_; value; body; recursive } ->
      infer_let ctx name type_ value body recursive
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
      let num_params = List.length params in
      (* Bind type params as rigid variables (locally abstract types) *)
      let param_ctx =
        List.fold_left
          (fun ctx param_name ->
            Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
          ctx params
      in
      (* Elaborate constructor payload types with rigid params in scope.
         Store as closures: env = outer ctx.env, body = payload term
         with Var 0..n-1 referencing params. *)
      let elaborated_ctors =
        List.map (fun (cname, payload_opt) ->
          match payload_opt with
          | None -> (cname, None)
          | Some payload_expr ->
              let payload_core, payload_ty = infer param_ctx payload_expr in
              Ctx.unify param_ctx payload_ty VU;
              let payload_term = payload_core in
              (cname, Some { env = ctx.env; body = payload_term }))
        ctors
      in
      let nominal = VNominal { id = NominalId.fresh (); name; params = []; constructors = elaborated_ctors } in
      (* For parameterized types, build an Explicit VPi chain so Option I64 works.
         For nullary types, just bind with VU as before. *)
      let body_ctx =
        if num_params = 0 then
          Ctx.define param_ctx name VU nominal
        else begin
          (* Push VNominal first so NomRef evaluation can find it *)
          let body_ctx = { param_ctx with
            env = nominal :: param_ctx.env;
            types = VU :: param_ctx.types;
            lvl = param_ctx.lvl + 1;
            bds = Defined :: param_ctx.bds
          } in
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
          let type_body_term = NomRef (name, type_var_terms) in
          let type_core_term =
            List.fold_right (fun _ acc -> Lam acc) params type_body_term
          in
          let type_val = Nbe.eval body_ctx.metas body_ctx.env type_core_term in
          let type_ty =
            let depth = List.length body_ctx.env in
            List.fold_right
              (fun _ acc ->
                VPi { explicitness = Explicit; domain = VU;
                      codomain = { env = body_ctx.env; body = Nbe.quote body_ctx.metas depth acc } })
              params VU
          in
          Ctx.define body_ctx name type_ty type_val
        end
      in
      let env = body_ctx.env in
      let body_ctx =
        List.fold_left2
          (fun ctx (cname, _payload_surface) payload_clo_opt ->
            let ctor_val, ctor_ty =
              build_ctor body_ctx.metas env name cname num_params payload_clo_opt in
            Ctx.define ctx cname ctor_ty ctor_val)
          body_ctx ctors (List.map snd elaborated_ctors)
      in
      let body_core, body_ty = infer body_ctx body in
      let ctor_payload_terms =
        List.map (fun (cname, payload_opt) ->
          match payload_opt with
          | None -> (cname, None)
          | Some payload_expr ->
              let payload_core, payload_ty = infer param_ctx payload_expr in
              (match payload_ty with
              | VU -> ()
              | _ -> failwith ("constructor payload for " ^ cname ^ " must be a type"));
              (cname, Some payload_core))
        ctors
      in
      (NominalDef { name; num_params; ctors = ctor_payload_terms; body = body_core },
       body_ty)
  | Match (scrutinee, branches) ->
      let scrut_core, scrut_ty = infer ctx scrutinee in
      let scrut_ty = refine_match_scrutinee_ty ctx scrut_ty branches in
      let ret_ty = Ctx.raw_meta ctx in
      let branches' =
        List.map (fun (pat, body) ->
          let core_pat, ctx' = elaborate_pat ctx pat scrut_ty in
          let body_core = check ctx' body ret_ty in
          (core_pat, body_core))
          branches
      in
      let pats = List.map fst branches' in
      check_match_exhaustive ctx scrut_ty pats;
      (Match (scrut_core, branches'), Nbe.force ctx.metas ret_ty)

(** Elaborate a surface pattern against a scrutinee type, producing a core
    pattern and extending the context with bound pattern variables. *)
and elaborate_pat (ctx : Ctx.t) (pat : Surface.pat) (scrutinee_ty : value)
    : core_pat * Ctx.t =
  match pat with
  | PatWild -> (CPatWild, ctx)
  | PatBind name -> (CPatBind, Ctx.bind ctx name scrutinee_ty)
  | PatAtom atom ->
      Ctx.unify ctx scrutinee_ty (VAtomTy (atom_ty_of_atom atom));
      (CPatAtom atom, ctx)
  | PatProd sub_pats -> (
      match Nbe.force ctx.metas scrutinee_ty with
      | VProdTy tys ->
          if List.length sub_pats <> List.length tys then
            raise (ElabError TupleLengthMismatch);
          let core_subs, ctx' =
            List.fold_left2
              (fun (acc, ctx) pat ty ->
                let core_pat, ctx' = elaborate_pat ctx pat ty in
                (core_pat :: acc, ctx'))
              ([], ctx) sub_pats tys
          in
          (CPatProd (List.rev core_subs), ctx')
      | _ -> raise (ElabError TupleLengthMismatch))
  | PatCon (name, sub_pats) ->
      (match Nbe.force ctx.metas scrutinee_ty with
      | VNominal n ->
          (match List.find_opt (fun (cname, _) -> String.equal cname name) n.constructors with
          | Some (_, payload_opt) ->
              let num_type_params = List.length n.params in
              (match (sub_pats, payload_opt) with
              | [], None -> (CPatCon (name, num_type_params, []), ctx)
              | [sub_pat], Some payload_clo ->
                  let payload_ty =
                    Nbe.eval ctx.metas (List.rev n.params @ payload_clo.env) payload_clo.body in
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

(** Generalize a let-bound value's type: if the value is a syntactic lambda
    and its type contains unsolved metas, abstract those metas into implicit
    VPi layers. At each use site, Phase 6's insert_implicits auto-instantiates
    with fresh metas. *)
and generalize (ctx : Ctx.t) (val_core : term) (val_ty : value) : term * value =
  (* Only generalize simple single-binder lambdas at the top level.
     Multi-binder or nested lambdas would have their de Bruijn levels
     shifted by outer binders, breaking VRigid level assignments. *)
  let has_bound = List.exists (fun bd -> bd = Bound) ctx.bds in
  let eligible = match val_core with
    | Lam body when not has_bound ->
        begin match body with Lam _ -> false | _ -> true end
    | _ -> false
  in
  if not eligible then (val_core, val_ty)
  else begin
    let seen = ref [] in
    let add id = if not (List.mem id !seen) then seen := id :: !seen in
    let rec collect v =
      match Nbe.force ctx.metas v with
      | VFlex { id; spine = [] } ->
          (match MetaContext.lookup ctx.metas id with Unsolved -> add id | Solved _ -> ())
      | VFlex { spine; _ } -> List.iter collect spine
      | VPi { domain = a; _ } -> collect a
      | VU | VAtom _ | VAtomTy _ | VRigid _ | VProd _ | VProdTy _ -> ()
      | _ -> ()
    in
    collect val_ty;
    let unsolved = List.rev !seen in
    let n = List.length unsolved in
    if n = 0 then (val_core, val_ty)
    else begin
      (* Solve innermost meta to highest level (deepest), outermost to ctx.lvl.
         All VPis are quoted at depth ctx.lvl + n so VRigid{lvl} → Var(ix) works. *)
      List.iteri (fun i meta_id ->
        MetaContext.solve ctx.metas meta_id
          (VRigid { lvl = ctx.lvl + n - 1 - i; spine = [] })
      ) unsolved;
      let gen_val = List.fold_left (fun acc _ -> Lam acc) val_core unsolved in
      let qdepth = ctx.lvl + n in
      let gen_ty_val =
        List.fold_right (fun _ acc ->
          VPi { explicitness = Implicit; domain = VU;
                codomain = { env = ctx.env; body = Nbe.quote ctx.metas qdepth acc } })
          unsolved val_ty
      in
      (gen_val, gen_ty_val)
    end
  end

(** Let inference. *)
and infer_let (ctx : Ctx.t) (name : string) (type_ : Surface.t option)
    (value : Surface.t) (body : Surface.t) (recursive : bool) : term * value =
  if recursive then begin
    let rec_ty =
      match type_ with
      | Some ty_expr ->
          let ty_core, ty_ty = infer ctx ty_expr in
          Ctx.unify ctx ty_ty VU;
          Ctx.eval ctx ty_core
      | None -> Ctx.raw_meta ctx
    in
    let ctx_with_self = Ctx.bind ctx name rec_ty in
    let val_core = check ctx_with_self value rec_ty in
    let fix_core = Fix val_core in
    let fix_val = Ctx.eval ctx fix_core in
    let ty_term = Ctx.quote ctx rec_ty in
    let ctx' = Ctx.define ctx name rec_ty fix_val in
    let body_core, body_ty = infer ctx' body in
    (Let (ty_term, fix_core, body_core), body_ty)
  end else begin
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
    let gen_val_core, gen_val_ty = generalize ctx val_core val_ty in
    let val_val = Ctx.eval ctx gen_val_core in
    let ty_term = Ctx.quote ctx gen_val_ty in
    let ctx' = Ctx.define ctx name gen_val_ty val_val in
    let body_core, body_ty = infer ctx' body in
    (Let (ty_term, gen_val_core, body_core), body_ty)
  end

(** If inference. *)
and infer_if (ctx : Ctx.t) (cond : Surface.t) (then_ : Surface.t)
    (else_ : Surface.t) : term * value =
  let cond_core = check ctx cond (VAtomTy TBool) in
  let then_core, then_ty = infer ctx then_ in
  let else_core = check ctx else_ then_ty in
  (If (cond_core, then_core, else_core), then_ty)

(** Build a constructor's VLam chain + VPi type from type params and optional payload.
    [payload_clo_opt] is a closure whose body is the payload type term with
    de Bruijn indices 0..num_params-1 referencing type params. *)
and build_ctor (mc : MetaContext.t) (env : env) (nominal_name : string) (ctor_name : string)
    (num_params : int) (payload_clo_opt : closure option)
    : value * value =
  let has_payload = Option.is_some payload_clo_opt in
  let total_args = num_params + (if has_payload then 1 else 0) in
  let param_vars = List.init num_params (fun i -> Var (total_args - 1 - i)) in
  let payload_var = if has_payload then [ Var 0 ] else [] in
  let all_spine_vars = param_vars @ payload_var in
  let body_term =
    Ctor { name = ctor_name; spine = all_spine_vars;
           nominal_name; nominal_spine = param_vars }
  in
  let core_term =
    let with_payload = if has_payload then Lam body_term else body_term in
    let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
    wrap num_params with_payload
  in
  let ctor_val = Nbe.eval mc env core_term in
  let depth = List.length env in
  let type_term =
    let nom_ret_vars =
      if has_payload then
        List.init num_params (fun i -> Var (num_params - i))
      else
        List.init num_params (fun i -> Var (num_params - 1 - i))
    in
    let ret_term = NomRef (nominal_name, nom_ret_vars) in
    match payload_clo_opt with
    | Some payload_clo ->
        (* The payload closure body has indices relative to [payload_clo.env]
           with num_params extra bindings prepended. Inside the implicit Pi chain,
           param i is at level depth+i. Evaluate with rigid vars to get the
           payload value, then quote at the right depth. *)
        let param_rigids = List.init num_params (fun i ->
          VRigid { lvl = depth + i; spine = [] }) in
        let payload_val =
          Nbe.eval mc (List.rev param_rigids @ payload_clo.env) payload_clo.body in
        let payload_term = Nbe.quote mc (depth + num_params) payload_val in
        let inner = Pi (Explicit, payload_term, ret_term) in
        let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (Pi (Implicit, U, t)) in
        wrap_pi num_params inner
    | None ->
        let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (Pi (Implicit, U, t)) in
        wrap_pi num_params ret_term
  in
  let ret_type = Nbe.eval mc env type_term in
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
  | Let { name; type_; value; body; recursive }, _ ->
      if recursive then begin
        let rec_ty =
          match type_ with
          | Some ty_expr ->
              let ty_core, ty_ty = infer ctx ty_expr in
              Ctx.unify ctx ty_ty VU;
              Ctx.eval ctx ty_core
          | None -> Ctx.raw_meta ctx
        in
        let ctx_with_self = Ctx.bind ctx name rec_ty in
        let val_core = check ctx_with_self value rec_ty in
        let fix_core = Fix val_core in
        let fix_val = Ctx.eval ctx fix_core in
        let ty_term = Ctx.quote ctx rec_ty in
        let ctx' = Ctx.define ctx name rec_ty fix_val in
        let body_core = check ctx' body expected in
        Let (ty_term, fix_core, body_core)
      end else begin
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
        let gen_val_core, gen_val_ty = generalize ctx val_core val_ty in
        let val_val = Ctx.eval ctx gen_val_core in
        let ty_term = Ctx.quote ctx gen_val_ty in
        let ctx' = Ctx.define ctx name gen_val_ty val_val in
        let body_core = check ctx' body expected in
        Let (ty_term, gen_val_core, body_core)
      end
  | Match (scrutinee, branches), _ ->
      let scrut_core, scrut_ty = infer ctx scrutinee in
      let scrut_ty = refine_match_scrutinee_ty ctx scrut_ty branches in
      let branches' =
        List.map (fun (pat, body) ->
          let core_pat, ctx' = elaborate_pat ctx pat scrut_ty in
          let body_core = check ctx' body expected in
          (core_pat, body_core))
          branches
      in
      check_match_exhaustive ctx scrut_ty (List.map fst branches');
      Match (scrut_core, branches')
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

(** Entry point: elaborate a surface expression in the given context. *)
let on_expr (ctx : Ctx.t) (expr : Surface.t) : term * value =
  infer ctx expr
