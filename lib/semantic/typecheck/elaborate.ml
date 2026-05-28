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
  | PatternBindingMismatch
  | UnknownRecordField of string
  | DuplicateRecordField of string
  | MissingRecordField of string
  | DuplicateEffectOperation of string
  | ExpectedEffect
  | DuplicateEffect
  | DuplicateEffectBranch of string
  | UnknownEffectOperation of string
  | EffectOperationPathExpected
  | UnhandledEffects
  | NonExhaustive of string
  | InvalidRecursiveRecord of string
  | ImportRequiresLoader of string
  | UnknownTrait of string
  | UnknownTraitMethod of string
  | DuplicateTraitField of string
  | MissingTraitField of string
  | AmbiguousTraitImplementation of string

exception ElabError of elab_error

module NameMap = Map.Make (String)

let trait_id_counter = ref 0
let fresh_trait_id () =
  let id = !trait_id_counter in
  incr trait_id_counter;
  id

type name_entry = { level : lvl; ty : value }

type trait_info = {
  trait_id : int;
  trait_name : string;
  trait_params : string list;
  trait_fields : (string * closure) list;
}

type trait_evidence = {
  evidence_trait_id : int;
  evidence_trait_name : string;
  evidence_args : value list;
  evidence_level : lvl;
  evidence_ty : value;
}

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
    traits : trait_info NameMap.t;
    trait_evidence : trait_evidence list;
    self_entry : name_entry option;
    self_type : value option;
    resume_entry : name_entry option;
    loader : Core_loader.t option;
  }

  (** Create an empty elaboration context with a fresh meta store. *)
  let empty () : t =
    let metas = MetaContext.create () in
    {
      env = [];
      types = [];
      lvl = 0;
      metas;
      bds = [];
      name_table = NameMap.empty;
      traits = NameMap.empty;
      trait_evidence = [];
      self_entry = None;
      self_type = None;
      resume_entry = None;
      loader = None;
    }

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
      traits = ctx.traits;
      trait_evidence = ctx.trait_evidence;
      self_entry = ctx.self_entry;
      self_type = ctx.self_type;
      resume_entry = ctx.resume_entry;
      loader = ctx.loader;
    }

  let bind_anonymous (ctx : t) (ty : value) : t * name_entry =
    let entry = { level = ctx.lvl; ty } in
    let var = VRigid { lvl = ctx.lvl; spine = [] } in
    ({
       env = var :: ctx.env;
       types = ty :: ctx.types;
       lvl = ctx.lvl + 1;
       metas = ctx.metas;
       bds = Bound :: ctx.bds;
       name_table = ctx.name_table;
       traits = ctx.traits;
       trait_evidence = ctx.trait_evidence;
       self_entry = ctx.self_entry;
       self_type = ctx.self_type;
       resume_entry = ctx.resume_entry;
       loader = ctx.loader;
     },
     entry)

  (** Extend the context with a [Defined] binder (let/define). *)
  let define (ctx : t) (name : string) (ty : value) (v : value) : t =
    {
      env = v :: ctx.env;
      types = ty :: ctx.types;
      lvl = ctx.lvl + 1;
      metas = ctx.metas;
      bds = Defined :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table;
      traits = ctx.traits;
      trait_evidence = ctx.trait_evidence;
      self_entry = ctx.self_entry;
      self_type = ctx.self_type;
      resume_entry = ctx.resume_entry;
      loader = ctx.loader;
    }

  (** Resolve a user-written name to its de Bruijn index and type.
      Converts the stored de Bruijn level to an index using the current
      context depth: [index = depth - level - 1]. *)
  let lookup (ctx : t) (name : string) : ix * value =
    match NameMap.find_opt name ctx.name_table with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable name))

  let lookup_self (ctx : t) : ix * value =
    match ctx.self_entry with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable "self"))

  let lookup_self_type (ctx : t) : value =
    match ctx.self_type with
    | Some ty -> ty
    | None -> raise (ElabError (UnboundVariable "Self"))

  let lookup_resume (ctx : t) : ix * value =
    match ctx.resume_entry with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable "resume"))

  let with_self_type (ctx : t) (ty : value) : t = { ctx with self_type = Some ty }

  let with_loader (ctx : t) (loader : Core_loader.t) : t = { ctx with loader = Some loader }

  let add_trait (ctx : t) (trait_info : trait_info) : t =
    { ctx with traits = NameMap.add trait_info.trait_name trait_info ctx.traits }

  let add_trait_evidence (ctx : t) (evidence : trait_evidence) : t =
    { ctx with trait_evidence = evidence :: ctx.trait_evidence }

  let clear_self (ctx : t) : t = { ctx with self_entry = None }

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
let pure_effects = effect_row_closure [] empty_effect_row
let ( ^-> ) = fun lhs rhs -> VPi { explicitness = Explicit; domain = lhs; effects = pure_effects; codomain = { env = []; body = rhs } }
let ( ^=> ) = fun lhs rhs -> VPi { explicitness = Implicit; domain = lhs; effects = pure_effects; codomain = { env = []; body = rhs } }
(** Build a [Pi] term (for primitive type schemas). *)
let ( ^->> ) = fun lhs rhs -> Pi { explicitness = Explicit; domain = lhs; effects = empty_effect_row; codomain = rhs }
let ( ^=>> ) = fun lhs rhs -> Pi { explicitness = Implicit; domain = lhs; effects = empty_effect_row; codomain = rhs }

let atom_ty_of_atom = function
  | Atom.I64 _ -> TI64
  | Bool _ -> TBool
  | Unit -> TUnit
  | Char _ -> TChar
  | String _ -> TString

type expr_effect = { core : term; value : value }
type expr_effects = expr_effect list

let empty_expr_effects = []
let singleton_expr_effect core value = [ { core; value } ]

let prims =
  let arithemetic = VAtomTy TI64 ^-> AtomTy TI64 ^->> AtomTy TI64 in
  let i64_comparator = VAtomTy TI64 ^-> AtomTy TI64 ^->> AtomTy TBool in
  let bool_comparator = VAtomTy TBool ^-> AtomTy TBool ^->> AtomTy TBool in
  let char_comparator = VAtomTy TChar ^-> AtomTy TChar ^->> AtomTy TBool in
  let unit_comparator = VAtomTy TUnit ^-> AtomTy TUnit ^->> AtomTy TBool in
  let string_comparator = VAtomTy TString ^-> AtomTy TString ^->> AtomTy TBool in
  [
    ("+", arithemetic);
    ("-", arithemetic);
    ("*", arithemetic);
    ("/", arithemetic);
    ("%", arithemetic);
    ("eq_i64", i64_comparator);
    ("neq_i64", i64_comparator);
    ("eq_bool", bool_comparator);
    ("neq_bool", bool_comparator);
    ("eq_char", char_comparator);
    ("neq_char", char_comparator);
    ("eq_unit", unit_comparator);
    ("neq_unit", unit_comparator);
    ("eq_string", string_comparator);
    ("neq_string", string_comparator);
    ("panic", VPi { explicitness = Implicit; domain = VU; effects = pure_effects; codomain = { env = []; body = AtomTy TString ^->> Var 1 } });
    ("<", i64_comparator);
    (">", i64_comparator);
    ("<=", i64_comparator);
    (">=", i64_comparator);
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

let visible_record_fields fields =
  List.filter_map
    (fun (name, kind, ty) -> if kind = Field then Some (name, ty) else None)
    fields

let find_record_field fields name =
  List.find_opt (fun (n, _) -> String.equal n name) fields

let rec is_type_like_value ctx value =
  match Nbe.force ctx.Ctx.metas value with
  | VU | VAtomTy _ | VPi _ | VProdTy _ | VNominal _ | VEffect _ -> true
  | VStruct { fields; _ } ->
      List.for_all
        (fun (_, kind, value) ->
          match kind with
          | Private | PrivateMethod -> true
          | Field | Public | Method -> is_type_like_value ctx value)
        fields
  | _ -> false

let check_type_like ctx ty value =
  if not (Ctx.conv ctx ty VU || is_type_like_value ctx value) then Ctx.unify ctx ty VU

let check_duplicate_names names =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun name ->
      if Hashtbl.mem seen name then raise (ElabError (DuplicateRecordField name));
      Hashtbl.replace seen name ())
    names

let check_duplicate_eff_ops ops =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (op : Surface.effect_op) ->
      if Hashtbl.mem seen op.name then raise (ElabError (DuplicateEffectOperation op.name));
      Hashtbl.replace seen op.name ())
    ops

let check_duplicate_trait_fields fields =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (name, _) ->
      if Hashtbl.mem seen name then raise (ElabError (DuplicateTraitField name));
      Hashtbl.replace seen name ())
    fields

let trait_dict_ty trait_name args fields =
  let marker_fields =
    match args with
    | [ arg ] ->
        [ ("__arg", Private, arg);
          ("__trait", Private, VAtom (Atom.String trait_name)) ]
    | _ -> []
  in
  VStruct { fields = marker_fields @ List.map (fun (name, ty) -> (name, Public, ty)) fields; partial = false }

let trait_key trait_name args =
  trait_name ^ " " ^ String.concat " " (List.map (fun _ -> "_") args)

let rewrite_record_self_refs record_name params expr =
  let invalid () =
    raise
      (ElabError
         (InvalidRecursiveRecord
            ("recursive record references must be same-instantiation uses of " ^ record_name)))
  in
  let rec collect_apps acc = function
    | Surface.Ap (f, Surface.Explicit, a) -> collect_apps (a :: acc) f
    | f -> (f, acc)
  in
  let shadowed bound name = List.exists (String.equal name) bound in
  let is_decl_param bound param arg =
    match arg with Surface.Var name -> String.equal name param && not (shadowed bound name) | _ -> false
  in
  let param_names params = List.map (fun (param : Surface.param) -> param.name) params in
  let rec go bound expr =
    match collect_apps [] expr with
    | Surface.Var name, args when String.equal name record_name && not (shadowed bound name) ->
        if List.length args = List.length params && List.for_all2 (is_decl_param bound) params args then
          Surface.SelfType
        else invalid ()
    | _ -> (
        match expr with
        | Surface.Ap (f, expl, a) -> Surface.Ap (go bound f, expl, go bound a)
        | Surface.Lam (param, body) -> Surface.Lam (param, go (param.name :: bound) body)
        | Surface.Let { name; type_; value; body; recursive } ->
            Surface.Let
              { name;
                type_ = Option.map (go bound) type_;
                value = go bound value;
                body = go (name :: bound) body;
                recursive }
        | Surface.If { cond; then_; else_ } ->
            Surface.If { cond = go bound cond; then_ = go bound then_; else_ = go bound else_ }
        | Surface.Annotated { inner; typ } -> Surface.Annotated { inner = go bound inner; typ = go bound typ }
        | Surface.Prod elems -> Surface.Prod (List.map (go bound) elems)
        | Surface.ProdTy elems -> Surface.ProdTy (List.map (go bound) elems)
        | Surface.Arrow (expl, name, a, effects, b) ->
            let bound' = match name with Some name -> name :: bound | None -> bound in
            let effects =
              Option.map
                (fun (row : Surface.effect_row) -> { Surface.effects = List.map (go bound') row.effects; tail = Option.map (go bound') row.tail })
                effects
            in
            Surface.Arrow (expl, name, go bound a, effects, go bound' b)
        | Surface.FieldAccess (e, name) -> Surface.FieldAccess (go bound e, name)
        | Surface.Proj (e, i) -> Surface.Proj (go bound e, i)
        | Surface.RecordConstruct { typ; fields } ->
            Surface.RecordConstruct
              { typ = go bound typ; fields = List.map (fun (name, value) -> (name, go bound value)) fields }
        | Surface.Struct { con_fields; bindings } ->
            let binding = function
              | Surface.LetBinding { name; value; public } ->
                  Surface.LetBinding { name; value = go bound value; public }
              | Surface.MethodBinding { name; params; body; public } ->
                  Surface.MethodBinding { name; params; body = go (param_names params @ bound) body; public }
              | Surface.TypeBinding { name; params; ctors; public } ->
                  Surface.TypeBinding
                    { name; params;
                      ctors = List.map (fun (ctor, payload) -> (ctor, Option.map (go (params @ bound)) payload)) ctors;
                      public }
              | Surface.RecordTypeBinding { name; params; fields; public } ->
                  Surface.RecordTypeBinding
                    { name; params;
                      fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                      public }
              | Surface.EffectBinding { name; params; ops; public } ->
                  Surface.EffectBinding
                    { name; params;
                      ops = List.map
                        (fun (op : Surface.effect_op) ->
                          { op with input = go (params @ bound) op.input; output = go (params @ bound) op.output })
                        ops;
                      public }
              | Surface.TraitBinding { name; params; fields; public } ->
                  Surface.TraitBinding
                    { name; params;
                      fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                      public }
              | Surface.ImplBinding { trait_path; trait_name; args; fields; public } ->
                  Surface.ImplBinding
                    { trait_path; trait_name;
                      args = List.map (go bound) args;
                      fields = List.map (fun (field, value) -> (field, go bound value)) fields;
                      public }
            in
            Surface.Struct
              { con_fields = List.map (fun (name, ty) -> (name, go bound ty)) con_fields;
                bindings = List.map binding bindings }
        | Surface.Import _ -> expr
        | Surface.Perform { effect_path; op; arg } -> Surface.Perform { effect_path; op; arg = go bound arg }
        | Surface.Resume arg -> Surface.Resume (go bound arg)
        | Surface.Open (name, body) -> Surface.Open (name, go bound body)
        | Surface.RecordTypeDef { name; params; fields; body } ->
            Surface.RecordTypeDef
              { name; params;
                fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                body = go (name :: bound) body }
        | Surface.TypeDef { name; params; ctors; body } ->
            Surface.TypeDef
              { name; params;
                ctors = List.map (fun (ctor, payload) -> (ctor, Option.map (go (params @ bound)) payload)) ctors;
                body = go (name :: bound) body }
        | Surface.EffectDef { name; params; ops; body } ->
            Surface.EffectDef
              { name; params;
                ops = List.map
                  (fun (op : Surface.effect_op) ->
                    { op with input = go (params @ bound) op.input; output = go (params @ bound) op.output })
                  ops;
                body = go (name :: bound) body }
        | Surface.TraitDef { name; params; fields; body } ->
            Surface.TraitDef
              { name; params;
                fields = List.map (fun (field, ty) -> (field, go (params @ bound) ty)) fields;
                body = go (name :: bound) body }
        | Surface.ImplDef { trait_path; trait_name; args; fields; body } ->
            Surface.ImplDef
              { trait_path; trait_name;
                args = List.map (go bound) args;
                fields = List.map (fun (field, value) -> (field, go bound value)) fields;
                body = go bound body }
        | Surface.Match (scrutinee, branches) ->
            let go_branch = function
              | Surface.ValueBranch (pat, body) -> Surface.ValueBranch (pat, go bound body)
              | Surface.EffectBranch { effect_path; op; arg_pat; body } ->
                  Surface.EffectBranch { effect_path; op; arg_pat; body = go bound body }
            in
            Surface.Match (go bound scrutinee, List.map go_branch branches)
        | Atom _ | Var _ | Self | SelfType -> expr)
  in
  go [] expr

let stdlib_source =
  {|
let (==) : {A : Type} -> A -> A -> Bool =
  fun {A : Type} ->
    match A with
      I64 -> eq_i64
    | Bool -> eq_bool
    | Char -> eq_char
    | Unit -> eq_unit
    | String -> eq_string
    | _ -> panic "equality not defined for this type"
    end
in
let (!=) : {A : Type} -> A -> A -> Bool =
  fun {A : Type} lhs rhs -> not ((==) {A} lhs rhs)
in
()
|}

let parsed_stdlib = lazy (Core_lexer.parse_expr stdlib_source)

let match_domain_of_ty ctx ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VNominal { params; constructors; _ } ->
      let ntp = List.length params in
      Core_match_compile.Nominal
        (List.map (fun (name, payload) -> (name, ntp, Option.is_some payload)) constructors)
  | VAtomTy atom_ty -> Atom atom_ty
  | VU -> Type
  | VProdTy tys -> Product (List.length tys)
  | VStruct { fields; _ } -> Record (List.map fst (visible_record_fields fields))
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
  | OField { parent; name } -> (
      match type_at_occurrence ctx ty parent with
      | Some parent_ty -> (
          match Nbe.force ctx.Ctx.metas parent_ty with
          | VStruct { fields; _ } ->
              visible_record_fields fields
              |> fun fields -> find_record_field fields name
              |> Option.map snd
          | _ -> None)
      | None -> None)

let domain_of_occurrence ctx scrut_ty occ =
  match type_at_occurrence ctx scrut_ty occ with
  | Some ty -> match_domain_of_ty ctx ty
  | None -> Unknown

let term_mentions_var target term =
  let rec go target = function
    | Var ix -> ix = target
    | Lam body | Fix body -> go (target + 1) body
    | Ap (f, _, a) -> go target f || go target a
    | Let (ty, def, body) -> go target ty || go target def || go (target + 1) body
    | Pi { domain; effects; codomain; _ } ->
        go target domain
        || List.exists (go (target + 1)) effects.effects
        || (match effects.tail with Some tail -> go (target + 1) tail | None -> false)
        || go (target + 1) codomain
    | If (cond, then_, else_) -> go target cond || go target then_ || go target else_
    | Prod elems | ProdTy elems -> List.exists (go target) elems
    | Proj (e, _) | Dot (e, _) | Open (e, _) -> go target e
    | Struct { con_fields; bindings; _ } ->
        List.exists (fun (_, ty) -> go target ty) con_fields
        || List.exists
             (function
               | LetBind (_, _, value) -> go target value
               | TypeBind _ | EffectBind _ -> false)
             bindings
    | RecordConstruct { typ; fields } ->
        go target typ || List.exists (fun (_, value) -> go target value) fields
    | NomRef (_, params) | EffectRef (_, params) -> List.exists (go target) params
    | Ctor { spine; nominal_spine; _ } ->
        List.exists (go target) spine || List.exists (go target) nominal_spine
    | Match (scrut, branches) ->
        go target scrut
        || List.exists
             (function
               | ValueBranch (_, body) -> go target body
               | EffectBranch { eff; body; _ } -> go target eff || go target body)
             branches
    | NominalDef { ctors; body; _ } ->
        List.exists (fun (_, payload) -> match payload with Some payload -> go target payload | None -> false) ctors || go target body
    | EffectDef { ops; body; _ } ->
        List.exists (fun (_, input, output) -> go target input || go target output) ops || go target body
    | Perform { eff; arg; _ } -> go target eff || go target arg
    | Atom _ | AtomTy _ | U | Prim _ | Meta _ | InsertedMeta _ | Con _ -> false
  in
  go target term

let rec subst_value_var (mc : MetaContext.t) (target : lvl) (replacement : value) (v : value) : value =
  match Nbe.force mc v with
  | VRigid { lvl; spine } when lvl = target ->
      List.fold_left (Nbe.apply mc) replacement spine
  | VPi { explicitness; domain; effects; codomain } ->
      let domain = subst_value_var mc target replacement domain in
      let effects = subst_effect_row_closure_var mc target replacement effects in
      VPi { explicitness; domain; effects; codomain = subst_closure_var mc target replacement codomain }
  | VProd elems -> VProd (List.map (subst_value_var mc target replacement) elems)
  | VProdTy elems -> VProdTy (List.map (subst_value_var mc target replacement) elems)
  | VStruct { fields; partial } ->
      VStruct { fields = List.map (fun (name, kind, value) -> (name, kind, subst_value_var mc target replacement value)) fields; partial }
  | VRecord { typ; fields } ->
      VRecord { typ = subst_value_var mc target replacement typ; fields = List.map (fun (name, value) -> (name, subst_value_var mc target replacement value)) fields }
  | VNominal n -> VNominal { n with params = List.map (subst_value_var mc target replacement) n.params }
  | VEffect e -> VEffect { e with params = List.map (subst_value_var mc target replacement) e.params }
  | VCon c -> VCon { c with spine = List.map (subst_value_var mc target replacement) c.spine; nominal = subst_value_var mc target replacement c.nominal }
  | VNeutral { ty; neutral } ->
      VNeutral { ty = subst_value_var mc target replacement ty; neutral = subst_neutral_var mc target replacement neutral }
  | VFlex { id; spine } -> VFlex { id; spine = List.map (subst_value_var mc target replacement) spine }
  | VRigid { lvl; spine } -> VRigid { lvl; spine = List.map (subst_value_var mc target replacement) spine }
  | VLam _ | VFix _ | VCont _ as v -> v
  | VU | VAtom _ | VAtomTy _ as v -> v

and subst_closure_var mc target replacement clo =
  { clo with env = List.map (subst_value_var mc target replacement) clo.env }

and subst_effect_row_closure_var mc target replacement row =
  { row with env = List.map (subst_value_var mc target replacement) row.env }

and subst_neutral_var mc target replacement neutral =
  let frames =
    List.map
      (function
        | FApp value -> FApp (subst_value_var mc target replacement value)
        | FIf { then_; else_ } -> FIf { then_ = subst_closure_var mc target replacement then_; else_ = subst_closure_var mc target replacement else_ }
        | FProj _ as frame -> frame
        | FDot _ as frame -> frame
        | FMatch branches -> FMatch (List.map (fun (pat, clo) -> (pat, subst_closure_var mc target replacement clo)) branches))
      neutral.frames
  in
  { neutral with frames }

let rec branch_type_refinement = function
  | Surface.PatType atom_ty -> Some (VAtomTy atom_ty)
  | Surface.PatOr (lhs, rhs) -> (
      match branch_type_refinement lhs with Some _ as found -> found | None -> branch_type_refinement rhs)
  | _ -> None

let refinement_target_of_scrutinee ctx scrut_core =
  match scrut_core with
  | Var ix -> Some (ctx.Ctx.lvl - ix - 1)
  | _ -> None

let refine_context_type_var ctx target replacement =
  let substitute = subst_value_var ctx.Ctx.metas target replacement in
  {
    ctx with
    Ctx.types = List.map substitute ctx.Ctx.types;
    name_table = NameMap.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.name_table;
    self_entry = Option.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.self_entry;
    resume_entry = Option.map (fun entry -> { entry with ty = substitute entry.ty }) ctx.Ctx.resume_entry;
  }

let close_recursive_payload_term nominal_name num_params =
  let rec collect_apps acc = function
    | Ap (f, Explicit, a) -> collect_apps (a :: acc) f
    | f -> (f, acc)
  in
  let rec go cutoff term =
    match collect_apps [] term with
    | Var ix, args when ix = cutoff && List.length args = num_params ->
        NomRef (nominal_name, List.map (go cutoff) args)
    | _ -> (
        match term with
        | Var ix when ix = cutoff ->
            NomRef (nominal_name, List.init num_params (fun i -> Var (num_params - 1 - i)))
        | Var ix when ix > cutoff -> Var (ix - 1)
        | Var ix -> Var ix
        | Lam body -> Lam (go (cutoff + 1) body)
        | Ap (f, expl, a) -> Ap (go cutoff f, expl, go cutoff a)
        | Let (ty, def, body) -> Let (go cutoff ty, go cutoff def, go (cutoff + 1) body)
        | Pi { explicitness; domain; effects; codomain } ->
            Pi
              { explicitness;
                domain = go cutoff domain;
                effects = { effects with effects = List.map (go (cutoff + 1)) effects.effects };
                codomain = go (cutoff + 1) codomain }
        | If (cond, then_, else_) -> If (go cutoff cond, go cutoff then_, go cutoff else_)
        | Prod elems -> Prod (List.map (go cutoff) elems)
        | ProdTy elems -> ProdTy (List.map (go cutoff) elems)
        | Proj (e, i) -> Proj (go cutoff e, i)
        | Dot (e, field) -> Dot (go cutoff e, field)
        | Struct { con_fields; bindings; partial } ->
            let con_fields = List.map (fun (field, ty) -> (field, go cutoff ty)) con_fields in
            let binding = function
              | LetBind (field, kind, value) -> LetBind (field, kind, go cutoff value)
              | TypeBind (field, kind, nominal, ctors) -> TypeBind (field, kind, nominal, ctors)
              | EffectBind (field, kind, eff) -> EffectBind (field, kind, eff)
            in
            Struct { con_fields; bindings = List.map binding bindings; partial }
        | RecordConstruct { typ; fields } ->
            RecordConstruct { typ = go cutoff typ; fields = List.map (fun (field, value) -> (field, go cutoff value)) fields }
        | Open (s, body) -> Open (go cutoff s, go cutoff body)
        | Fix body -> Fix (go (cutoff + 1) body)
        | NomRef (name, params) -> NomRef (name, List.map (go cutoff) params)
        | EffectRef (name, params) -> EffectRef (name, List.map (go cutoff) params)
        | Ctor { name; spine; nominal_name; nominal_spine } ->
            Ctor { name; spine = List.map (go cutoff) spine; nominal_name; nominal_spine = List.map (go cutoff) nominal_spine }
        | Match (scrut, branches) ->
            let go_branch = function
              | ValueBranch (pat, body) -> ValueBranch (pat, go cutoff body)
              | EffectBranch { eff; op; arg_pat; body } ->
                  EffectBranch { eff = go cutoff eff; op; arg_pat; body = go cutoff body }
            in
            Match (go cutoff scrut, List.map go_branch branches)
        | NominalDef { id; name; num_params; ctors; body } ->
            NominalDef
              { id; name; num_params;
                ctors = List.map (fun (ctor, payload) -> (ctor, Option.map (go cutoff) payload)) ctors;
                body = go cutoff body }
        | EffectDef { id; name; num_params; ops; body } ->
            EffectDef
              { id; name; num_params;
                ops = List.map (fun (op, input, output) -> (op, go cutoff input, go cutoff output)) ops;
                body = go cutoff body }
        | Perform { eff; op; arg } -> Perform { eff = go cutoff eff; op; arg = go cutoff arg }
        | Atom _ | AtomTy _ | U | Prim _ | Meta _ | InsertedMeta _ | Con _ as term -> term)
  in
  go 0

let resolve_path_core_value ctx path name =
  match path with
  | [] ->
      let ix, ty = Ctx.lookup ctx name in
      let core = Var ix in
      (core, Ctx.eval ctx core, ty)
  | first :: rest ->
      let ix, ty = Ctx.lookup ctx first in
      let rec go current_core current_value current_ty = function
        | [] -> (current_core, current_value, current_ty)
        | segment :: rest -> (
            match Nbe.force ctx.Ctx.metas current_ty with
            | VStruct { fields; _ } -> (
                match List.find_opt (fun (n, k, _) -> String.equal n segment && k <> Private && k <> PrivateMethod) fields with
                | Some (_, _, field_ty) ->
                    let next_core = Dot (current_core, segment) in
                    let next_value = Nbe.dot_value current_value segment in
                    go next_core next_value field_ty rest
                | None -> raise (ElabError (UnboundVariable segment)))
            | _ -> raise (ElabError (UnboundVariable segment)))
      in
      go (Var ix) (Ctx.eval ctx (Var ix)) ty (rest @ [ name ])

let resolve_path_value ctx path name =
  let _, value, ty = resolve_path_core_value ctx path name in
  (value, ty)

let lookup_trait ctx name =
  match NameMap.find_opt name ctx.Ctx.traits with
  | Some info -> info
  | None -> raise (ElabError (UnknownTrait name))

let eval_trait_fields ctx trait_info args =
  List.map
    (fun (field, clo) -> (field, Nbe.eval ctx.Ctx.metas (List.rev args @ clo.env) clo.body))
    trait_info.trait_fields

let resolve_trait_evidence ctx trait_info args =
  let matches =
    List.filter
      (fun evidence ->
        evidence.evidence_trait_id = trait_info.trait_id
        && List.length evidence.evidence_args = List.length args
        && List.for_all2 (Ctx.conv ctx) evidence.evidence_args args)
      ctx.Ctx.trait_evidence
  in
  match matches with
  | [ evidence ] -> (Var (Nbe.lvl_to_ix ctx.Ctx.lvl evidence.evidence_level), evidence.evidence_ty)
  | [] -> raise (ElabError (UnknownTrait trait_info.trait_name))
  | _ -> raise (ElabError (AmbiguousTraitImplementation (trait_key trait_info.trait_name args)))

let resolve_trait_dict_ty ctx = function
  | VStruct { fields; _ } -> (
      match fields with
      | ("__arg", Private, arg) :: ("__trait", Private, VAtom (Atom.String trait_name)) :: _ ->
          let trait_info = lookup_trait ctx trait_name in
          let fields = eval_trait_fields ctx trait_info [ arg ] in
          Some (trait_info, [ arg ], trait_dict_ty trait_info.trait_name [ arg ] fields)
      | _ -> None)
  | _ -> None

let resolve_trait_method ctx trait_info method_name =
  match List.find_opt (fun evidence -> evidence.evidence_trait_id = trait_info.trait_id) ctx.Ctx.trait_evidence with
  | None -> raise (ElabError (UnknownTrait trait_info.trait_name))
  | Some evidence -> (
      match Nbe.force ctx.Ctx.metas evidence.evidence_ty with
      | VStruct { fields; _ } -> (
          match List.find_opt (fun (name, kind, _) -> String.equal name method_name && kind <> Private && kind <> PrivateMethod) fields with
          | Some (_, _, method_ty) -> (Dot (Var (Nbe.lvl_to_ix ctx.Ctx.lvl evidence.evidence_level), method_name), method_ty)
          | None -> raise (ElabError (UnknownTraitMethod method_name)))
      | _ -> raise (ElabError (UnknownTraitMethod method_name)))

let find_nominal_template ctx path name =
  let scan_env () =
    List.find_map
      (function
        | VNominal n when String.equal n.name name -> Some (VNominal n)
        | _ -> None)
      ctx.Ctx.env
  in
  match path with
  | [] -> (
      let value, _ = resolve_path_value ctx [] name in
      match Nbe.force ctx.Ctx.metas value with
      | VNominal _ as nominal -> nominal
      | _ -> (
          match scan_env () with
          | Some nominal -> nominal
          | None -> raise (ElabError (UnknownConstructor name))))
  | _ ->
      let value, _ = resolve_path_value ctx path name in
      match Nbe.force ctx.Ctx.metas value with
      | VNominal _ as nominal -> nominal
      | _ -> raise (ElabError (UnknownConstructor name))

let starts_lowercase name =
  String.length name > 0 && Char.lowercase_ascii name.[0] = name.[0]

let rec refinement_for_nominal_head ctx = function
  | Surface.PatCon (path, name, _) -> (
      try
        match find_nominal_template ctx path name with
        | VNominal n -> Some (VNominal { n with params = List.init n.num_params (fun _ -> Ctx.raw_meta ctx) })
        | _ -> None
      with _ -> None)
  | Surface.PatOr (lhs, rhs) -> (
      match refinement_for_nominal_head ctx lhs with
      | Some _ as found -> found
      | None -> refinement_for_nominal_head ctx rhs)
  | _ -> None

let refine_branch_context ctx refinement_target pat =
  match (branch_type_refinement pat, refinement_target) with
  | Some replacement, Some target -> refine_context_type_var ctx target replacement
  | None, Some target -> (
      match refinement_for_nominal_head ctx pat with
      | Some replacement -> refine_context_type_var ctx target replacement
      | None -> ctx)
  | _ -> ctx

let refine_branch_expected ctx refinement_target pat expected =
  match (branch_type_refinement pat, refinement_target) with
  | Some replacement, Some target -> subst_value_var ctx.Ctx.metas target replacement expected
  | None, Some target -> (
      match refinement_for_nominal_head ctx pat with
      | Some replacement -> subst_value_var ctx.Ctx.metas target replacement expected
      | None -> expected)
  | _ -> expected

let union_expr_effects ctx lhs rhs =
  let add acc eff =
    if List.exists (fun existing -> Ctx.conv ctx existing.value eff.value) acc then acc else eff :: acc
  in
  List.rev (List.fold_left add (List.rev lhs) rhs)

let union_many_expr_effects ctx effs = List.fold_left (union_expr_effects ctx) empty_expr_effects effs

let require_empty_effects effects =
  if effects <> [] then raise (ElabError UnhandledEffects)

let effect_row_values ctx row binder =
  match row.tail with
  | Some _ -> raise (ElabError UnhandledEffects)
  | None -> List.map (fun eff -> Nbe.eval ctx.Ctx.metas (binder :: row.env) eff) row.effects

let check_effect_subset ctx actual expected =
  List.iter
    (fun eff ->
      if not
           (List.exists
              (fun expected ->
                try
                  Ctx.unify ctx eff.value expected;
                  true
                with Unify.UnifyError _ -> false)
              expected)
      then raise (ElabError UnhandledEffects))
    actual

let effect_row_of_expr_effects ctx effects =
  { effects = List.map (fun eff -> Ctx.quote ctx eff.value) effects; tail = None }

let rec surface_trait_bound_names = function
  | Surface.Var name -> Some [ name ]
  | Surface.Ap (Surface.Ap (Surface.Var "+", Surface.Explicit, lhs), Surface.Explicit, rhs) -> (
      match surface_trait_bound_names lhs, surface_trait_bound_names rhs with
      | Some lhs, Some rhs -> Some (lhs @ rhs)
      | _ -> None)
  | _ -> None

let trait_marker_ty trait_name arg =
  VStruct
    { fields =
        [ ("__arg", Private, arg);
          ("__trait", Private, VAtom (Atom.String trait_name)) ];
      partial = false }

let infer_ref : (Ctx.t -> Surface.t -> term * value) ref =
  ref (fun _ _ -> failwith "infer not initialized")

let check_ref : (Ctx.t -> Surface.t -> value -> term) ref =
  ref (fun _ _ _ -> failwith "check not initialized")

let collect_effects_ref : (Ctx.t -> Surface.t -> expr_effects) ref =
  ref (fun _ _ -> failwith "collect_effects not initialized")

let elaborate_trait ctx name params fields =
  check_duplicate_trait_fields fields;
  let param_ctx =
    List.fold_left
      (fun ctx param_name ->
        Ctx.define ctx param_name VU (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
      ctx params
  in
  let field_terms =
    List.map
      (fun (field_name, ty_expr) ->
        let ty_core, ty_ty = !infer_ref param_ctx ty_expr in
        check_type_like param_ctx ty_ty (Ctx.eval param_ctx ty_core);
        (field_name, ty_core))
      fields
  in
  let trait_info =
    { trait_id = fresh_trait_id ();
      trait_name = name;
      trait_params = params;
      trait_fields = List.map (fun (field_name, ty_core) -> (field_name, { env = ctx.Ctx.env; body = ty_core })) field_terms }
  in
  let trait_ty = VAtom (Atom.String ("trait:" ^ name)) in
  (trait_info, trait_ty)

let elaborate_impl ctx trait_name args fields =
  let trait_info = lookup_trait ctx trait_name in
  let arg_cores =
    List.map
      (fun arg ->
        let arg_core, arg_ty = !infer_ref ctx arg in
        check_type_like ctx arg_ty (Ctx.eval ctx arg_core);
        arg_core)
      args
  in
  let arg_values = List.map (Ctx.eval ctx) arg_cores in
  let expected_fields = eval_trait_fields ctx trait_info arg_values in
  let expected_dict_ty = trait_dict_ty trait_name arg_values expected_fields in
  check_duplicate_names (List.map fst fields);
  List.iter
    (fun (name, _) ->
      if Option.is_none (List.assoc_opt name expected_fields) then
        raise (ElabError (UnknownTraitMethod name)))
    fields;
  List.iter
    (fun (name, _) ->
      if Option.is_none (List.assoc_opt name fields) then raise (ElabError (MissingTraitField name)))
    expected_fields;
  let impl_effects = List.map (fun (_, value) -> !collect_effects_ref ctx value) fields in
  let field_cores =
    List.map
      (fun (name, value) ->
        let field_ty =
          match List.assoc_opt name expected_fields with
          | Some ty -> ty
          | None -> raise (ElabError (UnknownTraitMethod name))
        in
        (name, !check_ref ctx value field_ty))
      fields
  in
  let impl_core = Struct { con_fields = []; bindings = List.map (fun (name, value) -> LetBind (name, Public, value)) field_cores; partial = false } in
  let impl_name = "__impl_" ^ trait_name in
  let impl_value = VNeutral { ty = expected_dict_ty; neutral = { head = HVar ctx.Ctx.lvl; frames = [] } } in
  let ctx' = Ctx.define ctx impl_name expected_dict_ty impl_value in
  let evidence =
    { evidence_trait_id = trait_info.trait_id;
      evidence_trait_name = trait_name;
      evidence_args = arg_values;
      evidence_level = ctx.Ctx.lvl;
      evidence_ty = expected_dict_ty }
  in
  (Ctx.add_trait_evidence ctx' evidence, impl_effects, impl_name, expected_dict_ty, impl_core)

let rec insert_explicit_effect_metas ctx core ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VPi { explicitness = Explicit; codomain; _ } ->
      let arg_core = Ctx.fresh_meta ctx in
      let arg_value = Ctx.eval ctx arg_core in
      insert_explicit_effect_metas ctx
        (Ap (core, Explicit, arg_core))
        (Nbe.closure_apply ctx.Ctx.metas codomain arg_value)
  | _ -> (core, ty)

let resolve_perform_operation ctx ~effect_path ~op =
  match List.rev effect_path with
  | [] -> raise (ElabError EffectOperationPathExpected)
  | effect_name :: rev_path ->
      let path = List.rev rev_path in
      let effect_core, _effect_value, effect_ty = resolve_path_core_value ctx path effect_name in
      let effect_core, _effect_ty = insert_explicit_effect_metas ctx effect_core effect_ty in
      let effect_value = Ctx.eval ctx effect_core in
      match Nbe.force ctx.Ctx.metas effect_value with
      | VEffect eff -> (
          match List.find_opt (fun (name, _, _) -> String.equal name op) eff.operations with
          | Some (_, input, output) ->
              let env = List.rev eff.params @ input.env in
              let input_ty = Nbe.eval ctx.Ctx.metas env input.body in
              let output_ty = Nbe.eval ctx.Ctx.metas (List.rev eff.params @ output.env) output.body in
              (effect_core, VEffect eff, input_ty, output_ty)
          | None -> raise (ElabError (UnknownEffectOperation op)))
      | _ -> raise (ElabError ExpectedEffect)

let rec insert_implicit_args ctx core ty =
  match Nbe.force ctx.Ctx.metas ty with
  | VPi { explicitness = Implicit; domain; codomain; _ } ->
      let arg_core_opt =
        match resolve_trait_dict_ty ctx domain with
        | Some (trait_info, args, _) -> (
            try Some (fst (resolve_trait_evidence ctx trait_info args))
            with ElabError (UnknownTrait _) -> None)
        | None -> Some (Ctx.fresh_meta ctx)
      in
      (match arg_core_opt with
      | None -> (core, ty)
      | Some arg_core ->
          let arg_value = Ctx.eval ctx arg_core in
          insert_implicit_args ctx
            (Ap (core, Implicit, arg_core))
            (Nbe.closure_apply ctx.Ctx.metas codomain arg_value))
  | _ -> (core, ty)

let surface_value_branches branches =
  List.filter_map (function
    | Surface.ValueBranch (pat, body) -> Some (pat, body)
    | Surface.EffectBranch _ -> None)
    branches

let core_value_branches branches =
  List.filter_map (function
    | ValueBranch (pat, body) -> Some (pat, body)
    | EffectBranch _ -> None)
    branches

type surface_effect_branch = {
  effect_path : string list;
  op : string;
  arg_pat : Surface.pat;
  body : Surface.t;
}

let surface_effect_branches branches =
  List.filter_map (function
    | Surface.ValueBranch _ -> None
    | Surface.EffectBranch { effect_path; op; arg_pat; body } ->
        Some { effect_path; op; arg_pat; body })
    branches

let expr_effect_of_value ctx value = { core = Ctx.quote ctx value; value }

let remove_expr_effect ctx handled effects =
  List.filter (fun candidate -> not (Ctx.conv ctx candidate.value handled.value)) effects

let effect_instance_ops = function
  | VEffect eff -> List.map (fun (name, _, _) -> name) eff.operations
  | _ -> []

let effect_row_closure_of_expr_effects ctx effects =
  effect_row_closure ctx.Ctx.env (effect_row_of_expr_effects ctx effects)

let refine_match_scrutinee_ty ctx scrut_ty branches =
  let ty = Nbe.force ctx.Ctx.metas scrut_ty in
  match ty with
  | VNominal _ | VAtomTy _ | VProdTy _ -> ty
  | _ ->
      let rec find_pat = function
        | Surface.PatCon (path, name, _) ->
            let ctor_value, ctor_ty = resolve_path_value ctx path name in
            (match Nbe.force ctx.Ctx.metas ctor_value with
            | VNominal _ -> Some VU
            | _ -> Some (nominal_from_constructor_type ctx ctor_ty))
        | Surface.PatAtom atom -> Some (VAtomTy (atom_ty_of_atom atom))
        | Surface.PatType _ -> Some VU
        | Surface.PatProd ps ->
            Some (VProdTy (List.map (fun _ -> Ctx.raw_meta ctx) ps))
        | Surface.PatRecord { typ_path; typ; _ } ->
            let record_value, ty = resolve_path_value ctx typ_path typ in
            (match Nbe.force ctx.Ctx.metas ty with
            | VU -> Some record_value
            | VStruct _ as record_ty -> Some record_ty
            | _ -> None)
        | Surface.PatStructType _ -> (
            match ty with VStruct _ -> Some ty | _ -> Some VU)
        | Surface.PatOr (lhs, rhs) -> (
            match find_pat lhs with Some _ as found -> found | None -> find_pat rhs)
        | PatWild | PatBind _ -> None
      in
      let rec find = function
        | [] -> raise (ElabError NotANominalType)
        | (pat, _) :: rest -> (
            match find_pat pat with
            | Some target ->
                unify_scrutinee_ty ctx ty target;
                Nbe.force ctx.Ctx.metas ty
            | None -> find rest)
      in
      find branches

let maybe_refine_match_scrutinee_ty ctx scrut_ty branches =
  try refine_match_scrutinee_ty ctx scrut_ty branches with ElabError NotANominalType -> scrut_ty

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
let rec elaborate_effect_row (ctx : Ctx.t) = function
  | None -> empty_effect_row
  | Some (row : Surface.effect_row) ->
      let entries =
        List.map
          (fun eff_expr ->
            let eff_core, eff_ty = infer ctx eff_expr in
            Ctx.unify ctx eff_ty VU;
            let eff_value = Ctx.eval ctx eff_core in
            match Nbe.force ctx.metas eff_value with
            | VEffect _ -> (eff_core, eff_value)
            | _ -> raise (ElabError ExpectedEffect))
          row.effects
      in
      let rec check_unique = function
        | [] -> ()
        | (_, eff_value) :: rest ->
            if List.exists (fun (_, other) -> Ctx.conv ctx eff_value other) rest then
              raise (ElabError DuplicateEffect);
            check_unique rest
      in
      check_unique entries;
      { effects = List.map fst entries; tail = None }

and collect_effects (ctx : Ctx.t) (expr : Surface.t) : expr_effects =
  match expr with
  | Surface.Perform { effect_path; op; arg } ->
      let effect_core, effect_value, input_ty, _output_ty = resolve_perform_operation ctx ~effect_path ~op in
      let _arg_core = check ctx arg input_ty in
      union_expr_effects ctx (collect_effects ctx arg) (singleton_expr_effect effect_core effect_value)
  | Surface.Resume arg -> collect_effects ctx arg
  | Surface.Ap (f, Surface.Explicit, a) ->
      let f_core, f_ty = infer ctx f in
      let _f_core, f_ty = insert_implicit_args ctx f_core f_ty in
      let f_ty = Nbe.force ctx.Ctx.metas f_ty in
      let latent, _arg_core =
        match f_ty with
        | VPi { explicitness = Explicit; domain = a_ty; effects; _ } ->
            let arg_core = check ctx a a_ty in
            let latent =
              match effects.effects, effects.tail with
              | [], None -> empty_expr_effects
              | _ ->
                  let arg_val = Ctx.eval ctx arg_core in
                  List.map (fun value -> { core = Ctx.quote ctx value; value }) (effect_row_values ctx effects arg_val)
            in
            (latent, arg_core)
        | _ -> (empty_expr_effects, Atom Atom.Unit)
      in
      union_many_expr_effects ctx [ collect_effects ctx f; collect_effects ctx a; latent ]
  | Surface.Ap (f, Surface.Implicit, a) ->
      union_expr_effects ctx (collect_effects ctx f) (collect_effects ctx a)
  | Surface.Lam _ -> empty_expr_effects
  | Surface.Let { name; type_; value; body; recursive = false } ->
      let value_effects = collect_effects ctx value in
      let value_core, value_ty =
        match type_ with
        | Some ty_expr ->
            require_empty_effects (collect_effects ctx ty_expr);
            let ty_core, ty_ty = infer ctx ty_expr in
            let ty_val = Ctx.eval ctx ty_core in
            check_type_like ctx ty_ty ty_val;
            (check ctx value ty_val, ty_val)
        | None -> infer ctx value
      in
      let gen_value_core, gen_value_ty = generalize ctx value_core value_ty in
      let body_ctx =
        if value_effects = [] then Ctx.define ctx name gen_value_ty (Ctx.eval ctx gen_value_core)
        else Ctx.bind ctx name gen_value_ty
      in
      let body_effects = collect_effects body_ctx body in
      union_expr_effects ctx value_effects body_effects
  | Surface.Let { name; type_; value; body; recursive = true } ->
      let rec_ty =
        match type_ with
        | Some ty_expr ->
            require_empty_effects (collect_effects ctx ty_expr);
            let ty_core, ty_ty = infer ctx ty_expr in
            let ty_val = Ctx.eval ctx ty_core in
            check_type_like ctx ty_ty ty_val;
            ty_val
        | None -> Ctx.raw_meta ctx
      in
      let fix_core = Fix (check (Ctx.bind ctx name rec_ty) value rec_ty) in
      let fix_val = Ctx.eval ctx fix_core in
      union_expr_effects ctx (collect_effects (Ctx.bind ctx name rec_ty) value) (collect_effects (Ctx.define ctx name rec_ty fix_val) body)
  | Surface.If { cond; then_; else_ } ->
      union_many_expr_effects ctx [ collect_effects ctx cond; collect_effects ctx then_; collect_effects ctx else_ ]
  | Surface.Annotated { inner; typ } ->
      require_empty_effects (collect_effects ctx typ);
      collect_effects ctx inner
  | Surface.Prod elems | Surface.ProdTy elems -> union_many_expr_effects ctx (List.map (collect_effects ctx) elems)
  | Surface.Arrow (Surface.Implicit, Some name, a, row, b) -> (
      match surface_trait_bound_names a with
      | Some trait_names when List.for_all (fun trait_name -> NameMap.mem trait_name ctx.Ctx.traits) trait_names ->
          let type_ctx = Ctx.bind ctx name VU in
          let arg = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
          let dict_ctx =
            List.fold_left
              (fun c trait_name ->
                let trait_info = lookup_trait ctx trait_name in
                let dict_ty = trait_dict_ty trait_name [ arg ] (eval_trait_fields ctx trait_info [ arg ]) in
                let c', entry = Ctx.bind_anonymous c dict_ty in
                let evidence =
                  { evidence_trait_id = trait_info.trait_id;
                    evidence_trait_name = trait_name;
                    evidence_args = [ arg ];
                    evidence_level = entry.level;
                    evidence_ty = dict_ty }
                in
                Ctx.add_trait_evidence c' evidence)
              type_ctx trait_names
          in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects (collect_effects dict_ctx eff)) row.effects) row;
          require_empty_effects (collect_effects dict_ctx b)
      | _ ->
          require_empty_effects (collect_effects ctx a);
          let a_core, a_ty = infer ctx a in
          let a_val = Ctx.eval ctx a_core in
          check_type_like ctx a_ty a_val;
          let ctx' = Ctx.bind ctx name a_val in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects (collect_effects ctx' eff)) row.effects) row;
          require_empty_effects (collect_effects ctx' b));
      empty_expr_effects
  | Surface.Arrow (_, name, a, row, b) ->
      require_empty_effects (collect_effects ctx a);
      let a_core, a_ty = infer ctx a in
      let a_val = Ctx.eval ctx a_core in
      check_type_like ctx a_ty a_val;
      let ctx' = Ctx.bind ctx (Option.value name ~default:"_") a_val in
      Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects (collect_effects ctx' eff)) row.effects) row;
      require_empty_effects (collect_effects ctx' b);
      empty_expr_effects
  | Surface.FieldAccess (e, _) | Surface.Proj (e, _) -> collect_effects ctx e
  | Surface.RecordConstruct { typ; fields } ->
      union_many_expr_effects ctx (collect_effects ctx typ :: List.map (fun (_, value) -> collect_effects ctx value) fields)
  | Surface.Struct _ ->
      let core, _ty = infer ctx expr in
      let value = Ctx.eval ctx core in
      (match Nbe.force ctx.Ctx.metas value with
      | VStruct { fields; _ } ->
          fields
          |> List.filter_map (fun (_, _, value) -> match Nbe.force ctx.Ctx.metas value with VPi { effects; _ } -> Some effects.effects | _ -> None)
          |> List.concat
          |> List.map (fun core -> { core; value = Ctx.eval ctx core })
      | _ -> empty_expr_effects)
  | Surface.Open (name, body) ->
      let ix, ty = Ctx.lookup ctx name in
      let value = Ctx.eval ctx (Var ix) in
      let ctx' =
        match (Nbe.force ctx.Ctx.metas ty, Nbe.force ctx.Ctx.metas value) with
        | VStruct { fields = type_fields; _ }, VStruct { fields = value_fields; _ } ->
            List.fold_left
              (fun c (fname, k, fty) ->
                if k = Public || k = Method then
                  match List.find_opt (fun (vname, vk, _) -> String.equal fname vname && vk = k) value_fields with
                  | Some (_, _, value) -> Ctx.define c fname fty value
                  | None -> c
                else c)
              ctx type_fields
        | _ -> ctx
      in
      collect_effects ctx' body
  | Surface.RecordTypeDef { fields; body; _ } ->
      union_many_expr_effects ctx (List.map (fun (_, ty) -> collect_effects ctx ty) fields @ [ collect_effects ctx body ])
  | Surface.TypeDef { ctors; body; _ } ->
      union_many_expr_effects ctx (List.filter_map (fun (_, payload) -> Option.map (collect_effects ctx) payload) ctors @ [ collect_effects ctx body ])
  | Surface.EffectDef { name; params; ops; body } ->
      let _effect_id, eff, eff_ty, _elaborated_ops = elaborate_eff_family ctx name params ops in
      let op_effects = List.concat_map (fun (op : Surface.effect_op) -> [ collect_effects ctx op.input; collect_effects ctx op.output ]) ops in
      union_many_expr_effects ctx (op_effects @ [ collect_effects (Ctx.define ctx name eff_ty eff) body ])
  | Surface.TraitDef { name; params; fields; body } ->
      let trait_info, trait_ty = elaborate_trait ctx name params fields in
      let body_ctx = Ctx.add_trait (Ctx.define ctx name VU trait_ty) trait_info in
      union_many_expr_effects ctx (List.map (fun (_, ty) -> collect_effects ctx ty) fields @ [ collect_effects body_ctx body ])
  | Surface.ImplDef { trait_path = []; trait_name; args; fields; body } ->
      let ctx', impl_effects, _impl_name, _impl_ty, _impl_core = elaborate_impl ctx trait_name args fields in
      union_many_expr_effects ctx (impl_effects @ [ collect_effects ctx' body ])
  | Surface.ImplDef { trait_path = _ :: _; trait_name; _ } ->
      raise (ElabError (UnknownTrait trait_name))
  | Surface.Match (scrutinee, branches) ->
      let value_branches = surface_value_branches branches in
      let effect_branches = surface_effect_branches branches in
      let scrut_core, scrut_ty = infer ctx scrutinee in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let scrutinee_effects = collect_effects ctx scrutinee in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let ret_ty = Ctx.raw_meta ctx in
      let value_branch_effects =
        List.map
          (fun (pat, body) ->
            let branch_ctx = refine_branch_context ctx refinement_target pat in
            let _core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
            collect_effects ctx' body)
          value_branches
      in
      let effect_branch_effects =
        List.map
          (fun branch ->
            let _effect_core, _effect_value, input_ty, output_ty =
              resolve_effect_branch_operation ctx scrutinee_effects branch
            in
            let _core_pat, arg_ctx = elaborate_pat ctx branch.arg_pat input_ty in
            let cont_ty =
              VPi
                { explicitness = Explicit;
                  domain = output_ty;
                  effects = effect_row_closure_of_expr_effects arg_ctx residual;
                  codomain = { env = arg_ctx.env; body = Ctx.quote arg_ctx ret_ty } }
            in
            let branch_ctx, resume_entry = Ctx.bind_anonymous arg_ctx cont_ty in
            collect_effects { branch_ctx with Ctx.resume_entry = Some resume_entry } branch.body)
          effect_branches
      in
      union_many_expr_effects ctx (residual :: value_branch_effects @ effect_branch_effects)
  | Atom _ | Var _ | Self | SelfType | Import _ -> empty_expr_effects

and infer (ctx : Ctx.t) (expr : Surface.t) : term * value =
  match expr with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy TI64)
  | Atom (Bool b) -> (Atom (Bool b), VAtomTy TBool)
  | Atom Unit -> (Atom Unit, VAtomTy TUnit)
  | Atom (Char c) -> (Atom (Char c), VAtomTy TChar)
  | Atom (String s) -> (Atom (String s), VAtomTy TString)
  | Var name ->
      let ix, ty = Ctx.lookup ctx name in
      (Var ix, ty)
  | Self ->
      let ix, ty = Ctx.lookup_self ctx in
      (Var ix, ty)
  | SelfType ->
      (Ctx.quote ctx (Ctx.lookup_self_type ctx), VU)
  | Perform { effect_path; op; arg } ->
      let effect_core, _effect_value, input_ty, output_ty = resolve_perform_operation ctx ~effect_path ~op in
      let arg_core = check ctx arg input_ty in
      (Perform { eff = effect_core; op; arg = arg_core }, Nbe.force ctx.metas output_ty)
  | Resume arg -> infer_resume ctx arg
  | Ap (f, Surface.Explicit, a) -> infer_ap ctx f a
  | Ap (f, Surface.Implicit, a) -> infer_ap_implicit ctx f a
  | Let { name; type_; value; body; recursive } ->
      infer_let ctx name type_ value body recursive
  | If { cond; then_; else_ } -> infer_if ctx cond then_ else_
  | Lam (param, body) -> infer_lam ctx param body
  | Annotated { inner; typ } ->
      require_empty_effects (collect_effects ctx typ);
      let ty_core, ty_ty = infer ctx typ in
      let ty_val = Ctx.eval ctx ty_core in
      check_type_like ctx ty_ty ty_val;
      let core = check ctx inner ty_val in
      (core, ty_val)
  | Prod elems ->
      let cores_tys = List.map (infer ctx) elems in
      let cores = List.map fst cores_tys in
      let tys = List.map snd cores_tys in
      (Prod cores, VProdTy tys)
  | ProdTy elems ->
      let core_elems =
        List.map
          (fun elem ->
            require_empty_effects (collect_effects ctx elem);
            let elem_core, elem_ty = infer ctx elem in
            check_type_like ctx elem_ty (Ctx.eval ctx elem_core);
            elem_core)
          elems
      in
      (ProdTy core_elems, VU)
  | Arrow (Surface.Implicit, Some name, a, effects, b) -> (
      match surface_trait_bound_names a with
      | Some trait_names when List.for_all (fun trait_name -> NameMap.mem trait_name ctx.Ctx.traits) trait_names ->
          let type_ctx = Ctx.bind ctx name VU in
          let arg = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
          let dict_ctx, dict_layers =
            List.fold_left
              (fun (c, layers) trait_name ->
                let trait_info = lookup_trait ctx trait_name in
                let dict_ty = trait_dict_ty trait_name [ arg ] (eval_trait_fields ctx trait_info [ arg ]) in
                let dict_core = Ctx.quote c dict_ty in
                let c', entry = Ctx.bind_anonymous c dict_ty in
                let evidence =
                  { evidence_trait_id = trait_info.trait_id;
                    evidence_trait_name = trait_name;
                    evidence_args = [ arg ];
                    evidence_level = entry.level;
                    evidence_ty = dict_ty }
                in
                (Ctx.add_trait_evidence c' evidence, layers @ [ dict_core ]))
              (type_ctx, []) trait_names
          in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects (collect_effects dict_ctx eff)) row.effects) effects;
          let effects = elaborate_effect_row dict_ctx effects in
          require_empty_effects (collect_effects dict_ctx b);
          let b_core, b_ty = infer dict_ctx b in
          check_type_like dict_ctx b_ty (Ctx.eval dict_ctx b_core);
          let core =
            Pi
              { explicitness = Implicit;
                domain = U;
                effects;
                codomain =
                  List.fold_right
                    (fun dict_core codomain ->
                      Pi { explicitness = Implicit; domain = dict_core; effects = empty_effect_row; codomain })
                    dict_layers b_core }
          in
          (core, VU)
      | _ ->
          require_empty_effects (collect_effects ctx a);
          let a_core, a_ty = infer ctx a in
          let a_val = Ctx.eval ctx a_core in
          check_type_like ctx a_ty a_val;
          let ctx' = Ctx.bind ctx name a_val in
          Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects (collect_effects ctx' eff)) row.effects) effects;
          let effects = elaborate_effect_row ctx' effects in
          require_empty_effects (collect_effects ctx' b);
          let b_core, b_ty = infer ctx' b in
          check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
          (Pi { explicitness = Implicit; domain = a_core; effects; codomain = b_core }, VU))
  | Arrow (expl, name, a, effects, b) ->
      require_empty_effects (collect_effects ctx a);
      let a_core, a_ty = infer ctx a in
      let a_val = Ctx.eval ctx a_core in
      check_type_like ctx a_ty a_val;
      let ctx' = Ctx.bind ctx (Option.value name ~default:"_") a_val in
      Option.iter (fun (row : Surface.effect_row) -> List.iter (fun eff -> require_empty_effects (collect_effects ctx' eff)) row.effects) effects;
      let effects = elaborate_effect_row ctx' effects in
      require_empty_effects (collect_effects ctx' b);
      let b_core, b_ty = infer ctx' b in
      check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
      (Pi { explicitness = expl_of_surface expl; domain = a_core; effects; codomain = b_core }, VU)
  | FieldAccess (Var trait_name, name) when NameMap.mem trait_name ctx.Ctx.traits ->
      resolve_trait_method ctx (lookup_trait ctx trait_name) name
  | FieldAccess (e, name) ->
      let e_core, e_ty = infer ctx e in
      let e_core, e_ty = insert_implicit_args ctx e_core e_ty in
      (match Nbe.force ctx.metas e_ty with
      | VStruct { fields; partial } ->
          (match List.find_opt (fun (n, k, _) -> String.equal n name && k <> Private && k <> PrivateMethod) fields with
          | Some (_, _, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
          | None when partial ->
              let result_ty = Ctx.raw_meta ctx in
              let constraint_ty =
                VStruct { fields = fields @ [ (name, Field, result_ty) ]; partial = true }
              in
              Ctx.unify ctx e_ty constraint_ty;
              (Dot (e_core, name), result_ty)
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
          (Proj (e_core, i), Nbe.force ctx.metas (List.nth tys i))
      | _ -> raise (ElabError ApplyingNonFunction))
  | Import path -> (
      match ctx.loader with
      | Some loader -> Core_loader.load loader path (fun imported -> infer ctx imported)
      | None -> raise (ElabError (ImportRequiresLoader path)))
  | RecordConstruct { typ; fields } ->
      let typ_core, typ_ty = infer ctx typ in
      let typ_core, typ_ty = insert_implicit_args ctx typ_core typ_ty in
      (match Nbe.force ctx.metas typ_ty with
      | VStruct { fields = struct_fields; _ } as record_ty ->
          let record_fields = visible_record_fields struct_fields in
          check_duplicate_names (List.map fst fields);
          List.iter
            (fun (name, _) ->
              if Option.is_none (find_record_field record_fields name) then
                raise (ElabError (UnknownRecordField name)))
            fields;
          List.iter
            (fun (name, _) ->
              if Option.is_none (List.assoc_opt name fields) then
                raise (ElabError (MissingRecordField name)))
            record_fields;
          let field_cores =
            List.map
              (fun (name, value) ->
                let field_ty =
                  match find_record_field record_fields name with
                  | Some (_, ty) -> ty
                  | None -> raise (ElabError (UnknownRecordField name))
                in
                (name, check ctx value field_ty))
              fields
          in
          (RecordConstruct { typ = typ_core; fields = field_cores }, record_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Struct { con_fields; bindings } ->
      let con_cores =
        List.map (fun (name, ty_expr) ->
          let ty_core, ty_ty = infer ctx ty_expr in
          let ty_value = Ctx.eval ctx ty_core in
          check_type_like ctx ty_ty ty_value;
          (name, ty_core, ty_value))
        con_fields
      in
      let self_ty =
        VStruct {
          fields = List.map (fun (name, _, ty) -> (name, Field, ty)) con_cores;
          partial = true;
        }
      in
      let binding_ctx = Ctx.with_self_type ctx self_ty in
      let rec elaborate_method_params ctx params body =
        match params with
        | [] ->
            let body_core, body_ty = infer ctx body in
            (body_core, body_ty)
        | param :: rest ->
            let a_ty =
              match param.Surface.type_ with
              | Some ty_expr ->
                  let ty_core, ty_ty = infer ctx ty_expr in
                  let ty_value = Ctx.eval ctx ty_core in
                  check_type_like ctx ty_ty ty_value;
                  ty_value
              | None -> Ctx.raw_meta ctx
            in
            let ctx' = Ctx.bind ctx param.name a_ty in
            let body_core, body_ty = elaborate_method_params ctx' rest body in
            let body_ty_term = Ctx.quote ctx' body_ty in
            let method_ty =
              VPi {
                explicitness = expl_of_surface param.explicitness;
                domain = a_ty;
                effects = effect_row_closure ctx.env empty_effect_row;
                codomain = { env = ctx.env; body = body_ty_term };
              }
            in
            (Lam body_core, method_ty)
      in
      let elaborate_method ctx params body =
        let self_ctx, self_entry = Ctx.bind_anonymous ctx self_ty in
        let self_ctx = { self_ctx with Ctx.self_entry = Some self_entry } in
        let body_core, body_ty = elaborate_method_params self_ctx params body in
        let body_ty_term = Ctx.quote self_ctx body_ty in
        let method_ty =
          VPi {
            explicitness = Explicit;
            domain = self_ty;
            effects = effect_row_closure ctx.env empty_effect_row;
            codomain = { env = ctx.env; body = body_ty_term };
          }
        in
        (Lam body_core, method_ty)
      in
      let rec go ctx (acc_binds, acc_fields) = function
        | [] -> (List.rev acc_binds, List.rev acc_fields)
        | Surface.LetBinding { name; value; public } :: rest ->
            let value_ctx = Ctx.clear_self ctx in
            let val_core, val_ty = infer value_ctx value in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            let field = if public then [ (name, kind, val_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               field @ acc_fields)
              rest
        | Surface.MethodBinding { name; params; body; public } :: rest ->
            let method_core, method_ty = elaborate_method ctx params body in
            let method_val = Ctx.eval ctx method_core in
            let kind = if public then Method else PrivateMethod in
            let ctx' = Ctx.define ctx name method_ty method_val in
            let field = if public then [ (name, kind, method_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, method_core) :: acc_binds,
               field @ acc_fields)
              rest
        | Surface.EffectBinding { name; params; ops; public } :: rest ->
            let _effect_id, eff, eff_ty, _elaborated_ops =
              elaborate_eff_family ctx name params ops
            in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name eff_ty eff in
            let field = if public then [ (name, kind, eff_ty) ] else [] in
            go ctx'
              (EffectBind (name, kind, eff) :: acc_binds,
               field @ acc_fields)
              rest
        | Surface.TraitBinding _ :: _ | Surface.ImplBinding _ :: _ ->
            raise (ElabError ApplyingNonFunction)
        | Surface.RecordTypeBinding { name; params; fields; public } :: rest ->
            check_duplicate_names (List.map fst fields);
            let rewritten_fields =
              List.map
                (fun (field, ty) -> (field, rewrite_record_self_refs name params ty))
                fields
            in
            let rec elaborate_params ctx param_values = function
              | [] ->
                  let placeholder =
                    VNeutral
                      { ty = VU;
                        neutral = { head = HPrim name; frames = List.map (fun value -> FApp value) param_values } }
                  in
                  infer (Ctx.with_self_type ctx placeholder)
                    (Surface.Struct { con_fields = rewritten_fields; bindings = [] })
              | param :: rest ->
                  let param_value = VRigid { lvl = ctx.Ctx.lvl; spine = [] } in
                  let ctx' = Ctx.bind ctx param VU in
                  let body_core, body_ty = elaborate_params ctx' (param_values @ [ param_value ]) rest in
                  let body_ty_term = Ctx.quote ctx' body_ty in
                  ( Lam body_core,
                    VPi
                      { explicitness = Implicit;
                        domain = VU;
                        effects = effect_row_closure ctx.Ctx.env empty_effect_row;
                        codomain = { env = ctx.Ctx.env; body = body_ty_term } } )
            in
            let val_core, val_ty = elaborate_params ctx [] params in
            let val_val = Ctx.eval ctx val_core in
            let kind = if public then Public else Private in
            let ctx' = Ctx.define ctx name val_ty val_val in
            let field = if public then [ (name, kind, val_ty) ] else [] in
            go ctx'
              (LetBind (name, kind, val_core) :: acc_binds,
               field @ acc_fields)
              rest
        | Surface.TypeBinding { name; params; ctors; public } :: rest ->
            if params <> [] then raise (ElabError NotANominalType);
            let elaborated_ctors =
              List.map
                (fun (cname, payload_opt) ->
                  match payload_opt with
                  | None -> (cname, None)
                  | Some payload_expr ->
                      let payload_core, payload_ty = infer ctx payload_expr in
                      check_type_like ctx payload_ty (Ctx.eval ctx payload_core);
                      (cname, Some { env = ctx.env; body = payload_core }))
                ctors
            in
            let nominal = VNominal { id = NominalId.fresh (); name; num_params = 0; params = []; constructors = elaborated_ctors } in
            let kind = if public then Public else Private in
            let ctor_values, ctor_types =
              List.split
                (List.map
                   (fun (cname, payload_clo_opt) ->
                     let ctor_value, ctor_ty =
                       build_ctor ctx.metas (nominal :: ctx.env) name cname 0 payload_clo_opt
                     in
                     ((cname, ctor_value), (cname, ctor_ty)))
                   elaborated_ctors)
            in
            let ctx =
              List.fold_left
                (fun ctx ((ctor_name, ctor_value), (_, ctor_ty)) ->
                  Ctx.define ctx ctor_name ctor_ty ctor_value)
                ctx (List.combine ctor_values ctor_types)
            in
            let ctx = Ctx.define ctx name VU nominal in
            let type_fields = (name, kind, VU) :: List.map (fun (c, ty) -> (c, kind, ty)) ctor_types in
            go ctx
              (TypeBind (name, kind, nominal, ctor_values) :: acc_binds,
               type_fields @ acc_fields)
              rest
      in
      let core_bindings, extra_fields = go binding_ctx ([], []) bindings in
      let result_con_fields = List.map (fun (n, c, _) -> (n, c)) con_cores in
      let type_fields =
        List.map (fun (n, _, ty) -> (n, Field, ty)) con_cores
        @ extra_fields
      in
      (Struct { con_fields = result_con_fields; bindings = core_bindings; partial = false },
       VStruct { fields = type_fields; partial = false })
  | Open (name, body) ->
      let ix, ty = Ctx.lookup ctx name in
      let value = Ctx.eval ctx (Var ix) in
      (match (Nbe.force ctx.metas ty, Nbe.force ctx.metas value) with
      | VStruct { fields = type_fields; _ }, VStruct { fields = value_fields; _ } ->
          let ctx' =
            List.fold_left
              (fun c (fname, k, fty) ->
                if k = Public || k = Method then
                  match List.find_opt (fun (vname, vk, _) -> String.equal fname vname && vk = k) value_fields with
                  | Some (_, _, value) -> Ctx.define c fname fty value
                  | None -> c
                else c)
              ctx type_fields
          in
          let body_core, body_ty = infer ctx' body in
          (Open (Var ix, body_core), body_ty)
      | _ -> raise (ElabError (UnboundVariable name (* not a struct *))))
  | RecordTypeDef { name; params; fields; body } ->
      check_duplicate_names (List.map fst fields);
      let rewritten_fields =
        List.map
          (fun (field, ty) -> (field, rewrite_record_self_refs name params ty))
          fields
      in
      let rec elaborate_params ctx param_values = function
        | [] ->
            let placeholder =
              VNeutral
                { ty = VU;
                  neutral = { head = HPrim name; frames = List.map (fun value -> FApp value) param_values } }
            in
            infer (Ctx.with_self_type ctx placeholder)
              (Surface.Struct { con_fields = rewritten_fields; bindings = [] })
        | param :: rest ->
            let param_value = VRigid { lvl = ctx.lvl; spine = [] } in
            let ctx' = Ctx.bind ctx param VU in
            let body_core, body_ty = elaborate_params ctx' (param_values @ [ param_value ]) rest in
            let body_ty_term = Ctx.quote ctx' body_ty in
            ( Lam body_core,
              VPi
                { explicitness = Implicit;
                  domain = VU;
                  effects = effect_row_closure ctx.env empty_effect_row;
                  codomain = { env = ctx.env; body = body_ty_term } } )
      in
      let val_core, val_ty = elaborate_params ctx [] params in
      let val_val = Ctx.eval ctx val_core in
      let ty_term = Ctx.quote ctx val_ty in
      let ctx' = Ctx.define ctx name val_ty val_val in
      let body_core, body_ty = infer ctx' body in
      (Let (ty_term, val_core, body_core), body_ty)
  | TypeDef { name; params; ctors; body } ->
      let num_params = List.length params in
      (* Bind type params as rigid variables (locally abstract types) *)
      let param_ctx =
        List.fold_left
          (fun ctx param_name ->
            Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
          ctx params
      in
      let nominal_id = NominalId.fresh () in
      let nominal_placeholder = VNominal { id = nominal_id; name; num_params = 0; params = []; constructors = [] } in
      let recursive_param_ctx =
        if num_params = 0 then Ctx.define param_ctx name VU nominal_placeholder
        else
          let type_var_terms = List.mapi (fun i _ -> Var (num_params - 1 - i)) params in
          let type_body_term = NomRef (name, type_var_terms) in
          let type_core_term = List.fold_right (fun _ acc -> Lam acc) params type_body_term in
          let type_val = Nbe.eval param_ctx.metas (nominal_placeholder :: param_ctx.env) type_core_term in
          let type_ty =
            let depth = List.length param_ctx.env + 1 in
            List.fold_right
              (fun _ acc ->
                VPi { explicitness = Explicit; domain = VU;
                      effects = effect_row_closure (nominal_placeholder :: param_ctx.env) empty_effect_row;
                      codomain = { env = nominal_placeholder :: param_ctx.env; body = Nbe.quote param_ctx.metas depth acc } })
              params VU
          in
          Ctx.define param_ctx name type_ty type_val
      in
      let elaborated_ctors =
        List.map
          (fun (cname, payload_opt) ->
            match payload_opt with
            | None -> (cname, None)
            | Some payload_expr ->
                let payload_core, payload_ty = infer recursive_param_ctx payload_expr in
                check_type_like recursive_param_ctx payload_ty (Ctx.eval recursive_param_ctx payload_core);
                let payload_core = close_recursive_payload_term name num_params payload_core in
                (cname, Some { env = ctx.env @ [ nominal_placeholder ]; body = payload_core }))
          ctors
      in
      let nominal = VNominal { id = nominal_id; name; num_params; params = []; constructors = elaborated_ctors } in
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
                      effects = effect_row_closure body_ctx.env empty_effect_row;
                      codomain = { env = body_ctx.env; body = Nbe.quote body_ctx.metas depth acc } })
              params VU
          in
          Ctx.define body_ctx name type_ty type_val
        end
      in
      let env = nominal :: body_ctx.env in
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
        List.map
          (fun (cname, payload_opt) ->
            match payload_opt with
            | None -> (cname, None)
            | Some payload_expr ->
                let payload_core, payload_ty = infer recursive_param_ctx payload_expr in
                check_type_like recursive_param_ctx payload_ty (Ctx.eval recursive_param_ctx payload_core);
                (cname, Some (close_recursive_payload_term name num_params payload_core)))
          ctors
      in
      (NominalDef { id = nominal_id; name; num_params; ctors = ctor_payload_terms; body = body_core },
       body_ty)
  | EffectDef { name; params; ops; body } ->
      let num_params = List.length params in
      let effect_id, eff, eff_ty, elaborated_ops =
        elaborate_eff_family ctx name params ops
      in
      let body_ctx = Ctx.define ctx name eff_ty eff in
      let body_core, body_ty = infer body_ctx body in
      (EffectDef { id = effect_id; name; num_params; ops = elaborated_ops; body = body_core },
       body_ty)
  | TraitDef { name; params; fields; body } ->
      let trait_info, trait_ty = elaborate_trait ctx name params fields in
      let body_ctx = Ctx.add_trait (Ctx.define ctx name VU trait_ty) trait_info in
      let body_core, body_ty = infer body_ctx body in
      (Let (U, Atom (Atom.String ("trait:" ^ name)), body_core), body_ty)
  | ImplDef { trait_path = []; trait_name; args; fields; body } ->
      let body_ctx, _impl_effects, _impl_name, impl_ty, impl_core = elaborate_impl ctx trait_name args fields in
      let body_core, body_ty = infer body_ctx body in
      (Let (Ctx.quote ctx impl_ty, impl_core, body_core), body_ty)
  | ImplDef { trait_path = _ :: _; trait_name; _ } ->
      raise (ElabError (UnknownTrait trait_name))
  | Match (scrutinee, branches) ->
      let scrut_core, scrut_ty = infer ctx scrutinee in
      let value_branches = surface_value_branches branches in
      let effect_branches = surface_effect_branches branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let ret_ty = Ctx.raw_meta ctx in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let scrutinee_effects = collect_effects ctx scrutinee in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let value_branches' =
        List.map (fun (pat, body) ->
          let branch_ctx = refine_branch_context ctx refinement_target pat in
          let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
          let body_core = check ctx' body ret_ty in
          ValueBranch (core_pat, body_core))
          value_branches
      in
      let effect_branches' = List.map (elaborate_effect_branch ctx ret_ty residual scrutinee_effects) effect_branches in
      let pats = List.map fst (core_value_branches value_branches') in
      check_match_exhaustive ctx scrut_ty pats;
      (Match (scrut_core, value_branches' @ effect_branches'), Nbe.force ctx.metas ret_ty)

(** Elaborate a surface pattern against a scrutinee type, producing a core
    pattern and extending the context with bound pattern variables. *)
and elaborate_pat (ctx : Ctx.t) (pat : Surface.pat) (scrutinee_ty : value)
    : core_pat * Ctx.t =
  let core_pat, binders = elaborate_pat_binders ctx pat scrutinee_ty in
  let ctx' = List.fold_left (fun ctx (name, ty) -> Ctx.bind ctx name ty) ctx binders in
  (core_pat, ctx')

and elaborate_pat_binders (ctx : Ctx.t) (pat : Surface.pat)
    (scrutinee_ty : value) : core_pat * (string * value) list =
  match pat with
  | PatWild -> (CPatWild, [])
  | PatBind name -> (CPatBind, [ (name, scrutinee_ty) ])
  | PatAtom atom ->
      Ctx.unify ctx scrutinee_ty (VAtomTy (atom_ty_of_atom atom));
      (CPatAtom atom, [])
  | PatType atom_ty ->
      Ctx.unify ctx scrutinee_ty VU;
      (CPatType atom_ty, [])
  | PatOr (lhs, rhs) ->
      let lhs_core, lhs_binders = elaborate_pat_binders ctx lhs scrutinee_ty in
      let rhs_core, rhs_binders = elaborate_pat_binders ctx rhs scrutinee_ty in
      if List.length lhs_binders <> List.length rhs_binders then
        raise (ElabError PatternBindingMismatch);
      List.iter2
        (fun (lhs_name, lhs_ty) (rhs_name, rhs_ty) ->
          if not (String.equal lhs_name rhs_name) then
            raise (ElabError PatternBindingMismatch);
          Ctx.unify ctx lhs_ty rhs_ty)
        lhs_binders rhs_binders;
      (CPatOr (lhs_core, rhs_core), lhs_binders)
  | PatProd sub_pats -> (
      match Nbe.force ctx.metas scrutinee_ty with
      | VProdTy tys ->
          if List.length sub_pats <> List.length tys then
            raise (ElabError TupleLengthMismatch);
          let core_subs, binders =
            List.fold_left2
              (fun (core_acc, binder_acc) pat ty ->
                let core_pat, binders = elaborate_pat_binders ctx pat ty in
                (core_pat :: core_acc, binders @ binder_acc))
              ([], []) sub_pats tys
          in
          (CPatProd (List.rev core_subs), List.rev binders)
      | _ -> raise (ElabError TupleLengthMismatch))
  | PatRecord { typ_path; typ; fields; partial } ->
      let _record_value, record_ty = resolve_path_value ctx typ_path typ in
      Ctx.unify ctx scrutinee_ty record_ty;
      (match Nbe.force ctx.metas record_ty with
      | VStruct { fields = struct_fields; _ } ->
          let record_fields = visible_record_fields struct_fields in
          check_duplicate_names (List.map fst fields);
          List.iter
            (fun (name, _) ->
              if Option.is_none (find_record_field record_fields name) then
                raise (ElabError (UnknownRecordField name)))
            fields;
          if not partial then
            List.iter
              (fun (name, _) ->
                if Option.is_none (List.assoc_opt name fields) then
                  raise (ElabError (MissingRecordField name)))
              record_fields;
          let core_fields, binders =
            List.fold_left
              (fun (core_acc, binder_acc) (name, pat_opt) ->
                let field_ty =
                  match find_record_field record_fields name with
                  | Some (_, ty) -> ty
                  | None -> raise (ElabError (UnknownRecordField name))
                in
                let field_pat = Option.value pat_opt ~default:(Surface.PatBind name) in
                let core_pat, binders = elaborate_pat_binders ctx field_pat field_ty in
                ((name, core_pat) :: core_acc, binders @ binder_acc))
              ([], []) fields
          in
          (CPatRecord { fields = List.rev core_fields; partial }, List.rev binders)
      | _ -> raise (ElabError ApplyingNonFunction))
  | PatStructType { fields; partial } ->
      (match Nbe.force ctx.metas scrutinee_ty with
      | VStruct _ -> ()
      | _ -> Ctx.unify ctx scrutinee_ty VU);
      check_duplicate_names (List.map fst fields);
      let core_fields, binders =
        List.fold_left
          (fun (core_acc, binder_acc) (name, field_pat) ->
            let core_pat, binders = elaborate_pat_binders ctx field_pat VU in
            ((name, core_pat) :: core_acc, binders @ binder_acc))
          ([], []) fields
      in
      (CPatStructType { fields = List.rev core_fields; partial }, List.rev binders)
  | PatCon (path, name, sub_pats) -> (
      match Nbe.force ctx.metas scrutinee_ty with
      | VU -> (
          match find_nominal_template ctx path name with
          | VNominal n ->
              if List.length sub_pats <> n.num_params then
                raise (ElabError PatternArityMismatch);
              let param_tys = List.init n.num_params (fun _ -> VU) in
              let core_param_pats, binders =
                List.fold_left2
                  (fun (pat_acc, binder_acc) sub_pat param_ty ->
                    let core_pat, sub_binders = elaborate_pat_binders ctx sub_pat param_ty in
                    (core_pat :: pat_acc, sub_binders @ binder_acc))
                  ([], []) sub_pats param_tys
              in
              (CPatNominalHead { id = n.id; name = n.name; num_params = n.num_params;
                                 param_pats = List.rev core_param_pats },
               List.rev binders)
          | _ -> raise (ElabError (UnknownConstructor name))
          | exception ElabError (UnknownConstructor _) when path = [] && sub_pats = [] && starts_lowercase name ->
              (CPatBind, [ (name, VU) ])
          | exception ElabError (UnboundVariable _) when path = [] && sub_pats = [] && starts_lowercase name ->
              (CPatBind, [ (name, VU) ]))
      | _ ->
          let resolved_nominal =
            match path with
            | [] -> None
            | _ ->
                let ctor_value, _ = resolve_path_value ctx path name in
                match Nbe.force ctx.metas ctor_value with
                | VCon { nominal; _ } -> Some nominal
                | VLam _ | VPi _ -> None
                | _ -> raise (ElabError (UnknownConstructor name))
          in
          (match Nbe.force ctx.metas scrutinee_ty with
          | VNominal n ->
              Option.iter (Ctx.unify ctx scrutinee_ty) resolved_nominal;
              (match List.find_opt (fun (cname, _) -> String.equal cname name) n.constructors with
              | Some (_, payload_opt) ->
                  let num_type_params = List.length n.params in
                  (match (sub_pats, payload_opt) with
                  | [], None -> (CPatCon (name, num_type_params, []), [])
                  | [sub_pat], Some payload_clo ->
                      let payload_ty =
                        Nbe.eval ctx.metas (List.rev n.params @ payload_clo.env) payload_clo.body in
                      let core_sub, binders = elaborate_pat_binders ctx sub_pat payload_ty in
                      (CPatCon (name, num_type_params, [core_sub]), binders)
                  | _ ->
                      raise (ElabError PatternArityMismatch))
              | None -> raise (ElabError (UnknownConstructor name)))
          | _ -> raise (ElabError NotANominalType)))

and resolve_effect_branch_operation ctx scrutinee_effects branch =
  let effect_core, effect_value, input_ty, output_ty =
    resolve_perform_operation ctx ~effect_path:branch.effect_path ~op:branch.op
  in
  match List.find_opt (fun performed -> Ctx.conv ctx performed.value effect_value) scrutinee_effects with
  | None -> (effect_core, effect_value, input_ty, output_ty)
  | Some matched ->
      (matched.core, matched.value, Nbe.force ctx.Ctx.metas input_ty, Nbe.force ctx.Ctx.metas output_ty)

and handled_effects ctx scrutinee_effects branches =
  let grouped = Hashtbl.create 8 in
  List.iter
    (fun branch ->
      let _effect_core, effect_value, input_ty, _output_ty =
        resolve_effect_branch_operation ctx scrutinee_effects branch
      in
      let core_pat, _ = elaborate_pat ctx branch.arg_pat input_ty in
      check_match_exhaustive ctx input_ty [ core_pat ];
      let key =
        match Nbe.force ctx.Ctx.metas effect_value with
        | VEffect eff -> string_of_int eff.id ^ ":" ^ branch.op
        | _ -> raise (ElabError ExpectedEffect)
      in
      if Hashtbl.mem grouped key then raise (ElabError (DuplicateEffectBranch branch.op));
      Hashtbl.add grouped key effect_value)
    branches;
  List.filter
    (fun eff_expr ->
      match Nbe.force ctx.Ctx.metas eff_expr.value with
      | VEffect eff ->
          List.exists (fun performed -> Ctx.conv ctx performed.value eff_expr.value) scrutinee_effects
          && List.for_all
               (fun (op_name, _, _) -> Hashtbl.mem grouped (string_of_int eff.id ^ ":" ^ op_name))
               eff.operations
      | _ -> false)
    scrutinee_effects

and residual_effects ctx scrutinee_effects effect_branches =
  List.fold_left (fun effects handled -> remove_expr_effect ctx handled effects)
    scrutinee_effects
    (handled_effects ctx scrutinee_effects effect_branches)

and elaborate_effect_branch ctx ret_ty residual scrutinee_effects branch =
  let effect_core, _effect_value, input_ty, output_ty =
    resolve_effect_branch_operation ctx scrutinee_effects branch
  in
  let core_pat, arg_ctx = elaborate_pat ctx branch.arg_pat input_ty in
  let cont_ty =
    VPi
      { explicitness = Explicit;
        domain = output_ty;
        effects = effect_row_closure_of_expr_effects arg_ctx residual;
        codomain = { env = arg_ctx.env; body = Ctx.quote arg_ctx ret_ty } }
  in
  let body_ctx, resume_entry = Ctx.bind_anonymous arg_ctx cont_ty in
  let body_core = check { body_ctx with Ctx.resume_entry = Some resume_entry } branch.body ret_ty in
  EffectBranch { eff = effect_core; op = branch.op; arg_pat = core_pat; body = body_core }

(** Application inference.
    Loops to insert fresh metas for implicit VPi domains before consuming
    the user's explicit argument. *)
and infer_resume (ctx : Ctx.t) (arg : Surface.t) : term * value =
  let cont_ix, cont_ty = Ctx.lookup_resume ctx in
  match Nbe.force ctx.metas cont_ty with
  | VPi { explicitness = Explicit; domain; codomain; _ } ->
      let arg_core = check ctx arg domain in
      let arg_value = Ctx.eval ctx arg_core in
      (Ap (Var cont_ix, Explicit, arg_core), Nbe.closure_apply ctx.metas codomain arg_value)
  | _ -> raise (ElabError ApplyingNonFunction)

and infer_ap (ctx : Ctx.t) (f : Surface.t) (a : Surface.t) : term * value =
  let f_core, f_ty = infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  (* Insert leading implicit arguments before consuming the user's explicit argument. *)
  let f_core, f_ty = insert_implicit_args ctx f_core f_ty in
  let rec pending_trait_dicts ty pending =
    match Nbe.force ctx.metas ty with
    | VPi { explicitness = Implicit; domain; codomain; _ } -> (
        match resolve_trait_dict_ty ctx domain with
        | Some (trait_info, args, _) -> (
            match resolve_trait_evidence ctx trait_info args with
            | _ -> None
            | exception ElabError (UnknownTrait _) ->
                let dict_placeholder = Ctx.raw_meta ctx in
                pending_trait_dicts
                  (Nbe.closure_apply ctx.metas codomain dict_placeholder)
                  (pending @ [ (trait_info, args) ]))
        | None -> None)
    | ty -> if pending = [] then None else Some (pending, ty)
  in
  let apply_pending_trait_dicts pending core =
    List.fold_left
      (fun core (trait_info, args) ->
        let evidence_core, _ = resolve_trait_evidence ctx trait_info args in
        Ap (core, Implicit, evidence_core))
      core pending
  in
  match f_ty with
  | VPi { explicitness = Explicit; domain = a_ty; codomain = b_clo; _ } ->
      let a_core = check ctx a a_ty in
      let ret_ty =
        if term_mentions_var 0 b_clo.body then
          let a_val = Ctx.eval ctx a_core in
          Nbe.closure_apply ctx.metas b_clo a_val
        else Nbe.closure_apply ctx.metas b_clo (VRigid { lvl = ctx.lvl; spine = [] })
      in
      (Ap (f_core, Explicit, a_core), Nbe.force ctx.metas ret_ty)
  | _ -> (
      match pending_trait_dicts f_ty [] with
      | Some (pending, VPi { explicitness = Explicit; domain = a_ty; codomain = b_clo; _ }) ->
          let a_core = check ctx a a_ty in
          let f_core = apply_pending_trait_dicts pending f_core in
          let ret_ty =
            if term_mentions_var 0 b_clo.body then
              let a_val = Ctx.eval ctx a_core in
              Nbe.closure_apply ctx.metas b_clo a_val
            else Nbe.closure_apply ctx.metas b_clo (VRigid { lvl = ctx.lvl; spine = [] })
          in
          (Ap (f_core, Explicit, a_core), Nbe.force ctx.metas ret_ty)
      | _ -> (
          match f_ty with
          | VFlex _ | VRigid _ | VNeutral _ ->
              let a_ty = Ctx.raw_meta ctx in
              let a_core = check ctx a a_ty in
              let a_val = Ctx.eval ctx a_core in
              let ret_meta = Ctx.raw_meta (Ctx.bind ctx "_" a_ty) in
              let expected_f_ty =
                VPi
                  { explicitness = Explicit;
                    domain = a_ty;
                    effects = effect_row_closure ctx.env empty_effect_row;
                    codomain = { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta } }
              in
              Ctx.unify ctx f_ty expected_f_ty;
              let ret_ty =
                Nbe.closure_apply ctx.metas
                  { env = ctx.env; body = Ctx.quote (Ctx.bind ctx "_" a_ty) ret_meta }
                  a_val
              in
              (Ap (f_core, Explicit, a_core), ret_ty)
          | _ -> raise (ElabError ApplyingNonFunction)))

(** Explicit implicit application ([f {arg}]). *)
and infer_ap_implicit (ctx : Ctx.t) (f : Surface.t) (a : Surface.t) : term * value =
  let f_core, f_ty = infer ctx f in
  let f_ty = Nbe.force ctx.metas f_ty in
  match f_ty with
  | VPi { explicitness = Implicit; domain = a_ty; codomain = b_clo; _ } ->
      let a_core = check ctx a a_ty in
      let a_val = Ctx.eval ctx a_core in
      let ret_ty = Nbe.closure_apply ctx.metas b_clo a_val in
      (Ap (f_core, Implicit, a_core), Nbe.force ctx.metas ret_ty)
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
            effects = effect_row_closure ctx.env empty_effect_row;
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
      | VPi { domain = a; effects; codomain; _ } ->
          collect a;
          let var = VRigid { lvl = ctx.lvl; spine = [] } in
          List.iter (fun eff -> collect (Nbe.eval ctx.metas (var :: effects.env) eff)) effects.effects;
          collect (Nbe.closure_apply ctx.metas codomain var)
      | VU | VAtom _ | VAtomTy _ | VRigid _ | VProd _ | VProdTy _ | VCont _ -> ()
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
                effects = effect_row_closure ctx.env empty_effect_row;
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
          let ty_val = Ctx.eval ctx ty_core in
          check_type_like ctx ty_ty ty_val;
          ty_val
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
          let ty_val = Ctx.eval ctx ty_core in
          check_type_like ctx ty_ty ty_val;
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

and elaborate_eff_family (ctx : Ctx.t) (name : string) (params : string list)
    (ops : Surface.effect_op list) : effect_id * value * value * (string * term * term) list =
  check_duplicate_eff_ops ops;
  let param_ctx =
    List.fold_left
      (fun ctx param_name ->
        Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
      ctx params
  in
  let effect_id = EffectId.fresh () in
  let elaborated_ops =
    List.map
      (fun (op : Surface.effect_op) ->
        let input_core, input_ty = infer param_ctx op.input in
        check_type_like param_ctx input_ty (Ctx.eval param_ctx input_core);
        let output_core, output_ty = infer param_ctx op.output in
        check_type_like param_ctx output_ty (Ctx.eval param_ctx output_core);
        (op.name, input_core, output_core))
      ops
  in
  let operations =
    List.map
      (fun (op_name, input_core, output_core) ->
        (op_name, { env = ctx.env; body = input_core }, { env = ctx.env; body = output_core }))
      elaborated_ops
  in
  let eff = VEffect { id = effect_id; name; params = []; operations } in
  let eff_ty =
    List.fold_right
      (fun _ acc ->
        VPi
          { explicitness = Explicit;
            domain = VU;
            effects = effect_row_closure ctx.env empty_effect_row;
            codomain = { env = ctx.env; body = Nbe.quote ctx.metas ctx.lvl acc } })
      params VU
  in
  (effect_id, eff, eff_ty, elaborated_ops)

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
        let inner = payload_term ^->> ret_term in
        let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (U ^=>> t) in
        wrap_pi num_params inner
    | None ->
        let rec wrap_pi n t = if n = 0 then t else wrap_pi (n - 1) (U ^=>> t) in
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
        let ty_val = Ctx.eval ctx ty_core in
        check_type_like ctx ty_ty ty_val;
        ty_val
    | None ->
        Ctx.raw_meta ctx
  in
  let ctx' = Ctx.bind ctx param.name a_ty in
  let body_core, body_ty = infer ctx' body in
  let body_effects = collect_effects ctx' body in
  let body_ty_term = Ctx.quote ctx' body_ty in
  let pi_ty = VPi { explicitness = expl_of_surface param.explicitness; domain = a_ty; effects = effect_row_closure ctx.env (effect_row_of_expr_effects ctx' body_effects); codomain = { env = ctx.env; body = body_ty_term } } in
  (Lam body_core, pi_ty)

(** Bidirectional checking: verify [expr] against an [expected] type. *)
and check (ctx : Ctx.t) (expr : Surface.t) (expected : value) : term =
  let expected = Nbe.force ctx.metas expected in
  match (expr, expected) with
  | Lam (param, body), VPi { explicitness; domain = a_ty; effects; codomain = b_clo } ->
      if expl_of_surface param.explicitness <> explicitness then raise (ElabError ApplyingNonFunction);
      let ctx' = Ctx.bind ctx param.name a_ty in
      let binder = VRigid { lvl = ctx.lvl; spine = [] } in
      let rec insert_hidden_dicts ctx body_ty inserted =
        match Nbe.force ctx.Ctx.metas body_ty with
        | VPi { explicitness = Implicit; domain; codomain; _ } -> (
            match resolve_trait_dict_ty ctx domain with
            | Some (trait_info, args, dict_ty) ->
                let ctx', entry = Ctx.bind_anonymous ctx dict_ty in
                let evidence =
                  { evidence_trait_id = trait_info.trait_id;
                    evidence_trait_name = trait_info.trait_name;
                    evidence_args = args;
                    evidence_level = entry.level;
                    evidence_ty = dict_ty }
                in
                let dict_value = VRigid { lvl = entry.level; spine = [] } in
                insert_hidden_dicts (Ctx.add_trait_evidence ctx' evidence)
                  (Nbe.closure_apply ctx.Ctx.metas codomain dict_value)
                  (inserted + 1)
            | None -> (ctx, body_ty, inserted))
        | _ -> (ctx, body_ty, inserted)
      in
      let b_ty = Nbe.closure_apply ctx.metas b_clo binder in
      let body_ctx, body_expected, inserted = insert_hidden_dicts ctx' b_ty 0 in
      let body_core = check body_ctx body body_expected in
      let body_effects = collect_effects body_ctx body in
      check_effect_subset body_ctx body_effects (effect_row_values ctx effects binder);
      Lam (List.fold_left (fun acc _ -> Lam acc) body_core (List.init inserted Fun.id))
  | Match (scrutinee, branches), VPi _ ->
      let scrutinee_effects = collect_effects ctx scrutinee in
      let effect_branches = surface_effect_branches branches in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      require_empty_effects residual;
      let scrut_core = check ctx scrutinee VU in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let value_branches = surface_value_branches branches in
      let value_branches' =
        List.map
          (fun (pat, body) ->
            let branch_ctx = refine_branch_context ctx refinement_target pat in
            let core_pat, ctx' = elaborate_pat branch_ctx pat VU in
            let refined_expected = refine_branch_expected ctx refinement_target pat expected in
            let body_core = check ctx' body refined_expected in
            ValueBranch (core_pat, body_core))
          value_branches
      in
      let effect_branches' = List.map (elaborate_effect_branch ctx expected residual scrutinee_effects) effect_branches in
      check_match_exhaustive ctx VU (List.map fst (core_value_branches value_branches'));
      Match (scrut_core, value_branches' @ effect_branches')
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
              let ty_val = Ctx.eval ctx ty_core in
              check_type_like ctx ty_ty ty_val;
              ty_val
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
              let ty_val = Ctx.eval ctx ty_core in
              check_type_like ctx ty_ty ty_val;
              let core = check ctx value ty_val in
              (core, ty_val)
          | None -> infer ctx value
        in
        let gen_val_core, gen_val_ty = generalize ctx val_core val_ty in
        let ty_term = Ctx.quote ctx gen_val_ty in
        let ctx' =
          if collect_effects ctx value = [] then Ctx.define ctx name gen_val_ty (Ctx.eval ctx gen_val_core)
          else Ctx.bind ctx name gen_val_ty
        in
        let body_core = check ctx' body expected in
        Let (ty_term, gen_val_core, body_core)
      end
  | Match (scrutinee, branches), _ ->
      let scrut_core, scrut_ty = infer ctx scrutinee in
      let value_branches = surface_value_branches branches in
      let effect_branches = surface_effect_branches branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let scrutinee_effects = collect_effects ctx scrutinee in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      require_empty_effects residual;
      let value_branches' =
        List.map (fun (pat, body) ->
          let branch_ctx = refine_branch_context ctx refinement_target pat in
          let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
          let refined_expected = refine_branch_expected ctx refinement_target pat expected in
          let body_core = check ctx' body refined_expected in
          ValueBranch (core_pat, body_core))
          value_branches
      in
      let effect_branches' = List.map (elaborate_effect_branch ctx expected residual scrutinee_effects) effect_branches in
      check_match_exhaustive ctx scrut_ty (List.map fst (core_value_branches value_branches'));
      Match (scrut_core, value_branches' @ effect_branches')
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
      if Ctx.conv ctx expected VU then
        check_type_like ctx inferred (Ctx.eval ctx core)
      else
        Ctx.unify ctx expected inferred;
      core

let () =
  infer_ref := infer;
  check_ref := check;
  collect_effects_ref := collect_effects

(** Build the initial elaboration context with built-in types ([I64],
    [Bool], [Unit], [Char], [Type]) and primitive operators. *)
let init_ctx () : Ctx.t =
  let ctx = Ctx.empty () in
  let add_type ctx name v = Ctx.define ctx name VU v in
  let ctx = add_type ctx "I64" (VAtomTy TI64) in
  let ctx = add_type ctx "Bool" (VAtomTy TBool) in
  let ctx = add_type ctx "Unit" (VAtomTy TUnit) in
  let ctx = add_type ctx "Char" (VAtomTy TChar) in
  let ctx = add_type ctx "String" (VAtomTy TString) in
  let ctx = add_type ctx "Absurd" (VAtomTy TAbsurd) in
  let ctx = Ctx.define ctx "Type" VU VU in
  let ctx =
    NameMap.fold
      (fun name ty ctx ->
        Ctx.define ctx name ty (VNeutral { ty; neutral = { head = HPrim name; frames = [] } }))
      prims ctx
  in
  let stdlib = Lazy.force parsed_stdlib in
  let rec add_stdlib ctx = function
    | Surface.Let { name; type_; value; body; recursive = false } ->
        let value_core, value_ty =
          match type_ with
          | Some ty_expr ->
              let ty_core, ty_ty = infer ctx ty_expr in
              let ty_val = Ctx.eval ctx ty_core in
              check_type_like ctx ty_ty ty_val;
              (check ctx value ty_val, ty_val)
          | None -> infer ctx value
        in
        let gen_value_core, gen_value_ty = generalize ctx value_core value_ty in
        let gen_value = Ctx.eval ctx gen_value_core in
        add_stdlib (Ctx.define ctx name gen_value_ty gen_value) body
    | Surface.Let { recursive = true; _ } -> assert false
    | _ -> ctx
  in
  add_stdlib ctx stdlib

(** Entry point: elaborate a surface expression in the given context. *)
let on_expr ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  infer ctx expr

let on_expr_effects ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value * expr_effects =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  let core, ty = infer ctx expr in
  (core, ty, collect_effects ctx expr)
