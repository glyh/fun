open Core
include Elab_error
open Elab_common
open Elab_validate
open Elab_effects

module Ctx = Elab_ctx.Ctx

open Elab_resolve
open Elab_refine
open Elab_patterns
open Elab_match
open Elab_defs
open Elab_generalize
open Elab_apply
open Elab_ops

(* The one place a module or struct binding extends the elaboration context.
   [Core.binding_slots] owns the order and the count; the payloads - a type and a
   value per slot - are the elaborator's own, and a disagreement between the term
   a binding emits and the entries it pushes fails here rather than surfacing
   later as a wrong de Bruijn index.
   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
let extend_from_slots (ctx : Ctx.t) (bind : Core.struct_binding_term) payloads =
  let slots =
    match Core.binding_slots bind with
    | Some slots -> slots
    | None -> failwith "binding contributes no slot list"
  in
  List.fold_left2
    (fun (ctx : Ctx.t) _slot payload ->
      match payload with
      | `Param param_name ->
          Ctx.define ctx param_name VU (VRigid { lvl = ctx.Ctx.lvl; spine = [] })
      | `Entry (name, ty, value) -> Ctx.define ctx name ty value
      | `Anonymous (ty, value) -> fst (Ctx.define_anonymous ctx ty value))
    ctx slots payloads

(* The parameters a [rec] value takes before it is a struct type:
   [struct { … }], [fn(A : Type) { struct { … } }] or [fn[A : Type] { … }]. [None] for any other value. *)
let rec struct_type_params (value : Syntax.t) =
  match value.kind with
  | Syntax.Struct _ -> Some []
  | Syntax.Lam (param, body) ->
      Option.map (fun params -> param :: params) (struct_type_params body)
  | _ -> None

(* The parameters a value takes before it is an enum, and its constructors:
   [enum { … }] or [fn(A : Type) { enum { … } }]. [None] for any other value. *)
let rec enum_type_params (value : Syntax.t) =
  match value.kind with
  | Syntax.Enum { ctors; _ } -> Some ([], ctors)
  | Syntax.Lam (param, body) -> Option.map (fun (params, ctors) -> (param.name :: params, ctors)) (enum_type_params body)
  | _ -> None

(* An [enum] is the type declaration its value names: a [TypeDef] named [name]
   over [params], whose body is the type. Its constructors are members of the
   type, never names in scope, so their keys are resolved names nothing spells. *)
let enum_type_def ~(name : Syntax.id) ~params ~ctors ~span : Syntax.t =
  let hidden cname = Syntax.fresh_id (cname ^ "#ctor") in
  { kind = Syntax.TypeDef { name; params; ctors = List.map (fun (c, ps) -> (hidden c, ps)) ctors; body = { kind = Syntax.Var name; span } };
    span }

(* A [rec] group whose members are enums, as the type declarations they are. *)
let enum_group_decls members : Syntax.type_decl list option =
  let decls = List.map (fun ((n : Syntax.id), v) -> (n, enum_type_params v)) members in
  if List.for_all (fun (_, d) -> Option.is_none d) decls then None
  else if List.exists (fun (_, d) -> Option.is_none d) decls then
    raise (ElabError (InvalidRecursiveRecord "a rec … and … group holds enums, struct types or functions, not a mix"))
  else
    Some
      (List.map
         (fun ((name : Syntax.id), d) ->
           let params, ctors = Option.get d in
           { Syntax.name; params; ctors = List.map (fun (c, ps) -> (Syntax.fresh_id (c ^ "#ctor"), ps)) ctors })
         decls)

(* The constructors of a type value, as members: for [e : Type] naming a nominal,
   or a type former [e : (A : Type) -> … -> Type], each constructor with its core
   at [ctx] and its type. A former's constructors are generic over its
   parameters ([Option.Some : [A : Type] -> A -> Option(A)]). [None] when [e] is
   not a nominal type. *)
let type_constructors (ctx : Ctx.t) e_core e_ty =
  let mc = ctx.Ctx.metas in
  let rec peel depth ty value domains =
    match Nbe.force mc ty with
    | VU -> Some (depth, List.rev domains, value)
    | VPi { domain; codomain; _ } ->
        let arg = VRigid { lvl = depth; spine = [] } in
        peel (depth + 1) (Nbe.closure_apply mc codomain arg) (Nbe.apply mc value arg) (Nbe.quote mc depth domain :: domains)
    | _ -> None
  in
  match peel ctx.Ctx.lvl e_ty (Ctx.eval ctx e_core) [] with
  | None -> None
  | Some (depth, domains, value) -> (
      match Nbe.force mc value with
      | VNominal nom ->
          let env = List.init (depth - ctx.Ctx.lvl) (fun i -> VRigid { lvl = depth - 1 - i; spine = [] }) @ ctx.Ctx.env in
          let unapplied = VNominal { nom with params = [] } in
          let nomref = NomRef { id = nom.id; name = nom.name; num_params = nom.num_params;
                                captures = List.map (Nbe.quote mc depth) nom.captures; params = [] } in
          let wrap_params body = List.fold_left (fun acc _ -> Lam acc) body domains in
          let wrap_pis body = List.fold_right (fun domain acc -> Pi { explicitness = Implicit; domain; effects = empty_effect_row; codomain = acc }) domains body in
          Some
            (List.map
               (fun (cname, payload_clos) ->
                 let payload_count = List.length payload_clos in
                 let chain = ctor_term ~nominal:(Var 0) ~name:cname ~nominal_name:nom.name ~num_params:nom.num_params ~payload_count in
                 let applied = List.fold_left (fun acc p -> Ap (acc, Implicit, Nbe.quote mc (depth + 1) p)) chain nom.params in
                 let core = wrap_params (Let (U, nomref, applied)) in
                 let _, generic_ty = build_ctor mc (unapplied :: env) nom.name cname nom.num_params payload_clos in
                 let instance_ty =
                   List.fold_left
                     (fun ty p -> match Nbe.force mc ty with VPi { codomain; _ } -> Nbe.closure_apply mc codomain p | _ -> ty)
                     generic_ty nom.params
                 in
                 (cname, core, Nbe.eval mc ctx.Ctx.env (wrap_pis (Nbe.quote mc depth instance_ty))))
               (nominal_constructors nom.id nom.captures))
      | _ -> None)

(* [open T] of a type value: its constructors, in scope by their names for what
   follows, each bound as the core it is at [ctx] shifted past the ones before. *)
let open_type_constructors ~label (ctx : Ctx.t) ctors =
  let ctx0 = ctx in
  let ctx, members =
    List.fold_left
      (fun (c, members) (cname, core, ty) ->
        (add_opened_field c cname ty (Ctx.eval ctx0 core), NameMap.add cname { level = c.Ctx.lvl; ty } members))
      (ctx, NameMap.empty) ctors
  in
  ({ ctx with Ctx.opened = (label, members) :: ctx.Ctx.opened }, List.mapi (fun i (cname, core, ty) -> (cname, shift_term i 0 core, ty)) ctors)

(* What [export] re-exports: a member, or a named impl. *)
type export_member = Export_field | Export_impl

(* The public members of a module, for [export]: each a projection of the module
   at [ctx], with its type. A named impl is projected by its name; an unnamed one
   has no projection, so it must be named first. *)
let module_exports (ctx : Ctx.t) m_core m_ty =
  match Nbe.module_type_of ctx.Ctx.metas m_ty (Ctx.eval ctx m_core) with
  | VModule { entries; _ } ->
      List.filter_map
        (function
          | ModuleField (name, Public, ty) -> Some (Export_field, name, Dot (m_core, name), ty)
          | ModuleImpl (Some name, Public, ty, _) -> Some (Export_impl, name, Dot (m_core, name), ty)
          | ModuleImpl (None, Public, ty, _) ->
              let trait = match resolve_trait_dict_ty ctx ty with Some (info, _, _) -> info.trait_name | None -> "?" in
              raise (ElabError (ExportUnnamedImpl trait))
          | _ -> None)
        entries
  | _ -> raise (ElabError NotAModule)

(* A recursive group of struct types: [rec Numbers = struct { … }], or
   [rec A = struct { b : Option(B) } and B = struct { a : Option(A) }].
   Each binding mints an identity. Every member's body sees every member's name
   as a recursive occurrence of it (a function of the parameters to one), bound
   by [Let]s its core keeps; each finished value is what an occurrence unfolds
   to. Members elaborate in order: [extend] adds a finished member to the
   context the next one (and what follows the group) is elaborated in, and
   [value_ctx] is the context a member's body starts from. *)
let rec elab_rec_group (ops : Elab_ops.t) (ctx : Ctx.t) ~value_ctx ~extend members =
  match List.partition (fun (_, value) -> Option.is_some (struct_type_params value)) members with
  | _, [] -> elab_struct_group ops ctx ~value_ctx ~extend members
  | [], _ -> elab_fixpoint_group ops ctx ~value_ctx ~extend members
  | _ -> raise (ElabError (InvalidRecursiveRecord "a rec … and … group holds struct types or functions, not both"))

and elab_struct_group (ops : Elab_ops.t) (ctx : Ctx.t) ~value_ctx ~extend members =
  let members =
    List.map
      (fun ((key : string), (value : Syntax.t)) -> (key, Syntax.label key, Option.get (struct_type_params value), value))
      members
  in
  (* An occurrence captures what the enclosing scope names (E11). *)
  let levels = List.filter (fun l -> l < ctx.Ctx.lvl) ctx.Ctx.scope_captures in
  let occurrences =
    List.map
      (fun (_, name, params, _) ->
        let id = fresh_record_id () in
        let n = List.length params in
        let captures = List.map (fun l -> Var (n + ctx.Ctx.lvl - 1 - l)) levels in
        let term = List.fold_right (fun _ acc -> Lam acc) params (RecOcc { id; name; captures; args = List.init n (fun i -> Var (n - 1 - i)) }) in
        let ty =
          List.fold_right
            (fun (param : Syntax.param) acc ->
              VPi { explicitness = expl_of_syntax param.explicitness; domain = VU;
                    effects = effect_row_closure [] empty_effect_row;
                    codomain = { env = []; body = Nbe.quote ctx.Ctx.metas 1 acc } })
            params VU
        in
        (id, term, ty))
      members
  in
  let wrap core = List.fold_right (fun (_, term, ty) acc -> Let (Nbe.quote ctx.Ctx.metas 0 ty, term, acc)) occurrences core in
  let ctx, results =
    List.fold_left
      (fun (ctx, acc) ((key, name, _, value), (id, _, _)) ->
        let body_ctx =
          List.fold_left2
            (fun body_ctx (key, _, _, _) (_, term, ty) -> Ctx.define body_ctx key ty (Nbe.eval ctx.Ctx.metas ctx.Ctx.env term))
            (value_ctx ctx) members occurrences
        in
        let (body_core, body_ty), effects = collecting body_ctx (fun body_ctx -> ops.infer body_ctx value) in
        emit ctx effects;
        let core = wrap body_core in
        let finished = Ctx.eval ctx core in
        finish_record id { record_env = ctx.Ctx.env; record_body = core; record_levels = levels };
        let member = (key, name, core, body_ty, finished) in
        (extend ctx member, member :: acc))
      (ctx, []) (List.combine members occurrences)
  in
  (ctx, List.rev results)

(* A recursive group of values: [rec even : I64 -> Bool = fn(n) { … odd(n - 1) … }
   and odd : I64 -> Bool = fn(n) { … }]. Every body is checked once, seeing every
   member at its annotated type (or a meta); each member is its index into one
   [Fix] group over those bodies. The group's terms are built at the context the
   group starts in, and member [i] follows the [i] members before it. *)
and elab_fixpoint_group (ops : Elab_ops.t) (ctx : Ctx.t) ~value_ctx ~extend members =
  let members =
    List.map
      (fun ((key : string), (value : Syntax.t)) ->
        let body, ty =
          match value.kind with
          | Syntax.Annotated { inner; typ } -> let _, _, ty = ops.type_value_of_expr ctx typ in (inner, ty)
          | _ -> (value, Ctx.raw_meta ctx)
        in
        (key, Syntax.label key, body, ty))
      members
  in
  let body_ctx = List.fold_left (fun c (key, _, _, ty) -> Ctx.bind c key ty) (value_ctx ctx) members in
  let bodies =
    List.map
      (fun (_, _, body, ty) ->
        let core, effects = collecting body_ctx (fun body_ctx -> ops.check body_ctx body ty) in
        emit ctx effects;
        core)
      members
  in
  let group =
    List.map2 (fun (_, name, _, ty) body -> { fix_name = name; fix_pure = Ctx.pure_call ctx ty; fix_body = body }) members bodies
  in
  let start = ctx in
  let ctx, results =
    List.fold_left
      (fun (ctx, acc) (index, (key, name, _, ty)) ->
        let fix = Fix { members = group; index } in
        let member = (key, name, shift_term index 0 fix, ty, Ctx.eval start fix) in
        (extend ctx member, member :: acc))
      (ctx, []) (List.mapi (fun i m -> (i, m)) members)
  in
  (ctx, List.rev results)

(* A module or struct member [name = value]: its core, its type, and the value
   the items after it see - evaluated when evaluating it performs nothing,
   otherwise opaque, for evaluating it here would run what it performs. *)
let rec elab_member_value (ops : Elab_ops.t) (ctx : Ctx.t) ~value_ctx ~key ~name ~recursive value =
  let value = Syntax.name_enum name value in
  match if recursive then enum_type_params value else None with
  | Some (params, ctors) ->
      elab_member_value ops ctx ~value_ctx ~key ~name ~recursive:false
        (enum_type_def ~name:(Syntax.fresh_id key) ~params ~ctors ~span:value.span)
  | None ->
  match if recursive then struct_type_params value else None with
  | Some _ ->
      let _, members = elab_rec_group ops ctx ~value_ctx:(fun _ -> value_ctx) ~extend:(fun ctx _ -> ctx) [ (key, value) ] in
      let _, _, core, ty, finished = List.hd members in
      (core, ty, finished, [])
  | None ->
  let rec_ty = Ctx.raw_meta ctx in
  let value_ctx = if recursive then Ctx.bind value_ctx key rec_ty else value_ctx in
  let (val_core, val_ty), effects = collecting value_ctx (fun value_ctx -> ops.infer value_ctx value) in
  emit ctx effects;
  (if recursive then Ctx.unify ctx rec_ty val_ty);
  let val_core = if recursive then fix_one name (Ctx.pure_call ctx rec_ty) val_core else val_core in
  if is_empty_expr_effects effects then (val_core, val_ty, Ctx.eval ctx val_core, [])
  else
    let sealed_ty, sealed = seal_generative ctx val_ty in
    (val_core, sealed_ty, VRigid { lvl = ctx.Ctx.lvl; spine = [] }, sealed)

(* A block's [rec name : type_ = value]: its type's term, its core, and the type
   and value the body sees. A struct type is a recursive record; anything else
   is a fixpoint. *)
let elab_rec_let (ops : Elab_ops.t) (ctx : Ctx.t) ~name ~type_ value =
  let annotation = Option.map (fun ty_expr -> let _, _, ty_val = ops.type_value_of_expr ctx ty_expr in ty_val) type_ in
  match struct_type_params value with
  | Some _ ->
      let _, members = elab_rec_group ops ctx ~value_ctx:Fun.id ~extend:(fun ctx _ -> ctx) [ (name, value) ] in
      let _, _, core, ty, finished = List.hd members in
      Option.iter (Ctx.unify ctx ty) annotation;
      (Ctx.quote ctx ty, core, ty, finished)
  | None ->
      let rec_ty = match annotation with Some ty -> ty | None -> Ctx.raw_meta ctx in
      let val_core = ops.check (Ctx.bind ctx name rec_ty) value rec_ty in
      let fix_core = fix_one name (Ctx.pure_call ctx rec_ty) val_core in
      (Ctx.quote ctx rec_ty, fix_core, rec_ty, Ctx.eval ctx fix_core)

(* THE nominal-type binding elaboration, in one place.

   [type T(p...) = C1(..) | C2(..)] as a module or struct member. Three things
   have to line up and used to be written out twice, once here and once in the
   [Struct] fold, with the copies disagreeing:

   - the entries the binding contributes, which must equal [Core.binding_width]:
     the params, then the constructors, then the type itself, in that order,
     because that is the order [Nbe]'s [TypeBind] pushes them;
   - the context the payloads are elaborated in, which names the type so a
     payload can refer to it recursively, and which is *temporary* - it is not
     part of the binding's width;
   - the closure environment each payload is stored under, which excludes the
     params ([build_ctor] supplies those itself) and carries the nominal
     placeholder outermost so [NomRef] can find it by id.

   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
(* A type declaration's names as the elaborator's context keys them. *)
type type_member = {
  member_key : string;  (* the context's key: the binder's resolved name *)
  member_name : string;  (* the label the type and its constructors carry *)
  member_params : string list;
  member_ctors : (string * Syntax.t list) list;
  member_ctor_keys : string list;
}

let elab_type_group ?(ctors_private = false) (ops : Elab_ops.t) (ctx : Ctx.t) ~(members : Syntax.type_decl list) ~public
    : Ctx.t * (Core.struct_binding_term * (string * struct_field_kind * value) list) list =
  let members =
    List.map
      (fun (m : Syntax.type_decl) ->
        { member_key = m.name.name; member_name = Syntax.label m.name.name; member_params = Syntax.names m.params;
          member_ctors = List.map (fun ((c : Syntax.id), payloads) -> (Syntax.label c.name, payloads)) m.ctors;
          member_ctor_keys = List.map (fun ((c : Syntax.id), _) -> c.name) m.ctors })
      members
  in
  let param_ctx_of (m : type_member) =
    List.fold_left
      (fun ctx param_name ->
        Ctx.define ctx param_name VU (VRigid { lvl = ctx.Ctx.lvl; spine = [] }))
      ctx m.member_params
  in
  (* The type's own type: [Type] when nullary, an explicit [Pi] chain otherwise
     so [T I64] elaborates. *)
  let nominal_ty_of (m : type_member) placeholder_env param_ctx =
    if m.member_params = [] then VU
    else
      let depth = List.length param_ctx.Ctx.env + 1 in
      List.fold_right
        (fun _ acc ->
          VPi { explicitness = Explicit; domain = VU;
                effects = effect_row_closure placeholder_env empty_effect_row;
                codomain = { env = placeholder_env; body = Nbe.quote param_ctx.Ctx.metas depth acc } })
        m.member_params VU
  in
  (* Phase 1, register: a placeholder per member, sharing its id with the
     finished nominal, so payloads written against it mean the finished type. *)
  let registered =
    List.map
      (fun (m : type_member) ->
        let nominal_id = NominalId.fresh () in
        let placeholder =
          VNominal { id = nominal_id; name = m.member_name; num_params = List.length m.member_params; captures = []; params = [] }
        in
        let param_ctx = param_ctx_of m in
        let placeholder_env = placeholder :: param_ctx.Ctx.env in
        (m, nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty_of m placeholder_env param_ctx))
      members
  in
  let group = List.map (fun ((m : type_member), id, _, _, _, _) -> (id, m.member_name, List.length m.member_params)) registered in
  (* Phase 2, elaborate: every member's payloads, in a context naming every
     member. Those names are temporary - they contribute no width. Each payload
     leaves as a normal form over the member's params and the declaring scope. *)
  let elaborated =
    List.map
      (fun ((m : type_member), nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty) ->
        let group_ctx =
          List.fold_left
            (fun gctx ((other : type_member), _, other_placeholder, _, _, other_ty) ->
              if other.member_params = [] then Ctx.define gctx other.member_key VU other_placeholder
              else
                let former = nominal_former_term param_ctx.Ctx.metas ~depth:param_ctx.Ctx.lvl other_placeholder in
                Ctx.define gctx other.member_key other_ty (Nbe.eval param_ctx.Ctx.metas param_ctx.Ctx.env former))
            param_ctx registered
        in
        let payload_terms =
          List.map
            (fun (cname, payloads) ->
              ( cname,
                List.map
                  (fun payload_expr ->
                    let payload_core, payload_ty = ops.infer group_ctx payload_expr in
                    check_type_like group_ctx payload_ty (Ctx.eval group_ctx payload_core);
                    let closed = close_recursive_payload_group group payload_core in
                    Nbe.quote param_ctx.Ctx.metas param_ctx.Ctx.lvl (Ctx.eval param_ctx closed))
                  payloads ))
            m.member_ctors
        in
        (m, nominal_id, placeholder, param_ctx, placeholder_env, nominal_ty, payload_terms))
      registered
  in
  (* The declaration's own free variables, shared by its members (E11). *)
  let levels, captured =
    capture_payloads ~group_ids:(List.map (fun (id, _, _) -> id) group) ~scope_lvl:ctx.Ctx.lvl ~enclosing:ctx.Ctx.scope_captures
      (List.concat_map
         (fun ((m : type_member), _, _, _, _, _, payload_terms) ->
           List.map (fun (_, payloads) -> (List.length m.member_params, payloads)) payload_terms)
         elaborated)
  in
  let captured = ref captured in
  List.iter
    (fun (_, nominal_id, _, _, _, _, payload_terms) ->
      finish_nominal nominal_id
        (List.map (fun (cname, _) -> let payloads = List.hd !captured in captured := List.tl !captured; (cname, payloads)) payload_terms))
    elaborated;
  let capture_vals = capture_values ctx.Ctx.env ~lvl:ctx.Ctx.lvl levels in
  (* Phase 3, finish: build each nominal and its constructors, and extend the
     context in chain order - params, constructors, then the type. *)
  let kind = if public then Public else Private in
  let ctor_kind = if ctors_private then Private else kind in
  let ctx', results =
    List.fold_left
      (fun (ctx, acc) ((m : type_member), nominal_id, _, param_ctx, placeholder_env, nominal_ty, _) ->
        let num_params = List.length m.member_params in
        let nominal =
          VNominal { id = nominal_id; name = m.member_name; num_params; captures = capture_vals; params = [] }
        in
        let ctor_values, ctor_types =
          List.split
            (List.map
               (fun (cname, payload_clos) ->
                 let ctor_value, ctor_ty =
                   build_ctor param_ctx.Ctx.metas (nominal :: placeholder_env) m.member_name cname num_params payload_clos
                 in
                 ((cname, ctor_value), (cname, ctor_ty)))
               (nominal_constructors nominal_id capture_vals))
        in
        let bind =
          TypeBind { name = m.member_name; kind; ctor_kind; id = nominal_id; num_params;
                     captures = capture_terms ~lvl:ctx.Ctx.lvl levels;
                     ctors = List.map (fun (c, clos) -> (c, List.length clos)) (nominal_constructors nominal_id capture_vals) }
        in
        let ctx' =
          extend_from_slots ctx bind
            (List.map (fun p -> `Param p) m.member_params
            @ List.map2
                (fun key ((_, ctor_value), (_, ctor_ty)) -> `Entry (key, ctor_ty, ctor_value))
                m.member_ctor_keys (List.combine ctor_values ctor_types)
            @ [ `Entry (m.member_key, nominal_ty, nominal) ])
        in
        let fields = (m.member_name, kind, nominal_ty) :: List.map (fun (c, ty) -> (c, ctor_kind, ty)) ctor_types in
        (ctx', (bind, fields) :: acc))
      (ctx, []) elaborated
  in
  (ctx', List.rev results)

(* A struct's accumulated binds and entries after a type group. *)
let type_group_entries (acc_binds, acc_entries) results =
  List.fold_left
    (fun (acc_binds, acc_entries) (bind, fields) ->
      (bind :: acc_binds, List.rev_append (List.map (fun (name, kind, ty) -> StructField (name, kind, ty)) fields) acc_entries))
    (acc_binds, acc_entries) results

(* An exported name may not also be another public member of the module - one of
   its own or another export's. [exported] holds the names exported so far,
   [seen] every public name so far. *)
let check_export_clash ~exported ~seen (b : Syntax.struct_binding) entries =
  let names = List.filter_map (function ModuleField (n, Public, _) | ModuleImpl (Some n, Public, _, _) -> Some n | _ -> None) entries in
  let is_export = match b with Syntax.ExportBinding _ -> true | _ -> false in
  (* A constructor may share the name of the type it is exported from, as a
     declaration's constructor always could: the path then denotes it (I3). *)
  let source = match b with Syntax.ExportBinding { m = { kind = Syntax.Var id; _ }; _ } -> Some (Syntax.label id.name) | _ -> None in
  List.iter
    (fun n ->
      let own_type = Option.equal String.equal source (Some n) in
      if Hashtbl.mem exported n || (is_export && Hashtbl.mem seen n && not own_type) then raise (ElabError (ExportClash n));
      if is_export then Hashtbl.replace exported n ();
      Hashtbl.replace seen n ())
    names

(** Stage 7: per-binding module elaboration. Processes a single
    [Syntax.struct_binding] and returns the updated elaboration context,
    the resulting [Core.struct_binding_term] list, and the [Core.module_entry]
    list. [MacroBinding]/[MacroCallBinding] return empty results as the
    expander handles these separately. *)
let elab_module_binding (ops : Elab_ops.t) (ctx : Ctx.t) (b : Syntax.struct_binding)
    : Ctx.t * Core.struct_binding_term list * Core.module_entry list =
  match b with
  | Syntax.MethodBinding _ -> failwith "module binding cannot be method"
  | Syntax.FieldBinding _ -> failwith "a field is expanded only inside a struct"
  | Syntax.MacroBinding _ | Syntax.SyntaxBinding _ -> (ctx, [], [])
  | Syntax.MacroCallBinding _ -> (ctx, [], [])
  | Syntax.HoleBinding _ | Syntax.Items _ | Syntax.InstantiateBinding _ -> failwith "unexpanded declarations should not reach elaboration"
  | Syntax.PatternSynBinding { name = { name = key; _ }; params; rhs; public } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let scrutinee_ty =
        match rhs with
        | Syntax.PatCon (con_path, _) ->
            (match Elab_resolve.find_nominal_for_pattern_head_opt ctx con_path with
             | Some nominal -> nominal
             | None -> VU)
        | _ -> VU
      in
      let core_rhs, _binders = Elab_patterns.elaborate_pat_binders ctx rhs scrutinee_ty in
      let syn_val = VPatternSyn { name; params; rhs = core_rhs; scrutinee_ty } in
      let kind = if public then Public else Private in
      let bind = PatternSynBind (name, kind, syn_val) in
      let ctx' = extend_from_slots ctx bind [ `Entry (key, VU, syn_val) ] in
      (ctx', [bind], [ModuleField (name, kind, VU)])
  | Syntax.ExportBinding { public = false; _ } -> (ctx, [], [])
  | Syntax.ExportBinding { m; names; _ } ->
      (* Every member leaves as a public entry of this module, bound under a key
         nothing spells: an export opens nothing here. *)
      let m_core, m_ty = ops.infer ctx m in
      let members =
        match type_constructors ctx m_core m_ty with
        | Some ctors -> List.map (fun (c, core, ty) -> (Export_field, c, core, ty)) ctors
        | None -> module_exports ctx m_core m_ty
      in
      let members =
        match names with
        | None -> members
        | Some names ->
            List.map
              (fun n -> match List.find_opt (fun (_, c, _, _) -> String.equal c n) members with
                 | Some member -> member
                 | None -> raise (ElabError (ExportUnknownMember n)))
              names
      in
      let ctx0 = ctx in
      let ctx, binds, entries, _ =
        List.fold_left
          (fun (c, binds, entries, i) (what, name, core, ty) ->
            let value = Ctx.eval ctx0 core and core = shift_term i 0 core in
            match what with
            | Export_field ->
                let bind = LetBind (name, Public, core) in
                (extend_from_slots c bind [ `Entry (name ^ "#export", ty, value) ], bind :: binds, ModuleField (name, Public, ty) :: entries, i + 1)
            | Export_impl ->
                let bind = ImplBind (Some name, Public, core, ty) in
                (extend_from_slots c bind [ `Anonymous (ty, value) ], bind :: binds, ModuleImpl (Some name, Public, ty, value) :: entries, i + 1))
          (ctx, [], [], 0) members
      in
      (ctx, binds, entries)
  | Syntax.OpenBinding (mod_expr, label) ->
      (* Module-level [open]: the opened module's public fields are in scope for
         the bindings that *follow* (the caller folds this ctx forward), and the
         open exports nothing itself. [OpenBind] carries the same scope
         extension to the evaluator. *)
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      (match type_constructors ctx mod_core mod_ty with
       | Some ctors ->
           let ctx, ctors = open_type_constructors ~label ctx ctors in
           (ctx, List.rev_map (fun (c, core, _) -> LetBind (c, Private, core)) ctors,
            List.rev_map (fun (c, _, ty) -> ModuleField (c, Private, ty)) ctors)
       | None ->
           let mod_value = Ctx.eval ctx mod_core in
           let ctx, members = open_module_value ~label ctx mod_ty mod_value in
           (ctx, [OpenBind (mod_core, members)], []))
  | Syntax.LetBinding { name = { name = key; _ }; value; public; recursive } ->
      let name = Syntax.label key in
      let val_core, val_ty, val_val, sealed = elab_member_value ops ctx ~value_ctx:(Ctx.clear_self_scope ctx) ~key ~name ~recursive value in
      let kind = if public then Public else Private in
      let bind = LetBind (name, kind, val_core) in
      let ctx' = note_sealed (extend_from_slots ctx bind [ `Entry (key, val_ty, val_val) ]) ctx.Ctx.lvl sealed in
      (ctx', [bind], [ModuleField (name, kind, val_ty)])
  | Syntax.RecGroupBinding { members; public } when Option.is_some (enum_group_decls members) ->
      let ctx', results = elab_type_group ~ctors_private:true ops ctx ~members:(Option.get (enum_group_decls members)) ~public in
      (ctx', List.rev_map fst results,
       List.rev_map (fun (name, kind, ty) -> ModuleField (name, kind, ty)) (List.concat_map snd results))
  | Syntax.RecGroupBinding { members; public } ->
      let kind = if public then Public else Private in
      let extend ctx (key, name, core, ty, finished) = extend_from_slots ctx (LetBind (name, kind, core)) [ `Entry (key, ty, finished) ] in
      let ctx', members =
        elab_rec_group ops ctx ~value_ctx:Ctx.clear_self_scope ~extend
          (List.map (fun ((n : Syntax.id), v) -> (n.name, v)) members)
      in
      (* Last member first: the module fold prepends and reverses. *)
      ( ctx',
        List.rev_map (fun (_, name, core, _, _) -> LetBind (name, kind, core)) members,
        List.rev_map (fun (_, name, _, ty, _) -> ModuleField (name, kind, ty)) members )
  | Syntax.EffectBinding { name = { name = key; _ }; params; ops = eff_ops; public } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let _effect_id, eff, eff_ty, _elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let kind = if public then Public else Private in
      let bind = EffectBind (name, kind, eff) in
      let ctx' = extend_from_slots ctx bind [ `Entry (key, eff_ty, eff) ] in
      (ctx', [bind], [ModuleField (name, kind, eff_ty)])
  | Syntax.TraitBinding { name = { name = key; _ }; params; fields; public } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let kind = if public then Public else Private in
      let bind =
        LetBind (name, kind, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name })
      in
      let ctx' = extend_from_slots ctx bind [ `Entry (key, VU, trait_ty) ] in
      (ctx', [bind], [ModuleField (name, kind, VU)])
  | Syntax.ImplBinding { name; trait; args; fields; public } ->
      let name = Option.map (fun (i : Syntax.id) -> i.name) name in
      let c = elaborate_impl_contribution ops ctx trait args fields in
      let kind = if public then Public else Private in
      let bind = ImplBind (name, kind, c.impl_core, c.impl_dict_ty) in
      let level = ctx.Ctx.lvl in
      let ctx' =
        extend_from_slots ctx bind [ `Anonymous (c.impl_dict_ty, c.impl_value) ]
      in
      let ctx', _evidence = install_impl_evidence ?impl_name:name ctx' c ~level in
      (ctx', [bind], [ModuleImpl (name, kind, c.impl_dict_ty, c.impl_value)])

(* Quoted syntax is its reflection value, built with the scopes it was written
   with. Each hole is checked against the reflection type its position gives it
   (M10); one hole in two kinds of position is an error, not a coercion. *)
let infer ops (ctx : Ctx.t) (expr : Syntax.t) : term * value =
  match expr.kind with
  | Atom (I64 n) -> (Atom (I64 n), VAtomTy Atom_ty.TI64)
  | Atom Unit -> (Atom Unit, VAtomTy Atom_ty.TUnit)
  | Atom (Char c) -> (Atom (Char c), VAtomTy Atom_ty.TChar)
  | Atom (String s) -> (Atom (String s), VAtomTy Atom_ty.TString)
  | Atom (Scopes _ as s) -> (Atom s, VAtomTy Atom_ty.TScopes)
  | Var { name; _ } ->
      let ix, ty = Ctx.lookup ctx name in
      let core = Var ix in
      (core, ty)
  | Self ->
      let ix, ty = Ctx.lookup_self ctx in
      (Var ix, ty)
  | SelfType ->
      (Ctx.quote ctx (Ctx.lookup_self_type ctx), VU)
  | Perform { op = op_path; arg } ->
      let op = Syntax.path_last op_path in
      let effect_core, effect_value, input_ty, output_ty = resolve_perform_operation ctx op_path in
      let arg_core = ops.check ctx arg input_ty in
      emit ctx (singleton_expr_effect effect_core effect_value);
      (Perform { eff = effect_core; op; arg = arg_core }, Nbe.force ctx.metas output_ty)
  | Resume arg -> infer_resume ops ctx arg
  | RefNew e ->
      (* A new reference starts its own heap. *)
      let core, ty = ops.infer ctx e in
      let heap = Ctx.raw_meta ctx in
      emit ctx (mutate_effect ctx heap);
      (RefNew core, VRefTy (heap, ty))
  | RefGet r ->
      let r_core, r_ty = ops.infer ctx r in
      let r_core, r_ty = insert_implicit_args ctx r_core r_ty in
      (match Nbe.force ctx.metas r_ty with
      | VRefTy (heap, elem_ty) -> emit ctx (mutate_effect ctx heap); (RefGet r_core, elem_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | RefSet (r, e) ->
      let r_core, r_ty = ops.infer ctx r in
      let r_core, r_ty = insert_implicit_args ctx r_core r_ty in
      (match Nbe.force ctx.metas r_ty with
      | VRefTy (heap, elem_ty) ->
          let e_core = ops.check ctx e elem_ty in
          record_store ctx heap elem_ty;
          emit ctx (mutate_effect ctx heap);
          (RefSet (r_core, e_core), VAtomTy Atom_ty.TUnit)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Ap (f, Explicitness.Explicit, a) -> infer_ap ops ctx f a
  | Ap (f, Explicitness.Implicit, a) -> infer_ap_implicit ops ctx f a
  | LetRecGroup { members; body } when Option.is_some (enum_group_decls members) ->
      (* The same knot as a module's group; each binding's slots become [Let]s,
         pushed in the order the context was extended. *)
      let body_ctx, results = elab_type_group ~ctors_private:true ops ctx ~members:(Option.get (enum_group_decls members)) ~public:false in
      let body_core, body_ty = ops.infer body_ctx body in
      let slot_def (sl : Core.slot) =
        match sl.sl_source with
        | SlotDef t -> t
        | SlotPlaceholder -> AtomTy Atom_ty.TUnit
        | SlotValue _ -> invalid_arg "a type group pushes no values"
      in
      let slots = List.concat_map (fun (bind, _) -> Option.get (Core.binding_slots bind)) results in
      (List.fold_right (fun sl acc -> Let (U, slot_def sl, acc)) slots body_core, body_ty)
  | LetRecGroup { members; body } ->
      let body_ctx, members =
        elab_rec_group ops ctx ~value_ctx:Fun.id ~extend:(fun ctx (key, _, _, ty, finished) -> Ctx.define ctx key ty finished)
          (List.map (fun ((n : Syntax.id), v) -> (n.name, v)) members)
      in
      let body_core, body_ty = ops.infer body_ctx body in
      let core, _ =
        List.fold_right
          (fun (_, _, core, ty, _) (acc, depth) -> (Let (Nbe.quote ctx.metas depth ty, core, acc), depth - 1))
          members (body_core, ctx.lvl + List.length members - 1)
      in
      (core, body_ty)
  | Let ({ recursive = true; value; _ } as l) when Option.is_some (enum_type_params value) ->
      let params, ctors = Option.get (enum_type_params value) in
      ops.infer ctx { expr with kind = Let { l with recursive = false; value = enum_type_def ~name:l.name ~params ~ctors ~span:value.span } }
  | Let { name = { name; _ }; type_; value; body; recursive } ->
      let value = Syntax.name_enum (Syntax.label name) value in
      discharging ctx ~visible_of:(fun (_, body_ty) -> [ Nbe.quote ctx.metas (ctx.lvl + 1) body_ty ]) @@ fun ctx ->
      if recursive then begin
        let ty_term, fix_core, rec_ty, fix_val = elab_rec_let ops ctx ~name ~type_ value in
        let ctx' = Ctx.define ctx name rec_ty fix_val in
        let body_core, body_ty = ops.infer ctx' body in
        (Let (ty_term, fix_core, body_core), body_ty)
      end else begin
        let (val_core, val_ty), value_effects =
          collecting ctx (fun ctx ->
            match type_ with
            | Some ty_expr ->
                let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx ty_expr in
                let core = ops.check ctx value ty_val in
                (core, ty_val)
            | None -> ops.infer ctx value)
        in
        emit ctx value_effects;
        let gen_val_core, gen_val_ty = generalize ctx val_core val_ty in
        let ty_term = Ctx.quote ctx gen_val_ty in
        let ctx' = let_body_ctx ctx name gen_val_ty gen_val_core value_effects in
        let body_core, body_ty = ops.infer ctx' body in
        if not (is_empty_expr_effects value_effects) && Option.is_some (mentions_generative ctx.metas ctx.lvl gen_val_ty) then
          check_sealed_stays ctx.metas ~inner:ctx.lvl ~depth:ctx'.Ctx.lvl ~name:(Syntax.label name) body_ty;
        (Let (ty_term, gen_val_core, body_core), body_ty)
      end
  | Lam _ when Elab_poly_arrows.lambda_has_poly ~alias:(Elab_type_expr.poly_row_alias ctx) expr ->
      ops.infer ctx (Elab_poly_arrows.lambda ~alias:(Elab_type_expr.poly_row_alias ctx) expr)
  | Lam (param, body) -> infer_lam ops ctx param body
  | Annotated { inner; typ } ->
      let _ty_core, _ty_ty, ty_val = ops.type_value_of_expr ctx typ in
      let core = ops.check ctx inner ty_val in
      (core, ty_val)
  | Prod elems ->
      let cores_tys = List.map (ops.infer ctx) elems in
      let cores = List.map fst cores_tys in
      let tys = List.map snd cores_tys in
      (Prod cores, VProdTy tys)
  (* Reached only when a bound set does not name traits (a bound is read by
     the implicit arrow, [trait_bounds_opt]). *)
  | TraitBoundSet _ -> raise (ElabError (UnknownTrait "a {…} bound lists traits"))
  | ProdTy elems ->
      let core_elems =
        List.map
          (fun elem ->
            let elem_core, elem_ty = pure ctx (fun ctx -> ops.infer ctx elem) in
            check_type_like ctx elem_ty (Ctx.eval ctx elem_core);
            elem_core)
          elems
      in
      (ProdTy core_elems, VU)
  | Arrow _ when Elab_poly_arrows.has_poly ~alias:(Elab_type_expr.poly_row_alias ctx) expr ->
      ops.infer ctx (Elab_poly_arrows.signature ~alias:(Elab_type_expr.poly_row_alias ctx) expr)
  | Arrow (Explicitness.Implicit, Some { name; _ }, a, effects, b) -> (
      match trait_bounds_opt ctx a with
      | Some trait_infos ->
          let type_ctx = Ctx.bind ctx name VU in
          let dict_ctx, dict_layers = bind_trait_bound_dicts ctx name trait_infos in
          let effects = Elab_type_expr.elaborate_effect_row ops type_ctx effects in
          let b_core, b_ty = pure dict_ctx (fun dict_ctx -> ops.infer dict_ctx b) in
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
          let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
          let ctx' = Ctx.bind ctx name a_val in
          let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
          let b_core, b_ty = pure ctx' (fun ctx' -> ops.infer ctx' b) in
          check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
          (Pi { explicitness = Implicit; domain = a_core; effects; codomain = b_core }, VU))
  | Arrow (expl, name, a, effects, b) ->
      let a_core, _a_ty, a_val = ops.type_value_of_expr ctx a in
      let ctx' = Ctx.bind ctx (Option.fold ~none:"_" ~some:(fun (i : Syntax.id) -> i.name) name) a_val in
      let effects = Elab_type_expr.elaborate_effect_row ops ctx' effects in
      let b_core, b_ty = pure ctx' (fun ctx' -> ops.infer ctx' b) in
      check_type_like ctx' b_ty (Ctx.eval ctx' b_core);
      (Pi { explicitness = expl_of_syntax expl; domain = a_core; effects; codomain = b_core }, VU)
  | FieldAccess (head, name)
    when Option.is_some (trait_of_form_opt ctx head) ->
      resolve_trait_method ctx (Option.get (trait_of_form_opt ctx head)) name
  | FieldAccess (e, name) ->
      let (e_core, e_ty), head_effects = collecting ctx (fun ctx -> ops.infer ctx e) in
      emit ctx head_effects;
      let e_core, e_ty = insert_implicit_args ctx e_core e_ty in
      (match type_constructors ctx e_core e_ty with
      | Some ctors -> (
          match List.find_opt (fun (c, _, _) -> String.equal c name) ctors with
          | Some (_, core, ty) -> (core, ty)
          | None -> raise (ElabError (UnboundVariable name)))
      | None ->
      (match Nbe.force_shape ctx.metas (Nbe.module_type_of ctx.metas e_ty (Ctx.eval ctx e_core)) with
      | VModule { entries; partial = _ } -> (
          match find_field_last (fun (n, _, _) -> String.equal n name) (visible_module_fields entries) with
          | Some (_, _, field_ty) ->
              check_generative_escape ctx ~head_effects field_ty;
              (Dot (e_core, name), Nbe.force ctx.metas field_ty)
          (* A named impl is a member: [M.eq_C] has the trait dictionary type,
             which is what makes it usable in evidence position. *)
          | None -> (
              match module_impl_type_opt entries name with
              | Some (Public, impl_ty) -> (Dot (e_core, name), Nbe.force ctx.metas impl_ty)
              | _ -> raise (ElabError (UnboundVariable name))))
      | VStruct { entries; partial } -> (
          let fields = struct_entry_fields entries in
          match Nbe.force ctx.metas (Ctx.eval ctx e_core) with
          | VStruct _ -> (
              match find_field_last (fun (n, _, _) -> String.equal n name) (visible_struct_members fields) with
              | Some (_, _, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
              | None -> raise (ElabError (UnboundVariable name)))
          | _ -> (
              let record_fields = visible_record_fields fields in
              let is_self = match ctx.Ctx.self_entry, e_core with
                | Some _, Var ix -> fst (Ctx.lookup_self ctx) = ix
                | _ -> false
              in
              let method_ty =
                match find_field_last (fun (n, k, _) -> String.equal n name && k = Method) fields with
                | Some _ as m -> m
                | None when is_self ->
                    Option.map (fun ty -> (name, Method, ty)) (List.assoc_opt name ctx.Ctx.self_methods)
                | None -> None
              in
              match find_record_field record_fields name, method_ty with
              | Some (_, field_ty), _ -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
              (* [v.m]: a method call - the method applied to the value. *)
              | None, Some (_, _, method_ty) -> (
                  match Nbe.force ctx.metas method_ty with
                  | VPi { domain; effects; codomain; _ } -> (
                      Ctx.unify ctx e_ty domain;
                      let e_value = Ctx.eval ctx e_core in
                      let row = effect_row_values ctx effects e_value in
                      match Nbe.force ctx.metas (Nbe.closure_apply ctx.metas codomain e_value) with
                      | VPi _ as rest ->
                          emit ctx (expr_effects_of_row_values ctx row);
                          (Dot (e_core, name), rest)
                      (* [method m()] declares no parameters, as [fn()]: [v.m] is a
                         function of [()], and [v.m()] runs it. *)
                      | result ->
                          let quote v = Nbe.quote ctx.metas (ctx.lvl + 1) v in
                          let arrow_row : effect_row = { effects = List.map quote row.effect_values; tails = List.map quote row.tail_values } in
                          ( Lam (Dot (shift_term 1 0 e_core, name)),
                            VPi { explicitness = Explicit; domain = VAtomTy Atom_ty.TUnit;
                                  effects = effect_row_closure ctx.env arrow_row;
                                  codomain = { env = ctx.env; body = quote result } } ))
                  | _ -> raise (ElabError ApplyingNonFunction))
              | None, None when partial && not is_self ->
                  let result_ty = Ctx.raw_meta ctx in
                  let constraint_ty =
                    VStruct { entries = entries @ [ StructField (name, Field, result_ty) ]; partial = true }
                  in
                  Ctx.unify ctx e_ty constraint_ty;
                  (Dot (e_core, name), result_ty)
              | None, None -> raise (ElabError (UnboundVariable name))))
      | VRecord { typ; _ } -> (
          match Nbe.force_shape ctx.metas typ with
          | VStruct { entries; _ } -> (
              match find_record_field (visible_record_fields (struct_entry_fields entries)) name with
              | Some (_, field_ty) -> (Dot (e_core, name), Nbe.force ctx.metas field_ty)
              | None -> raise (ElabError (UnboundVariable name)))
          | _ -> raise (ElabError ApplyingNonFunction))
      | VFlex _ | VRigid _ | VNeutral _ ->
          let result_ty = Ctx.raw_meta ctx in
          let constraint_ty =
            VStruct { entries = [ StructField (name, Field, result_ty) ]; partial = true }
          in
          Ctx.unify ctx e_ty constraint_ty;
          (Dot (e_core, name), result_ty)
      | _ -> raise (ElabError ApplyingNonFunction)))
  | Proj (e, i) ->
      let e_core, e_ty = ops.infer ctx e in
      (match Nbe.force ctx.metas e_ty with
      | VProdTy tys ->
          if i < 0 || i >= List.length tys then
            raise (ElabError TupleLengthMismatch);
          (Proj (e_core, i), Nbe.force ctx.metas (List.nth tys i))
      | _ -> raise (ElabError ApplyingNonFunction))
  | Import { path; _ } when String.equal path Compiler_names.Module_name.std_import_path ->
      (* Reserved path: [import "std"] resolves to the builtin prelude module,
         already elaborated and bound by [init_ctx] as [stdlib]. Resolving it
         here (typecheck layer) keeps the loader from having to name the prelude
         upward across the layer boundary. *)
      let ix, ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
      (Var ix, ty)
  | Import { path; _ } -> (
      match ctx.loader with
      | Some loader ->
          (* A compilation unit's meaning depends only on its own source plus
             what it imports and opens, so it elaborates against the base
             context, not this one. Its term is therefore anchored at the base
             and cannot be spliced in here; its value can, and is. *)
          let _core, value, ty =
              Core_loader.load_elaborated loader path
                ~elaborate:(fun imported expand_ctx ->
                    (* Only the unit's context takes the unit's expander. The
                       importer keeps its own: elaboration reads [eval_and_apply]
                       and the evaluation budget out of [expand_ctx], and after an import
                       both would otherwise come from the last imported unit.
                       See docs/wayfinder/tickets/base-context-shared-state.md. *)
                    let unit_ctx = Ctx.with_expander (Ctx.unit_base ctx) expand_ctx in
                    (* A unit's top-level bindings run when it loads: a program's top. *)
                    let since = MetaContext.count unit_ctx.metas in
                    let (core, ty), effects = collecting unit_ctx (fun unit_ctx -> ops.infer unit_ctx imported) in
                    Elab_effects.require_handled_at_entry ~since unit_ctx effects;
                    (core, Ctx.eval unit_ctx core, ty))
                ~eval_and_apply:Nbe.apply_macro
                ~syntax_nominals:(Elab_stdlib.syntax_nominals ctx)
              in
          (Imported value, ty)
      | None -> raise (ElabError (ImportRequiresLoader path)))
  | RecordConstruct { typ; fields } ->
      let typ_core, typ_ty = ops.infer ctx typ in
      let typ_core, typ_ty = insert_implicit_args ctx typ_core typ_ty in
      (match Nbe.force ctx.metas typ_ty with
      | VStruct { entries = struct_entries; _ } as record_ty ->
          let record_fields = visible_record_fields (struct_entry_fields struct_entries) in
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
                (name, ops.check ctx value field_ty))
              fields
          in
          (RecordConstruct { typ = typ_core; fields = field_cores }, record_ty)
      | _ -> raise (ElabError ApplyingNonFunction))
  | Sig { bindings } -> Elab_type_expr.infer_signature ops ctx bindings
  | Module { bindings } ->
      (* The module's stamp (E11): its first, private slot. Every nominal the
         module declares captures it. At check time it is [()]; at run time a
         module whose evaluation performs something allocates a fresh one, so
         each evaluation's types are distinct instances - one slot, read by
         every constructor alike. *)
      let stamp_ty = VAtomTy Atom_ty.TUnit in
      let stamp_bind def = LetBind (Compiler_names.Module_name.stamp, Private, def) in
      let binding_ctx =
        Ctx.enclosing_scope (Ctx.clear_self_scope ctx) (fun m -> List.iter (fun b -> ignore (Expand.go_struct_binding m b)) bindings)
      in
      let binding_ctx =
        let stamped = extend_from_slots binding_ctx (stamp_bind (Atom Atom.Unit)) [ `Entry (Compiler_names.Module_name.stamp, stamp_ty, VAtom Atom.Unit) ] in
        { stamped with Ctx.scope_captures = List.sort_uniq compare (binding_ctx.Ctx.lvl :: binding_ctx.Ctx.scope_captures) }
      in
      let first_declared = NominalId.next () in
      let (end_ctx, core_bindings, entries), performed =
        collecting binding_ctx (fun binding_ctx ->
          let exported = Hashtbl.create 8 and seen = Hashtbl.create 16 in
          List.fold_left (fun (ctx, acc_binds, acc_entries) b ->
            let ctx', binds, e = elab_module_binding ops ctx b in
            check_export_clash ~exported ~seen b e;
            (ctx', binds @ acc_binds, e @ acc_entries))
          (binding_ctx, [], []) bindings)
      in
      emit ctx performed;
      let generative = not (is_empty_expr_effects performed) in
      if generative then
        for id = first_declared to NominalId.next () - 1 do Hashtbl.replace generative_nominals id () done;
      check_sealed_members_stay ctx ~inner:binding_ctx.Ctx.lvl end_ctx entries;
      let stamp = if generative then RefNew (Atom Atom.Unit) else Atom Atom.Unit in
      let core_bindings = stamp_bind stamp :: List.rev core_bindings in
      let entries = ModuleField (Compiler_names.Module_name.stamp, Private, stamp_ty) :: List.rev entries in
      let fields = module_entry_fields entries in
      validate_module_fields fields;
      (Module { bindings = core_bindings; signature = false }, VModule { entries; partial = false })
  | Struct { bindings } ->
      (* Items elaborate in source order (as a module's do): a field's type sees
         the items written before it, and leaves as a value, quoted at the
         struct's own level - every slot an item adds holds a value. A method
         needs [self]'s type, which is every field, so a method written before
         the last field is elaborated right after it. *)
      let outer = ctx in
      let fields = ref [] and deferred = ref [] in
      let partial_self () =
        VStruct { entries = List.rev_map (fun (name, _, ty) -> StructField (name, Field, ty)) !fields; partial = true }
      in
      (* A method follows the arrow rule (E3): its innermost arrow carries the row
         its [can] declares - none when absent - and its body performs within it. *)
      let method_body ctx effects body =
        let (body_core, body_ty), performed = collecting ctx (fun ctx -> ops.infer ctx body) in
        let row = Elab_type_expr.elaborate_effect_row ops ctx effects in
        check_effect_subset ~in_function:true ctx performed
          { effect_values = List.map (Ctx.eval ctx) row.effects; tail_values = List.map (Ctx.eval ctx) row.tails };
        ((body_core, body_ty), row)
      in
      (* The row sits on the innermost arrow, so it is performed once every
         argument is supplied. *)
      let innermost_row rest row = if rest = [] then row else empty_effect_row in
      let rec elaborate_method_params ctx effects params body =
        match params with
        | [] -> method_body ctx effects body
        | param :: rest ->
            let a_ty =
              match param.Syntax.type_ with
              | Some ty_expr ->
                  let _ty_core, _ty_ty, ty_value = ops.type_value_of_expr ctx ty_expr in
                  ty_value
              | None -> Ctx.raw_meta ctx
            in
            let ctx' = Ctx.bind ctx param.Syntax.name.name a_ty in
            let (body_core, body_ty), row = elaborate_method_params ctx' effects rest body in
            let body_ty_term = Ctx.quote ctx' body_ty in
            let method_ty =
              VPi {
                explicitness = expl_of_syntax param.explicitness;
                domain = a_ty;
                effects = effect_row_closure ctx.env (innermost_row rest row);
                codomain = { env = ctx.env; body = body_ty_term };
              }
            in
            ((Lam body_core, method_ty), row)
      in
      (* Every method's type, known before any method body: its parameter and
         result annotations, else metas the method's own body solves, and its
         declared row. So [self.m] can call a method written later. The self
         parameter is the fields (a partial struct: any struct holding them). *)
      let method_types = ref None in
      (* A method's type against the type it was known at: parameters, rows and
         result - so a [->{_}] row known before the body is the row the body
         solved. *)
      let unify_method_types (ctx : Ctx.t) actual promised =
        let rec go depth actual promised =
          match Nbe.force ctx.metas actual, Nbe.force ctx.metas promised with
          | VPi a, VPi p ->
              Unify.unify ctx.metas ctx.env depth a.domain p.domain;
              let var = VRigid { lvl = depth; spine = [] } in
              Unify.unify ctx.metas ctx.env (depth + 1)
                (VEffectRow (Nbe.eval_effect_row_closure ctx.metas a.effects var))
                (VEffectRow (Nbe.eval_effect_row_closure ctx.metas p.effects var));
              go (depth + 1) (Nbe.closure_apply ctx.metas a.codomain var) (Nbe.closure_apply ctx.metas p.codomain var)
          | a, p -> Unify.unify ctx.metas ctx.env depth a p
        in
        Eval_budget.request ~demand:"a unification" ctx.metas.budget (fun () -> go ctx.lvl actual promised)
      in
      let method_type ctx params effects body =
        let result = match body.Syntax.kind with Syntax.Annotated { typ; _ } -> Some typ | _ -> None in
        let rec go ctx = function
          | [] ->
              let ret = match result with
                | Some typ -> let _, _, v = ops.type_value_of_expr ctx typ in v
                | None -> Ctx.raw_meta ctx
              in
              (ret, Elab_type_expr.elaborate_effect_row ops ctx effects)
          | (param : Syntax.param) :: rest ->
              let a_ty = match param.type_ with
                | Some t -> let _, _, v = ops.type_value_of_expr ctx t in v
                | None -> Ctx.raw_meta ctx
              in
              let ctx' = Ctx.bind ctx param.name.name a_ty in
              let body_ty, row = go ctx' rest in
              (VPi { explicitness = expl_of_syntax param.explicitness; domain = a_ty;
                     effects = effect_row_closure ctx.env (innermost_row rest row);
                     codomain = { env = ctx.env; body = Ctx.quote ctx' body_ty } }, row)
        in
        let self_ty = partial_self () in
        let self_ctx, _ = Ctx.bind_anonymous (Ctx.with_self_type ctx self_ty) self_ty in
        let body_ty, row = go self_ctx params in
        VPi { explicitness = Explicit; domain = self_ty;
              effects = effect_row_closure ctx.env (innermost_row params row);
              codomain = { env = ctx.env; body = Ctx.quote self_ctx body_ty } }
      in
      (* ponytail: method types are read where the first method is elaborated, so a
         method's annotation cannot name an item written after that point. *)
      let method_types_in ctx =
        match !method_types with
        | Some tys -> tys
        | None ->
            let tys =
              List.filter_map
                (function
                  | Syntax.MethodBinding { name; params; effects; body; public } ->
                      Some (Syntax.label name.name, (if public then Method else PrivateMethod), method_type ctx params effects body)
                  | _ -> None)
                bindings
            in
            method_types := Some tys;
            tys
      in
      (* Inside a method, [self] is the struct being defined: its fields and every
         method, so [self.m(…)] is a method call. *)
      let elaborate_method ctx name params effects body =
        let self_ty = partial_self () in
        let tys = method_types_in ctx in
        let ctx = Ctx.with_self_type ctx self_ty in
        let self_ctx, self_entry = Ctx.bind_anonymous ctx self_ty in
        let self_ctx =
          { self_ctx with
            Ctx.self_entry = Some self_entry;
            handler_scopes = [];
            self_methods = List.map (fun (n, _, ty) -> (n, ty)) tys }
        in
        let (body_core, body_ty), row = elaborate_method_params self_ctx effects params body in
        let body_ty_term = Ctx.quote self_ctx body_ty in
        let method_ty =
          VPi {
            explicitness = Explicit;
            domain = self_ty;
            effects = effect_row_closure ctx.env (innermost_row params row);
            codomain = { env = ctx.env; body = body_ty_term };
          }
        in
        (match List.find_opt (fun (n, _, _) -> String.equal n name) tys with
         | Some (_, _, promised) -> unify_method_types ctx method_ty promised
         | None -> ());
        (Lam body_core, method_ty)
      in
      (* [Self] in an item other than a field is the fields written so far; a
         field's own type keeps the enclosing [Self] (a record declaration's). *)
      let rec go ~defer ctx acc items = go_item ~defer (Ctx.with_self_type ctx (partial_self ())) acc items
      and go_item ~defer ctx (acc_binds, acc_entries) = function
        | [] -> (ctx, (acc_binds, acc_entries))
        | Syntax.FieldBinding { name; type_ } :: rest ->
            let mentions (m : Syntax.id) =
              let found = ref false in
              ignore (Expand.map_ids (fun (i : Syntax.id) -> if String.equal i.name m.name then found := true; i) type_);
              !found
            in
            (match List.find_map (function Syntax.MethodBinding { name = m; _ } when mentions m -> Some m | _ -> None) !deferred with
             | Some m -> raise (ElabError (FieldTypeMentionsMethod { field = name; method_ = m.name }))
             | None -> ());
            let _core, _ty_ty, value = ops.type_value_of_expr { ctx with Ctx.self_type = outer.Ctx.self_type } type_ in
            fields := (name, Ctx.quote outer value, value) :: !fields;
            go ~defer ctx (acc_binds, acc_entries) rest
        | (Syntax.MethodBinding _ as m) :: rest when defer ->
            deferred := m :: !deferred;
            go ~defer ctx (acc_binds, acc_entries) rest
        | (Syntax.MacroBinding _ | Syntax.SyntaxBinding _) :: rest -> go ~defer ctx (acc_binds, acc_entries) rest
        | (Syntax.HoleBinding _ | Syntax.Items _ | Syntax.InstantiateBinding _) :: _ -> failwith "unexpanded declarations should not reach elaboration"
        | Syntax.MacroCallBinding _ :: rest -> go ~defer ctx (acc_binds, acc_entries) rest
        | Syntax.PatternSynBinding { name = { name = key; _ }; params; rhs; public } :: rest ->
            let name = Syntax.label key in
            let params = Syntax.names params in
            let scrutinee_ty =
              match rhs with
              | Syntax.PatCon (con_path, _) ->
                  (match Elab_resolve.find_nominal_for_pattern_head_opt ctx con_path with
                   | Some nominal -> nominal
                   | None -> VU)
              | _ -> VU
            in
            let core_rhs, _binders = Elab_patterns.elaborate_pat_binders ctx rhs scrutinee_ty in
            let syn_val = VPatternSyn { name; params; rhs = core_rhs; scrutinee_ty } in
            let kind = if public then Public else Private in
            let bind = PatternSynBind (name, kind, syn_val) in
            let ctx' = extend_from_slots ctx bind [ `Entry (key, VU, syn_val) ] in
            go ~defer ctx'
               (bind :: acc_binds,
                StructField (name, kind, VU) :: acc_entries)
              rest
        | Syntax.ExportBinding { public = false; _ } :: rest -> go ~defer ctx (acc_binds, acc_entries) rest
        | Syntax.ExportBinding _ :: _ -> invalid_arg "export is expanded only as a module item"
        | Syntax.OpenBinding (mod_expr, label) :: rest ->
            let mod_core, mod_ty = ops.infer ctx mod_expr in
            (match type_constructors ctx mod_core mod_ty with
             | Some ctors ->
                 let ctx, ctors = open_type_constructors ~label ctx ctors in
                 go ~defer ctx (List.rev_append (List.map (fun (c, core, _) -> LetBind (c, Private, core)) ctors) acc_binds, acc_entries) rest
             | None ->
                 let mod_value = Ctx.eval ctx mod_core in
                 let ctx, members = open_module_value ~label ctx mod_ty mod_value in
                 go ~defer ctx (OpenBind (mod_core, members) :: acc_binds, acc_entries) rest)
        | Syntax.LetBinding { name = { name = key; _ }; value; public; recursive; _ } :: rest ->
            let name = Syntax.label key in
            let val_core, val_ty, val_val, sealed = elab_member_value ops ctx ~value_ctx:(Ctx.clear_self ctx) ~key ~name ~recursive value in
            let kind = if public then Public else Private in
            let bind = LetBind (name, kind, val_core) in
            let ctx' = note_sealed (extend_from_slots ctx bind [ `Entry (key, val_ty, val_val) ]) ctx.Ctx.lvl sealed in
            let entries = if public then [ StructField (name, kind, val_ty) ] else [] in
            go ~defer ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.RecGroupBinding { members; public } :: rest when Option.is_some (enum_group_decls members) ->
            let ctx', results = elab_type_group ~ctors_private:true ops ctx ~members:(Option.get (enum_group_decls members)) ~public in
            go ~defer ctx' (type_group_entries (acc_binds, acc_entries) results) rest
        | Syntax.RecGroupBinding { members; public } :: rest ->
            let kind = if public then Public else Private in
            let extend ctx (key, name, core, ty, finished) = extend_from_slots ctx (LetBind (name, kind, core)) [ `Entry (key, ty, finished) ] in
            let ctx', members =
              elab_rec_group ops ctx ~value_ctx:Ctx.clear_self ~extend
                (List.map (fun ((n : Syntax.id), v) -> (n.name, v)) members)
            in
            let binds = List.map (fun (_, name, core, _, _) -> LetBind (name, kind, core)) members in
            let entries = if public then List.map (fun (_, name, _, ty, _) -> StructField (name, kind, ty)) members else [] in
            go ~defer ctx' (List.rev_append binds acc_binds, List.rev_append entries acc_entries) rest
        | Syntax.MethodBinding { name = { name = key; _ }; params; effects; body; public } :: rest ->
            let name = Syntax.label key in
            let method_core, method_ty = elaborate_method ctx name params effects body in
            let method_val = Ctx.eval ctx method_core in
            let kind = if public then Method else PrivateMethod in
            let bind = LetBind (name, kind, method_core) in
            let ctx' = extend_from_slots ctx bind [ `Entry (key, method_ty, method_val) ] in
            let entries = if public then [ StructField (name, kind, method_ty) ] else [] in
            go ~defer ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.EffectBinding { name = { name = key; _ }; params; ops = eff_ops; public } :: rest ->
            let name = Syntax.label key in
            let params = Syntax.names params in
            let _effect_id, eff, eff_ty, _elaborated_ops =
              elaborate_eff_family ops ctx name params eff_ops
            in
            let kind = if public then Public else Private in
            let bind = EffectBind (name, kind, eff) in
            let ctx' = extend_from_slots ctx bind [ `Entry (key, eff_ty, eff) ] in
            let entries = if public then [ StructField (name, kind, eff_ty) ] else [] in
            go ~defer ctx'
              (bind :: acc_binds,
               List.rev_append entries acc_entries)
              rest
        | Syntax.TraitBinding _ :: _ ->
            raise (ElabError ApplyingNonFunction)
        | Syntax.ImplBinding { name; trait; args; fields; public } :: rest ->
            let name = Option.map (fun (i : Syntax.id) -> i.name) name in
            let c = elaborate_impl_contribution ops ctx trait args fields in
            let kind = if public then Public else Private in
            let bind = ImplBind (name, kind, c.impl_core, c.impl_dict_ty) in
            let level = ctx.Ctx.lvl in
            let ctx' =
              extend_from_slots ctx bind [ `Anonymous (c.impl_dict_ty, c.impl_value) ]
            in
            let ctx', _evidence = install_impl_evidence ?impl_name:name ctx' c ~level in
            go ~defer ctx'
              (bind :: acc_binds,
               StructImpl (name, kind, c.impl_dict_ty, c.impl_value) :: acc_entries)
              rest
      in
      let is_field = function Syntax.FieldBinding _ -> true | _ -> false in
      let rec split_after_last_field = function
        | items when not (List.exists is_field items) -> ([], items)
        | item :: rest -> let before, after = split_after_last_field rest in (item :: before, after)
        | [] -> ([], [])
      in
      let before, after = split_after_last_field bindings in
      let ctx', acc = go ~defer:true ctx ([], []) before in
      let ctx', acc = go ~defer:false ctx' acc (List.rev !deferred) in
      let _end_ctx, (rev_binds, rev_entries) = go ~defer:false ctx' acc after in
      let con = List.rev !fields in
      check_duplicate_names (List.map (fun (n, _, _) -> n) con);
      (Struct { con_fields = List.map (fun (n, c, _) -> (n, c)) con; bindings = List.rev rev_binds; partial = false },
       VStruct { entries = List.map (fun (n, _, ty) -> StructField (n, Field, ty)) con @ List.rev rev_entries; partial = false })
  | OpenChoice { name = { name; _ }; opens; fallback } -> (
      match Ctx.lookup_choice_opt ctx name { opens; fallback } with
      | Some (ix, ty) -> (Var ix, ty)
      | None -> raise (ElabError (UnboundVariable name)))
  | Open (mod_expr, body, label) -> (
      let mod_core, mod_ty = ops.infer ctx mod_expr in
      match type_constructors ctx mod_core mod_ty with
      | Some ctors ->
          let body_ctx, ctors = open_type_constructors ~label ctx ctors in
          let body_core, body_ty = ops.infer body_ctx body in
          let core, _ = List.fold_right (fun (_, core, ty) (acc, depth) -> (Let (Nbe.quote ctx.metas depth ty, core, acc), depth - 1)) ctors (body_core, ctx.lvl + List.length ctors - 1) in
          (core, body_ty)
      | None ->
          let mod_value = Ctx.eval ctx mod_core in
          let body_ctx, members = open_module_value ~label ctx mod_ty mod_value in
          let body_core, body_ty = ops.infer body_ctx body in
          (Open (mod_core, members, body_core), body_ty))
  | Enum { name; ctors } ->
      let name = Syntax.fresh_id (Option.value name ~default:"enum" ^ "#enum") in
      ops.infer ctx (enum_type_def ~name ~params:[] ~ctors ~span:expr.span)
  | TypeDef { name = { name = key; _ }; params; ctors; body } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let ctor_keys = List.map (fun ((c : Syntax.id), _) -> c.name) ctors in
      let ctors = List.map (fun ((c : Syntax.id), payloads) -> (Syntax.label c.name, payloads)) ctors in
      let num_params = List.length params in
      (* Bind type params as rigid variables (locally abstract types) *)
      let param_ctx =
        List.fold_left
          (fun ctx param_name ->
            Ctx.define ctx param_name VU (VRigid { lvl = ctx.lvl; spine = [] }))
          ctx params
      in
      let nominal_id = NominalId.fresh () in
      let nominal_placeholder = VNominal { id = nominal_id; name; num_params; captures = []; params = [] } in
      let former_ty =
        let depth = param_ctx.lvl + 1 in
        List.fold_right
          (fun _ acc ->
            VPi { explicitness = Explicit; domain = VU;
                  effects = effect_row_closure (nominal_placeholder :: param_ctx.env) empty_effect_row;
                  codomain = { env = nominal_placeholder :: param_ctx.env; body = Nbe.quote param_ctx.metas depth acc } })
          params VU
      in
      let former ctx nominal = Nbe.eval ctx.Ctx.metas ctx.Ctx.env (nominal_former_term ctx.Ctx.metas ~depth:ctx.Ctx.lvl nominal) in
      let recursive_param_ctx =
        if num_params = 0 then Ctx.define param_ctx key VU nominal_placeholder
        else Ctx.define param_ctx key former_ty (former param_ctx nominal_placeholder)
      in
      (* Each payload as a normal form over the params and the declaring scope. *)
      let payload_terms =
        List.map
          (fun (cname, payloads) ->
            ( cname,
              List.map
                (fun payload_expr ->
                  let payload_core, payload_ty = ops.infer recursive_param_ctx payload_expr in
                  check_type_like recursive_param_ctx payload_ty (Ctx.eval recursive_param_ctx payload_core);
                  let closed = close_recursive_payload_term nominal_id name num_params payload_core in
                  Nbe.quote param_ctx.metas param_ctx.lvl (Ctx.eval param_ctx closed))
                payloads ))
          ctors
      in
      let levels, captured =
        capture_payloads ~group_ids:[ nominal_id ] ~scope_lvl:ctx.lvl ~enclosing:ctx.scope_captures
          (List.map (fun (_, payloads) -> (num_params, payloads)) payload_terms)
      in
      let ctor_payload_terms = List.map2 (fun (cname, _) payloads -> (cname, payloads)) payload_terms captured in
      finish_nominal nominal_id ctor_payload_terms;
      let captures = capture_values ctx.env ~lvl:ctx.lvl levels in
      let nominal = VNominal { id = nominal_id; name; num_params; captures; params = [] } in
      (* For parameterized types, build an Explicit VPi chain so Option I64 works.
         For nullary types, just bind with VU as before. *)
      let body_ctx =
        if num_params = 0 then
          Ctx.define param_ctx key VU nominal
        else begin
          let body_ctx = { param_ctx with
            env = nominal :: param_ctx.env;
            lvl = param_ctx.lvl + 1;
            bds = Defined :: param_ctx.bds
          } in
          let type_ty =
            let depth = List.length body_ctx.env in
            List.fold_right
              (fun _ acc ->
                VPi { explicitness = Explicit; domain = VU;
                      effects = effect_row_closure body_ctx.env empty_effect_row;
                      codomain = { env = body_ctx.env; body = Nbe.quote body_ctx.metas depth acc } })
              params VU
          in
          Ctx.define body_ctx key type_ty (former body_ctx nominal)
        end
      in
      let env = nominal :: body_ctx.env in
      let body_ctx =
        List.fold_left2
          (fun ctx (key, _) (cname, payload_clos) ->
            let ctor_val, ctor_ty =
              build_ctor body_ctx.metas env name cname num_params payload_clos in
            Ctx.define ctx key ctor_ty ctor_val)
          body_ctx (List.combine ctor_keys ctors) (nominal_constructors nominal_id captures)
      in
      let body_core, body_ty = ops.infer body_ctx body in
      (NominalDef { id = nominal_id; name; num_params; captures = capture_terms ~lvl:ctx.lvl levels;
                    ctors = ctor_payload_terms; body = body_core },
       body_ty)
  | EffectDef { name = { name = key; _ }; params; ops = eff_ops; body } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let num_params = List.length params in
      let effect_id, eff, eff_ty, elaborated_ops =
        elaborate_eff_family ops ctx name params eff_ops
      in
      let body_ctx = Ctx.define ctx key eff_ty eff in
      let body_core, body_ty = ops.infer body_ctx body in
      (EffectDef { id = effect_id; name; num_params; ops = elaborated_ops; body = body_core },
       body_ty)
  | TraitDef { name = { name = key; _ }; params; fields; body } ->
      let name = Syntax.label key in
      let params = Syntax.names params in
      let trait_info, trait_ty = elaborate_trait ops ctx name params fields in
      let body_ctx = Ctx.define ctx key VU trait_ty in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (U, TraitRef { trait_id = trait_info.trait_id; trait_name = trait_info.trait_name }, body_core), body_ty)
  | ImplDef { name; trait; args; fields; body } ->
      let name = Option.map (fun (i : Syntax.id) -> i.name) name in
      let body_ctx, _evidence, impl_ty, impl_core =
        elaborate_impl ?impl_name:name ops ctx trait args fields in
      let body_core, body_ty = ops.infer body_ctx body in
      (Let (Ctx.quote ctx impl_ty, impl_core, body_core), body_ty)
  | Match (scrutinee, branches) ->
      let within_handler, check_escapes = escape_guard ctx in
      let hctx = with_handler ctx branches in
      let (scrut_core, scrut_ty), scrutinee_effects = collecting hctx (fun ctx -> ops.infer ctx scrutinee) in
      let value_branches = value_branches_of branches in
      let effect_branches = effect_branches_of branches in
      let scrut_ty = maybe_refine_match_scrutinee_ty ctx scrut_ty value_branches in
      let ret_ty = Ctx.raw_meta ctx in
      let refinement_target = refinement_target_of_scrutinee ctx scrut_core in
      let residual = residual_effects ctx scrutinee_effects effect_branches in
      let handled = handled_instances ctx scrutinee_effects effect_branches in
      let (value_branches', effect_branches'), body_effects =
        within_handler handled (fun () -> collecting hctx (fun ctx ->
          ( List.map (fun (pat, body) ->
              let branch_ctx = refine_branch_context ctx refinement_target pat in
              let core_pat, ctx' = elaborate_pat branch_ctx pat scrut_ty in
              let body_core = ops.check ctx' body ret_ty in
              ValueBranch (core_pat, body_core))
              value_branches,
            List.map (elaborate_effect_branch ops ~handler_ctx:hctx ctx ret_ty residual scrutinee_effects) effect_branches )))
      in
      emit_residual ctx ~residual_of:(fun effects -> residual_effects ctx effects effect_branches) scrutinee_effects body_effects;
      check_escapes handled ret_ty;
      let pats = List.map fst (core_value_branches value_branches') in
      check_match_exhaustive ctx scrut_ty pats;
      (Match (scrut_core, value_branches' @ effect_branches'), Nbe.force ctx.metas ret_ty)
  | MacroCall (f, args) ->
      let macro_name = match f.kind with Var n -> Some n.name | _ -> None in
      (match macro_name with
       | Some name ->
           apply_typed_macro ~check:ops.check ctx ~name args ~expected:None
       | None -> failwith "macro-only syntax should not reach elaboration")
  | MacroDef _ | SyntaxDef _ | SyntaxOperatorUse _ | Block _ | Instantiate _ ->
      failwith "macro-only syntax should not reach elaboration"
  | Stx _ -> failwith "stx-only syntax should not reach elaboration"
  (* A typed macro argument, elaborated where the call was written: the output
     places it under at most the binders it added there, so its core is weakened
     past them. An argument whose core holds an [open] cannot be, and elaborates
     again from its expanded form. *)
  | Elaborated { arg; form } -> (
      match Hashtbl.find_opt elaborated_args arg with
      | Some (core, ty, at_lvl, effects) when ctx.lvl >= at_lvl && (ctx.lvl = at_lvl || shiftable core) ->
          emit ctx effects;
          (shift_term (ctx.lvl - at_lvl) 0 core, ty)
      | Some _ -> ops.infer ctx form
      | None -> failwith "Elab_infer: an elaborated macro argument outlived its application")
  | Quote { template; holes } ->
      let ns = Elab_stdlib.syntax_nominals ctx in
      (quote_core ~check:ops.check ctx (Macro_eval.wrap_stx ~nominals:(Some ns) template) holes, ns.expr)
  | QuoteDecls { items; holes } ->
      let ns = Elab_stdlib.syntax_nominals ctx in
      ( quote_core ~check:ops.check ctx (Macro_eval.wrap_stx_decl ~nominals:(Some ns) items) holes,
        Elab_stdlib.resolve ctx [ Compiler_names.Module_name.syntax; Compiler_names.Syntax_name.decls ] )
