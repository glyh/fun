open Syntax

let id_name (id : Syntax.id) : string = id.name

let param_name (param : Syntax.param) : string = id_name param.name

let add_id_scope (scope : Scope_set.t) (id : Syntax.id) : Syntax.id =
  { name = id.name; span = id.span; scope = Scope_set.union id.scope scope }

let same_span_file (a : Source_span.t) (b : Source_span.t) =
  match (a.file, b.file) with
  | Some a, Some b -> String.equal a b
  | None, None -> true
  | _ -> false

let span_contains (outer : Source_span.t) (inner : Source_span.t) =
  outer.synthetic || inner.synthetic
  || (same_span_file outer inner && outer.start_byte <= inner.start_byte
     && inner.end_byte <= outer.end_byte)

(* Which ids in a binder's body receive the binder's scope. In the model, all
   of them - the body as it exists when the binder is expanded - and macro
   output made later receives none. Templates break that order: they
   instantiate during enforestation, so their output already sits in the body.
   An id a template instance introduced carries that instance's intro scope,
   and its span is its token in the template's replacement, so "inside the
   binder's region" is exactly "the template was defined inside the binder's
   body" - when the replacement's tokens would have received the scope. Every
   other id - written at the use site, spliced through a hole, or produced by
   a procedural macro during expansion - is in the body, and receives it. *)
let add_id_scope_if within (scope : Scope_set.t) (id : Syntax.id) : Syntax.id =
  match within with
  | Some region when Scope_set.has_template_intro id.scope && not (span_contains region id.span) -> id
  | _ -> add_id_scope scope id

let add_id_scopes scopes id =
  List.fold_left (fun id scope -> add_id_scope scope id) id scopes

let bind_id scope resolved_name (id : Syntax.id) : Syntax.id =
  { name = resolved_name; span = id.span; scope = Scope_set.union id.scope scope }

let map_path (on_id : Syntax.id -> Syntax.id) (p : Syntax.path) : Syntax.path = { p with head = on_id p.head }

let map_param (on_id : Syntax.id -> Syntax.id) (f : Syntax.t -> Syntax.t) (param : Syntax.param) : Syntax.param =
  { name = on_id param.name;
    type_ = Option.map f param.type_;
    trait_bounds = List.map (map_path on_id) param.trait_bounds;
    explicitness = param.explicitness }

(** Apply [on_id] to every identifier - occurrence and binder - in a form. *)
let rec map_ids (on_id : Syntax.id -> Syntax.id) (stx : t) : t =
  { stx with kind = go_kind on_id stx.kind }

and go_kind (on_id : Syntax.id -> Syntax.id) (k : kind) : kind =
  let go = map_ids on_id in
  match k with
  | Var id -> Var (on_id id)
  | Atom _ -> k
  | Stx s -> Stx (go s)
  | Quote { template; holes } -> Quote { template = go template; holes = List.map (fun (n, h) -> (n, go h)) holes }
  | Self -> k
  | SelfType -> k
  | Ap (f, e, a) -> Ap (go f, e, go a)
  | Lam (p, body) ->
    Lam (map_param on_id go p, go body)
  | Let { name; type_; value; body; recursive } ->
    Let { name = on_id name;
          type_ = Option.map go type_;
          value = go value;
          body = go body;
          recursive }
  | Annotated { inner; typ } ->
    Annotated { inner = go inner; typ = go typ }
  | Prod xs -> Prod (List.map go xs)
  | ProdTy xs -> ProdTy (List.map go xs)
  | Arrow (expl, name, dom, eff, cod) ->
    Arrow (expl, Option.map on_id name, go dom, Option.map (fun e -> { effects = List.map go e.effects; tail = Option.map go e.tail }) eff, go cod)
  | FieldAccess (e, n) -> FieldAccess (go e, n)
  | Proj (e, n) -> Proj (go e, n)
  | RecordConstruct { typ; fields } ->
    RecordConstruct { typ = go typ; fields = List.map (fun (n, e) -> (n, go e)) fields }
  | Struct { con_fields; bindings } ->
    Struct { con_fields = List.map (fun (n, e) -> (n, go e)) con_fields;
             bindings = List.map (go_struct_binding on_id) bindings }
  | Module { bindings } ->
    Module { bindings = List.map (go_struct_binding on_id) bindings }
  | Import _ -> k
  | Open (m, body) -> Open (go m, go body)
  | RecordTypeDef { name; params; fields; body } ->
    RecordTypeDef { name = on_id name; params = List.map on_id params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | TypeDef { name; params; ctors; body } ->
    TypeDef { name = on_id name; params = List.map on_id params; ctors = List.map (fun (n, ps) -> (on_id n, List.map go ps)) ctors; body = go body }
  | EffectDef { name; params; ops; body } ->
    EffectDef { name = on_id name; params = List.map on_id params; ops = List.map (fun op -> { op with input = go op.input; output = go op.output }) ops; body = go body }
  | TraitDef { name; params; fields; body } ->
    TraitDef { name = on_id name; params = List.map on_id params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | ImplDef { name; trait; args; fields; body } ->
    ImplDef { name; trait = map_path on_id trait; args = List.map go args; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | Perform { op; arg } ->
    Perform { op = map_path on_id op; arg = go arg }
  | Resume e -> Resume (go e)
  | RefNew e -> RefNew (go e)
  | RefGet e -> RefGet (go e)
  | RefSet (l, r) -> RefSet (go l, go r)
  | Match (scrut, brs) ->
    Match (go scrut, List.map (go_match_branch on_id) brs)
  | MacroDef { name; value; body; kind } ->
    MacroDef { name = on_id name; value = go value; body = go body; kind }
  | MacroCall (f, args) ->
    MacroCall (go f, List.map go args)
  | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } ->
    SyntaxOperatorUse { operator = on_id operator; fixity; operands = List.map go operands; declaration_span; use_span; unit }

and go_struct_binding (on_id : Syntax.id -> Syntax.id) (binding : Syntax.struct_binding) : Syntax.struct_binding =
  match binding with
  | LetBinding { name; value; public; recursive } ->
    LetBinding { name = on_id name;
                 value = map_ids on_id value; public; recursive }
  | MethodBinding { name; params; body; public } ->
    let new_name = on_id name in
    let new_params = List.map (map_param on_id (map_ids on_id)) params in
    MethodBinding { name = new_name; params = new_params;
                    body = map_ids on_id body; public }
  | TypeBinding { members; public } ->
    TypeBinding { members = List.map (fun (m : type_decl) ->
                    { name = on_id m.name; params = List.map on_id m.params;
                      ctors = List.map (fun (n, ps) -> (on_id n, List.map (map_ids on_id) ps)) m.ctors }) members;
                  public }
  | RecordTypeBinding { name; params; fields; public } ->
    RecordTypeBinding { name = on_id name;
                        params = List.map on_id params; fields = List.map (fun (n, e) -> (n, map_ids on_id e)) fields; public }
  | EffectBinding { name; params; ops; public } ->
    EffectBinding { name = on_id name;
                    params = List.map on_id params; ops = List.map (fun op -> { op with input = map_ids on_id op.input; output = map_ids on_id op.output }) ops; public }
  | TraitBinding { name; params; fields; public } ->
    TraitBinding { name = on_id name;
                   params = List.map on_id params; fields = List.map (fun (n, e) -> (n, map_ids on_id e)) fields; public }
  | ImplBinding { name; trait; args; fields; public } ->
    ImplBinding { name; trait = map_path on_id trait; args = List.map (map_ids on_id) args;
                  fields = List.map (fun (n, e) -> (n, map_ids on_id e)) fields; public }
  | MacroBinding { name; value; public; kind } ->
    MacroBinding { name = on_id name; value = map_ids on_id value; public; kind }
  | MacroCallBinding { f; args } ->
    MacroCallBinding { f = map_ids on_id f; args = List.map (map_ids on_id) args }
  | PatternSynBinding { name; params; rhs; public } ->
    PatternSynBinding { name = on_id name; params = List.map on_id params; rhs = go_pat on_id rhs; public }
  | OpenBinding m -> OpenBinding (map_ids on_id m)

and go_match_branch on_id = function
  | ValueBranch (p, body) -> ValueBranch (go_pat on_id p, map_ids on_id body)
  | EffectBranch { op; arg_pat; body } ->
    EffectBranch { op = map_path on_id op; arg_pat = go_pat on_id arg_pat; body = map_ids on_id body }

and go_pat on_id k = match k with
  | PatCon (path, ps) -> PatCon (map_path on_id path, List.map (go_pat on_id) ps)
  | PatRecord { typ; fields; partial } ->
    PatRecord { typ = map_path on_id typ;
                fields = List.map (fun (n, p) -> (n, Option.map (go_pat on_id) p)) fields;
                partial }
  | PatStructType { fields; partial } ->
    PatStructType { fields = List.map (fun (n, p) -> (n, go_pat on_id p)) fields; partial }
  | PatOr (l, r) -> PatOr (go_pat on_id l, go_pat on_id r)
  | PatProd ps -> PatProd (List.map (go_pat on_id) ps)
  | PatAtom _ -> k
  | PatType _ -> k
  | PatWild -> k
  | PatBind id -> PatBind (on_id id)

(** Add a scope mark to every identifier's scope set, only within [within]'s
    source region when given. *)
let add_scope ?within (s : Scope_set.t) (stx : t) : t = map_ids (add_id_scope_if within s) stx

let add_scope_within region scope stx = add_scope ~within:region scope stx

(** The one hygiene contract of a macro application (M2), for every path that
    applies one. What the application receives gets a fresh use-site scope and
    a fresh intro scope; what it returns has the intro scope flipped. Ids it
    received lose the intro scope again; ids the macro wrote gain it, so they
    cannot capture the caller's and the caller's cannot capture them. *)
type application = {
  receive : Syntax.t -> Syntax.t;
  emit : Syntax.t -> Syntax.t;
  emit_binding : Syntax.struct_binding -> Syntax.struct_binding;
}

let application (ctx : Expand_ctx.t) : application =
  let use_site = Expand_ctx.fresh_scope_set ctx in
  let intro = Expand_ctx.fresh_scope_set ctx in
  let flip (id : Syntax.id) =
    let scope =
      if Scope_set.subset intro id.scope then Scope_set.diff id.scope intro
      else Scope_set.union id.scope intro
    in
    { id with scope }
  in
  { receive = (fun stx -> add_scope intro (add_scope use_site stx));
    emit = map_ids flip;
    emit_binding = go_struct_binding flip }

let add_scopes_within region scopes stx =
  List.fold_left (fun acc scope -> add_scope_within region scope acc) stx scopes

let add_param_scope (scope : Scope_set.t) (param : Syntax.param) : Syntax.param =
  { param with name = add_id_scope scope param.name; type_ = Option.map (add_scope scope) param.type_ }

let add_scopes (scopes : Scope_set.t list) (stx : Syntax.t) : Syntax.t =
  List.fold_left (fun acc scope -> add_scope scope acc) stx scopes

let add_pat_scope (scope : Scope_set.t) pat = go_pat (add_id_scope scope) pat

let add_pat_scopes (scopes : Scope_set.t list) pat =
  List.fold_left (fun acc scope -> add_pat_scope scope acc) pat scopes

let add_struct_binding_scopes scopes binding =
  List.fold_left
    (fun binding scope -> go_struct_binding (add_id_scope scope) binding)
    binding scopes

let add_struct_binding_scopes_within region scopes binding =
  List.fold_left
    (fun binding scope -> go_struct_binding (add_id_scope_if (Some region) scope) binding)
    binding scopes

let syntax_operator_context (arg : Syntax.t) =
  match arg.kind with
  | SyntaxOperatorUse { operator; declaration_span; use_span; _ } ->
      Some
        (Printf.sprintf "syntax operator %S used at %s, declared at %s" operator.name
           (Format.asprintf "%a" Source_span.pp use_span)
           (Format.asprintf "%a" Source_span.pp declaration_span))
  | _ -> None

let syntax_operator_failure arg msg =
  match syntax_operator_context arg with
  | Some ctx -> ctx ^ ": " ^ msg
  | None -> msg

let with_syntax_operator_context arg f =
  match syntax_operator_context arg with
  | None -> f ()
  | Some _ -> (
      try f () with
      | exn -> failwith (syntax_operator_failure arg (Printexc.to_string exn)))

let expand_id_params (ctx : Expand_ctx.t) scopes params =
  let rec go active_scopes param_scopes acc = function
    | [] -> (List.rev acc, List.rev param_scopes)
    | param :: rest ->
      let param = add_id_scopes active_scopes param in
      let scope, resolved_name =
        Expand_ctx.extend_at_fresh ctx ~name:param.name ~base_scope:param.scope
      in
      go (active_scopes @ [ scope ]) (scope :: param_scopes)
        (bind_id scope resolved_name param :: acc) rest
  in
  go scopes [] [] params

(** The main expander: walks the syntax tree, allocates fresh scopes for
    each binder, adds those scopes to identifier occurrences in the binder's
    body. This implements hygienic lexical scoping. *)

(** Did the parser synthesize an implicit binder param for this annotation?
    True for [LegacyExprBinder] and for uppercase non-wildcard [Expr(Named)]. *)
let parser_synthesized_binder (ann : Syntax.MacroAnnotation.t option) : bool =
  match ann with
  | Some (Syntax.MacroAnnotation.LegacyExprBinder _) -> true
  | Some (Syntax.MacroAnnotation.Expr (Some (Named n))) ->
      let is_upper c = c >= 'A' && c <= 'Z' in
      n <> "_" && String.length n > 0 && is_upper n.[0]
  | _ -> false

(** Strip the leading [Lam] from a macro value. This undoes the parser's
    synthesized implicit binder when the semantic resolver determines the
    annotation is a type constraint rather than a binder. *)
let strip_leading_lam (value : Syntax.t) : Syntax.t =
  match value.kind with
  | Syntax.Lam (_, inner) -> inner
  | _ -> value

(** Flatten a curried application spine into its head and the argument list
    in application order (leftmost-written argument first). *)
let rec flatten_ap (stx : t) (acc : (Explicitness.t * t) list) :
    t * (Explicitness.t * t) list =
  match stx.kind with
  | Ap (f, e, a) -> flatten_ap f ((e, a) :: acc)
  | _ -> (stx, acc)

(** Number of parameters a compiled macro transformer accepts, i.e. how many
    curried arguments belong to one macro call. Counts the leading [Lam]s of
    the closure body (the same heuristic as the operator-macro path). *)
let macro_arity (v : Core.value) : int =
  let rec count = function Core.Lam body -> 1 + count body | _ -> 0 in
  match v with
  | Core.VLam { body = { body; _ } } -> 1 + count body
  | _ -> 0

(** Resolve an application/head identifier through the unified binding table
    and decide whether it names a macro. Returns [None] when the name
    resolves to a value binding (so a local value cleanly shadows a macro of
    the same name), or is simply unbound and has no macro entry. Otherwise
    returns [Some (key, entry_opt, provisional)] where [key] is the macro
    table key (the hygienic [resolved_name] for a local macro, or the surface
    name for an imported/operator macro). *)
let macro_head_key (ctx : Expand_ctx.t) (id : Syntax.id) :
    (string * Expand_ctx.macro_entry option * bool) option =
  match Expand_ctx.resolve ctx id with
  | Some { Binding.kind = Binding.Value; _ } -> None
  | Some { Binding.kind = Binding.Macro; resolved_name; _ } ->
      Some
        ( resolved_name,
          Expand_ctx.lookup_macro_entry ctx resolved_name,
          Expand_ctx.is_provisional_macro ctx resolved_name )
  (* Only a context-less id (empty scope set: built from a string) falls back to
     its written name — the string fall-through (S6) that quoted syntax
     retires. An id written in source resolves by scope set alone, so a macro
     bound inside a block is not reachable after it. *)
  | None when not (Scope_set.is_empty id.scope) -> None
  | None -> (
      match Expand_ctx.lookup_macro_entry ctx id.name with
      | Some _ as e -> Some (id.name, e, Expand_ctx.is_provisional_macro ctx id.name)
      | None ->
          if Expand_ctx.is_provisional_macro ctx id.name then Some (id.name, None, true)
          else None)

(* The unit a module expression denotes, when it denotes one: [import "m"]
   directly, or a name bound to one. Macros are members of a unit, so this is
   what both [M.answer(0)] and [open M] need in order to find them. *)
let rec unit_path_of (ctx : Expand_ctx.t) (m : t) : string option =
  match m.kind with
  | Import path -> Some path
  | Var id -> (
      match Expand_ctx.resolve ctx id with
      | Some { Binding.resolved_name; _ } -> Expand_ctx.module_unit ctx resolved_name
      | None -> Expand_ctx.module_unit ctx id.name)
  (* [M.I] where [I] is a unit-valued member of the unit [M] names, so a macro
     reached through a re-export resolves at any depth. *)
  | FieldAccess (inner, field) ->
      Option.bind (unit_path_of ctx inner)
        (fun path -> Expand_ctx.unit_member ctx ~path ~name:field)
  | _ -> None

(* [M.answer] in head position, where [M] names a unit exporting macro [answer].
   [M.answer] on its own is NOT resolved here - a macro is not a runtime value,
   so it stays the field-access error it already was. *)
let macro_member_key (ctx : Expand_ctx.t) (head : t) :
    (string * Expand_ctx.macro_entry) option =
  match head.kind with
  | FieldAccess (m, field) -> (
      match unit_path_of ctx m with
      | Some path ->
          let key = Expand_ctx.unit_macro_key ~path ~name:field in
          Option.map (fun e -> (key, e)) (Expand_ctx.lookup_macro_entry ctx key)
      | None -> None)
  | _ -> None

(* [open]ing a unit delivers its macros bare, the way it delivers its values.
   Each arrives as an ordinary [Macro] binding in the scope-aware binding table,
   under a fresh scope the caller adds to whatever the open covers - so an open
   inside a [do] block does not leak, and a local binding of the same name
   shadows it by the usual rule. *)
let open_unit_macro_scopes (ctx : Expand_ctx.t) (m : t) : Scope_set.t list =
  match unit_path_of ctx m with
  | None -> []
  | Some path ->
      List.map
        (fun name ->
          Expand_ctx.extend_at_kinded ctx ~name ~base_scope:Scope_set.empty
            ~kind:Binding.Macro
            ~resolved_name:(Expand_ctx.unit_macro_key ~path ~name))
        (Expand_ctx.unit_macro_names ctx path)

(* The scope set to hang a dotted macro call's synthesised head on: the one the
   module expression itself carries, so the rewritten head stays in the same
   hygienic position as what it replaces. *)
let member_scope (m : t) : Scope_set.t =
  match m.kind with Var id -> id.scope | _ -> Scope_set.empty

(* A path's head is an occurrence like any other: resolved by scope set and
   renamed to its binder's resolved name. Its members are labels, left alone. *)
let expand_path (ctx : Expand_ctx.t) (p : Syntax.path) : Syntax.path =
  match Expand_ctx.resolve ctx p.head with
  | Some info -> { p with head = { p.head with name = info.resolved_name } }
  | None -> p

let rec expand (ctx : Expand_ctx.t) (stx : t) : t =
  match stx.kind with
  | Var id ->
    begin match Expand_ctx.resolve ctx id with
    | Some info -> { stx with kind = Var { id with name = info.resolved_name } }
    | None -> stx
    end
  | Atom _ | Self | SelfType | Stx _ -> stx
  | Quote { template; holes } ->
    (* The template is data: nothing in it is resolved or renamed here. *)
    let prune (id : Syntax.id) = { id with scope = Expand_ctx.prune_to_definition_site ctx id.scope } in
    { stx with kind = Quote { template = map_ids prune template;
                              holes = List.map (fun (n, h) -> (n, expand ctx h)) holes } }
  | Import path ->
    Option.iter (fun f -> f ctx path) ctx.Expand_ctx.load_macros;
    stx
  | Lam (param, body) ->
    let pname = param_name param in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:pname ~base_scope:param.name.scope in
    let body = expand ctx (add_scope_within stx.span scope body) in
    let param = { param with name = bind_id scope resolved_name param.name; type_ = Option.map (expand ctx) param.type_ } in
    { stx with kind = Lam (param, body) }
  | Let { name; type_; value; body; recursive } ->
    let binding_name = id_name name in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:binding_name ~base_scope:name.scope in
    let value = if recursive then expand ctx (add_scope_within stx.span scope value) else expand ctx value in
    (* [M = import "m"] makes [M] a handle on the unit, so [M.answer(0)] can
       find its macros. Checked after expansion, since the import may itself be
       what a macro produced. *)
    (match value.kind with
     | Import path -> Expand_ctx.bind_module_unit ctx ~resolved_name ~path
     | _ -> ());
    let body = expand ctx (add_scope_within stx.span scope body) in
    let name = bind_id scope resolved_name name in
    { stx with kind = Let { name; type_ = Option.map (expand ctx) type_; value; body; recursive } }
  | Ap (f, e, a) ->
    let default () = { stx with kind = Ap (expand ctx f, e, expand ctx a) } in
    let head, spine = flatten_ap stx [] in
    (* The head is a macro: gather its arity-many arguments from the spine,
       expand the call in place (or defer type-aware macros to the elaborator),
       and re-apply any remaining spine arguments as an ordinary application
       around the macro's result. *)
    let expand_macro_head ~key ~macro_entry ~head_id =
      let arity = macro_arity macro_entry.Expand_ctx.value in
      let n = List.length spine in
      let take = if arity <= 0 || arity > n then n else arity in
      let rec split k xs =
        if k <= 0 then ([], xs)
        else match xs with
          | x :: tl -> let a, b = split (k - 1) tl in (x :: a, b)
          | [] -> ([], [])
      in
      let macro_spine, rest = split take spine in
      let macro_args = List.map snd macro_spine in
      let head_stx = { head with kind = Var { head_id with name = key } } in
      let macro_result = run_macro_call ctx stx ~key ~macro_entry ~head:head_stx macro_args in
      match rest with
      | [] -> macro_result
      | _ ->
        List.fold_left
          (fun acc (expl, arg) ->
            { stx with kind = Ap (acc, expl, expand ctx arg) })
          macro_result rest
    in
    begin match head.kind, macro_member_key ctx head with
    | FieldAccess (m, field), Some (key, macro_entry) ->
      let head_id = { Syntax.name = field; span = head.span; scope = member_scope m } in
      expand_macro_head ~key ~macro_entry ~head_id
    | Var id, _ ->
      begin match macro_head_key ctx id with
      | Some (key, Some macro_entry, _) -> expand_macro_head ~key ~macro_entry ~head_id:id
      | Some (_, None, true) ->
        failwith (Printf.sprintf "macro '%s' cannot be expanded during its own definition" id.name)
      | Some (_, None, false) | None -> default ()
      end
    | _, _ -> default ()
    end
  | Annotated { inner; typ } ->
    { stx with kind = Annotated { inner = expand ctx inner; typ = expand ctx typ } }
  | Prod xs -> { stx with kind = Prod (List.map (expand ctx) xs) }
  | ProdTy xs -> { stx with kind = ProdTy (List.map (expand ctx) xs) }
  | Arrow (expl, Some name, dom, eff, cod) ->
    let dom = expand ctx dom in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:name.name ~base_scope:name.scope in
    let name = bind_id scope resolved_name name in
    let expand_scoped e = expand ctx (add_scope_within stx.span scope e) in
    let eff = Option.map (fun e -> { effects = List.map expand_scoped e.effects; tail = Option.map expand_scoped e.tail }) eff in
    { stx with kind = Arrow (expl, Some name, dom, eff, expand_scoped cod) }
  | Arrow (expl, None, dom, eff, cod) ->
    { stx with kind = Arrow (expl, None, expand ctx dom, Option.map (fun e -> { effects = List.map (expand ctx) e.effects; tail = Option.map (expand ctx) e.tail }) eff, expand ctx cod) }
  | FieldAccess (e, n) -> { stx with kind = FieldAccess (expand ctx e, n) }
  | Proj (e, n) -> { stx with kind = Proj (expand ctx e, n) }
  | RecordConstruct { typ; fields } ->
    { stx with kind = RecordConstruct { typ = expand ctx typ; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields } }
  | Struct { con_fields; bindings } ->
    { stx with kind = Struct { con_fields = List.map (fun (n, e) -> (n, expand ctx e)) con_fields;
                               bindings = expand_struct_bindings ctx bindings } }
  | Module { bindings } ->
    { stx with kind = Module { bindings = expand_struct_bindings ctx bindings } }
  | Open (m, body) ->
    (* Expand the module expression first: an [open (import "m")] is what loads
       that unit's macros, and they have to be there before the open can bind
       them for the body. *)
    let m' = expand ctx m in
    let scopes = open_unit_macro_scopes ctx m in
    { stx with kind = Open (m', expand ctx (add_scopes_within stx.span scopes body)) }
  | RecordTypeDef { name; params; fields; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = RecordTypeDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes_within e.span param_scopes e))) fields; body = expand ctx (add_scope_within stx.span scope body) } }
  | TypeDef { name; params; ctors; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    let ctors, ctor_scopes =
      List.split
        (List.map
           (fun ((cname : Syntax.id), payload) ->
             (* The constructor is introduced *after* the type name by the same
                declaration, so it is bound under the type's scope rather than
                beside it. As siblings the two scope sets are incomparable, and
                [type T = T I64] - where a constructor shares its type's written
                name - resolves as an ambiguous binding instead of shadowing.
                Nesting makes the constructor strictly more specific, which is
                the same last-wins rule a dotted path and [open] already use. *)
             let ctor_scope =
               Expand_ctx.extend_at ctx ~name:cname.name
                 ~base_scope:(Scope_set.union scope cname.scope)
                 ~resolved_name:cname.name
             in
             ((add_id_scope ctor_scope cname, payload), ctor_scope))
           ctors)
    in
    { stx with kind = TypeDef { name; params; ctors = List.map (fun (n, ps) -> (n, List.map (fun p -> expand ctx (add_scopes_within p.span (scope :: param_scopes) p)) ps)) ctors; body = expand ctx (add_scopes_within stx.span (scope :: ctor_scopes) body) } }
  | EffectDef { name; params; ops; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = EffectDef { name; params; ops = List.map (fun op -> { op with input = expand ctx (add_scopes_within op.input.span param_scopes op.input); output = expand ctx (add_scopes_within op.output.span param_scopes op.output) }) ops; body = expand ctx (add_scope_within stx.span scope body) } }
  | TraitDef { name; params; fields; body } ->
    let scope = Expand_ctx.extend_at ctx ~name:name.name ~base_scope:name.scope ~resolved_name:name.name in
    let name = add_id_scope scope name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = TraitDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes_within e.span param_scopes e))) fields; body = expand ctx (add_scope_within stx.span scope body) } }
  | ImplDef { name; trait; args; fields; body } ->
    { stx with kind = ImplDef { name; trait = expand_path ctx trait; args = List.map (expand ctx) args;
                                fields = List.map (fun (n, e) -> (n, expand ctx e)) fields;
                                body = expand ctx body } }
  | Perform { op; arg } ->
    { stx with kind = Perform { op = expand_path ctx op; arg = expand ctx arg } }
  | Resume e -> { stx with kind = Resume (expand ctx e) }
  | RefNew e -> { stx with kind = RefNew (expand ctx e) }
  | RefGet e -> { stx with kind = RefGet (expand ctx e) }
  | RefSet (l, r) -> { stx with kind = RefSet (expand ctx l, expand ctx r) }
  | Match (scrut, brs) ->
    { stx with kind =
        Match (expand ctx scrut,
               List.map (expand_match_branch ctx) brs) }
  | MacroDef { name; value; body; kind; _ } ->
    begin match ctx.Expand_ctx.elaborate with
    | Some elab ->
      let value = Expand_ctx.in_macro_definition ctx (fun () -> expand ctx value) in
      let lowered = Lower_surface.lower_expr value in
      let macro_fn = elab lowered in
      let resolved_kind = match kind with Some ann -> Syntax.MacroAnnotationAdapter.resolve_kind_only ann | None -> Syntax.MacroKind.default in
      (* Promote the macro into the scope-aware binding table with a fresh
         hygienic [resolved_name] and a [Macro] kind, then key its compiled
         entry by that [resolved_name]. This replaces the old macro_table
         save/restore shadowing hack with ordinary lexical scoping: an inner
         macro (or a value) named [name] cleanly shadows an outer one, and the
         macro's call sites resolve to it through normal binding resolution. *)
      let scope, resolved_name =
        Expand_ctx.extend_at_fresh_kinded ctx ~name:name.name ~base_scope:name.scope ~kind:Binding.Macro () in
      Expand_ctx.register_macro ctx ~name:resolved_name ~value:macro_fn;
      Expand_ctx.register_macro_kind ctx ~name:resolved_name ~kind:resolved_kind;
      expand ctx (add_scope_within stx.span scope body)
    | None ->
      failwith "macro definition requires an elaboration callback in expand context"
    end
  | MacroCall (f, args) ->
    (* [MacroCall] nodes are now compiler-derived (there is no surface [@]
       syntax). They still arrive here from the operator-macro prefix path.
       The argument list is exact (not a curried spine), so it is passed
       through verbatim. *)
    begin match f.kind with
    | Var id ->
      begin match macro_head_key ctx id with
      | Some (key, Some macro_entry, _) ->
        let head_stx = { f with kind = Var { id with name = key } } in
        run_macro_call ctx stx ~key ~macro_entry ~head:head_stx args
      | Some (_, None, true) ->
        failwith (Printf.sprintf "macro '%s' cannot be expanded during its own definition" id.name)
      | Some (_, None, false) | None ->
        { stx with kind = MacroCall (expand ctx f, List.map (expand ctx) args) }
      end
    | _ -> { stx with kind = MacroCall (expand ctx f, List.map (expand ctx) args) }
    end
  | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } ->
    (* Take the body from the unit whose declaration won the fixity. The node
       carries that unit, so there is nothing to resolve here and no way for the
       precedence and the body to come from different units. Falls back to the
       written name for an operator declared in this file, and for the prelude's
       own operators, which are not unit members. *)
    let operator_entry =
      let by_unit =
        Option.bind unit (fun path ->
          Expand_ctx.lookup_macro_entry ctx (Expand_ctx.unit_macro_key ~path ~name:operator.name))
      in
      match by_unit with
      | Some _ as e -> e
      | None -> (
          (* Declared here: the operator's id resolves like any macro head. *)
          match macro_head_key ctx operator with
          | Some (_, (Some _ as e), _) -> e
          | _ -> Expand_ctx.lookup_macro_entry ctx operator.name)
    in
    begin match operator_entry with
    | Some macro_entry ->
      let macro_fn = macro_entry.Expand_ctx.value in
      let macro_nominals = macro_entry.Expand_ctx.syntax_nominals in
      begin match ctx.Expand_ctx.eval_and_apply with
      | Some apply_fn ->
        Expand_ctx.with_macro_fuel ctx ~name:operator.name (fun () ->
          let app = application ctx in
          let operands = List.map app.receive operands in
          let result = match operands with
            | [ lhs; rhs ] ->
                let open Core in
                let rec term_lam_count = function
                  | Lam body -> 1 + term_lam_count body
                  | _ -> 0
                in
                let arity = match macro_fn with
                  | VLam { body = { body; _ }; _ } -> 1 + term_lam_count body
                  | _ -> 0
                in
                if arity >= 2 then
                  let lhs_stx = Macro_eval.wrap_stx ~nominals:macro_nominals lhs in
                  let rhs_stx = Macro_eval.wrap_stx ~nominals:macro_nominals rhs in
                  apply_fn (apply_fn macro_fn lhs_stx) rhs_stx
                else
                  apply_fn macro_fn (Macro_eval.wrap_stx ~nominals:macro_nominals
                    { stx with kind = SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } })
            | [ single ] ->
                let stx = Macro_eval.wrap_stx ~nominals:macro_nominals single in
                apply_fn macro_fn stx
            | _ -> apply_fn macro_fn (Macro_eval.wrap_stx ~nominals:macro_nominals
                     { stx with kind = SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } })
          in
          begin match Macro_eval.unwrap_stx ?nominals:macro_nominals result with
          | Some expanded -> expand ctx (app.emit expanded)
          | None -> failwith (syntax_operator_failure { stx with kind = SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } }
                                ("operator macro did not return a syntax value, got " ^ Macro_eval.value_tag result))
          end)
      | None -> failwith "operator macro call requires an apply callback"
      end
    | _ when Expand_ctx.is_provisional_macro ctx operator.name ->
      failwith (Printf.sprintf "macro '%s' cannot be expanded during its own definition" operator.name)
    | _ ->
      { stx with kind = SyntaxOperatorUse { operator; fixity; operands = List.map (expand ctx) operands; declaration_span; use_span; unit } }
    end

(** Run a resolved procedural-macro call: check kind compatibility against the
    current context, then either defer a type-aware macro to the elaborator
    (producing the internal [MacroCall] node with [Stx]-wrapped args) or expand
    it in place by applying the compiled transformer to its syntax arguments.
    [macro_args] is the exact argument list; [head] is the head with its
    macro-table key as name (so a deferred node resolves in the elaborator). *)
and run_macro_call (ctx : Expand_ctx.t) (stx : t) ~(key : string)
    ~(macro_entry : Expand_ctx.macro_entry) ~(head : t) (macro_args : t list) : t =
  let macro_fn = macro_entry.Expand_ctx.value in
  let macro_nominals = macro_entry.Expand_ctx.syntax_nominals in
  let macro_kind =
    match Expand_ctx.lookup_macro_kind ctx key with
    | Some k -> k
    | None -> Syntax.MacroKind.default
  in
  let ctx_kind = Expand_ctx.get_expansion_position ctx in
  let macro_base = match macro_kind with Syntax.MacroKind.Expr _ -> Syntax.MacroKind.Expr (None, None) | k -> k in
  let ctx_base = match ctx_kind with Syntax.MacroKind.Expr _ -> Syntax.MacroKind.Expr (None, None) | k -> k in
  if macro_base <> ctx_base then
    failwith (Printf.sprintf "macro '%s' has kind %s but was used in %s context"
                key (Syntax.MacroKind.to_string macro_kind) (Syntax.MacroKind.to_string ctx_kind));
  if Syntax.MacroKind.has_type_binding macro_kind then
    (* Defer to elaborator: wrap args in Stx to survive lowering *)
    let wrap_stx arg = { arg with kind = Syntax.Stx arg } in
    { stx with kind = MacroCall (head, List.map (fun a -> wrap_stx (expand ctx a)) macro_args) }
  else begin match ctx.Expand_ctx.eval_and_apply with
    | Some apply_fn ->
      Expand_ctx.with_macro_fuel ctx ~name:key (fun () ->
        let ctx_arg = match macro_args with a :: _ -> a | [] -> stx in
        let app = application ctx in
        let result =
          with_syntax_operator_context ctx_arg (fun () ->
              List.fold_left (fun fn arg ->
                  let arg_stx = Macro_eval.wrap_stx ~nominals:macro_nominals (app.receive arg) in
                  apply_fn fn arg_stx)
                macro_fn macro_args)
        in
        begin match Macro_eval.unwrap_stx ?nominals:macro_nominals result with
        | Some expanded -> expand ctx (app.emit expanded)
        | None ->
            failwith (syntax_operator_failure ctx_arg
              ("macro did not return a syntax value, got " ^ Macro_eval.value_tag result))
        end)
    | None -> failwith "macro call requires an apply callback in expand context"
  end

and expand_struct_bindings_with_scopes ?(after_binding = fun _ -> ()) (ctx : Expand_ctx.t) bindings =
  let rec go active_scopes acc all_scopes = function
    | [] -> (List.rev acc, all_scopes)
    | binding :: rest ->
      let binding = add_struct_binding_scopes active_scopes binding in
      let expanded_bindings, introduced_scopes_list = expand_struct_binding ctx binding in
      let acc =
        List.fold_left (fun acc b -> b :: acc) acc expanded_bindings
      in
      after_binding expanded_bindings;
      let active_scopes = active_scopes @ List.flatten introduced_scopes_list in
      go active_scopes acc (all_scopes @ introduced_scopes_list) rest
  in
  go [] [] [] bindings

and expand_struct_bindings (ctx : Expand_ctx.t) bindings =
  let expanded_bindings, _introduced_scopes_list = expand_struct_bindings_with_scopes ctx bindings in
  List.filter (function Syntax.MacroBinding _ -> false | _ -> true) expanded_bindings

and expand_method_params_body ctx params body =
  let rec go active_scopes param_scopes acc = function
    | [] ->
      (List.rev acc, expand ctx (add_scopes param_scopes body))
    | param :: rest ->
      let param = List.fold_left (fun param scope -> add_param_scope scope param) param active_scopes in
      let pname = param_name param in
      let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~name:pname ~base_scope:param.name.scope in
      let param = { param with name = bind_id scope resolved_name param.name; type_ = Option.map (expand ctx) param.type_ } in
      go (active_scopes @ [ scope ]) (param_scopes @ [ scope ]) (param :: acc) rest
  in
  go [] [] [] params

and expand_struct_binding (ctx : Expand_ctx.t) (binding : Syntax.struct_binding) : Syntax.struct_binding list * Scope_set.t list list =
  match binding with
  | LetBinding { name; value; public; recursive } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let value =
      let prev = Expand_ctx.get_expansion_position ctx in
      Expand_ctx.set_expansion_position ctx Syntax.MacroKind.(Expr (None, None));
      let v = if recursive then expand ctx (add_scope_within value.span scope value) else expand ctx value in
      Expand_ctx.set_expansion_position ctx prev;
      v
    in
    (* Same handle as the expression-level [Let]: [I = import "inner"] inside a
       module makes [I.answer(0)] expand. *)
    (match value.kind with
     | Import path ->
       Expand_ctx.bind_module_unit ctx ~resolved_name:binding_name ~path;
       if public then Expand_ctx.record_own_unit_member ctx ~name:binding_name ~path
     | _ -> ());
    ([LetBinding { name = add_id_scope scope name; value; public; recursive }], [[ scope ]])
  | MethodBinding { name; params; body; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, body = expand_method_params_body ctx params body in
    ([MethodBinding { name = add_id_scope scope name; params; body; public }], [[ scope ]])
  | TypeBinding { members; public } ->
    (* Every member name is introduced before any payload is expanded, so a
       chain's members see each other; separate statements stay sequential. *)
    let member_scopes =
      List.map
        (fun (m : type_decl) ->
          Expand_ctx.extend_at ctx ~name:(id_name m.name) ~base_scope:m.name.scope ~resolved_name:(id_name m.name))
        members
    in
    let members, ctor_scopes =
      List.split
        (List.map2
           (fun (m : type_decl) scope ->
             let params, param_scopes = expand_id_params ctx [] m.params in
             let ctors, ctor_scopes =
               List.split
                 (List.map
                    (fun ((cname : Syntax.id), payload) ->
                      (* The constructor is introduced *after* the type name by the same
                         declaration, so it is bound under the type's scope rather than
                         beside it. As siblings the two scope sets are incomparable, and
                         [type T = T I64] - where a constructor shares its type's written
                         name - resolves as an ambiguous binding instead of shadowing.
                         Nesting makes the constructor strictly more specific, which is
                         the same last-wins rule a dotted path and [open] already use. *)
                      let ctor_scope =
                        Expand_ctx.extend_at ctx ~name:cname.name
                          ~base_scope:(Scope_set.union scope cname.scope)
                          ~resolved_name:cname.name
                      in
                      ((add_id_scope ctor_scope cname, payload), ctor_scope))
                    m.ctors)
             in
             let payload_scopes = member_scopes @ param_scopes in
             ( { name = add_id_scope scope m.name; params;
                 ctors = List.map (fun (n, ps) -> (n, List.map (fun p -> expand ctx (add_scopes payload_scopes p)) ps)) ctors },
               ctor_scopes ))
           members member_scopes)
    in
    ([TypeBinding { members; public }], [member_scopes @ List.concat ctor_scopes])
  | RecordTypeBinding { name; params; fields; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    ([RecordTypeBinding { name = add_id_scope scope name;
                          params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; public }],
     [[ scope ]])
  | EffectBinding { name; params; ops; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    ([EffectBinding { name = add_id_scope scope name;
                      params; ops = List.map (fun op -> { op with input = expand ctx (add_scopes param_scopes op.input); output = expand ctx (add_scopes param_scopes op.output) }) ops; public }],
     [[ scope ]])
  | TraitBinding { name; params; fields; public } ->
    let binding_name = id_name name in
    let scope = Expand_ctx.extend_at ctx ~name:binding_name ~base_scope:name.scope ~resolved_name:binding_name in
    let params, param_scopes = expand_id_params ctx [] params in
    ([TraitBinding { name = add_id_scope scope name;
                     params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; public }],
     [[ scope ]])
  | ImplBinding { name; trait; args; fields; public } ->
    ([ImplBinding { name; trait = expand_path ctx trait; args = List.map (expand ctx) args;
                   fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; public }],
     [[]])
  | PatternSynBinding { name; params; rhs; public } ->
     ([PatternSynBinding { name; params; rhs; public }], [[]])
  | OpenBinding m ->
    (* An open binds no name of its own; the names it brings into scope are
       resolved by the elaborator, not the expander. *)
    let m' = expand ctx m in
    ([OpenBinding m'], [ open_unit_macro_scopes ctx m ])
   | MacroBinding { name; value; public; kind } ->
    begin match ctx.Expand_ctx.elaborate with
    | Some elab ->
      (* Stage 5: resolve kind via driver callback if set, else adapter fallback *)
      let (resolved_kind, strip_lam) =
        match ctx.Expand_ctx.resolve_macro_kind, kind with
        | Some resolve, Some ann ->
            let (semantic_kind, semantic_param) = resolve ann in
            (semantic_kind, parser_synthesized_binder (Some ann) && Option.is_none semantic_param)
        | _ ->
            let k = match kind with Some ann -> Syntax.MacroAnnotationAdapter.resolve_kind_only ann | None -> Syntax.MacroKind.default in
            (k, false)
      in
      let binding_name = id_name name in
      (* Stage 7: introduce name scope and register provisional macro BEFORE
         expansion/elaboration so the macro's own name is known during its
         definition (for future re-expansion-recursion support). The
         provisional marker prevents premature callable lookup. *)
      let scope = Expand_ctx.extend_at_kinded ctx ~name:binding_name ~base_scope:name.scope ~kind:Binding.Macro ~resolved_name:binding_name in
      let macro_snapshot = Expand_ctx.snapshot_macro ctx binding_name in
      Expand_ctx.register_provisional_macro ctx ~name:binding_name ();
      Expand_ctx.register_macro_kind ctx ~name:binding_name ~kind:resolved_kind;
      Fun.protect
        ~finally:(fun () ->
          if Expand_ctx.is_provisional_macro ctx binding_name then
            Expand_ctx.restore_macro_snapshot ctx ~name:binding_name macro_snapshot)
        (fun () ->
          let value = Expand_ctx.in_macro_definition ctx (fun () -> expand ctx value) in
          (* Strip parser-synthesized Lam when semantic resolution says constraint *)
          let value = if strip_lam then strip_leading_lam value else value in
          let lowered = Lower_surface.lower_expr value in
          let macro_fn = elab lowered in
          Expand_ctx.fill_provisional_macro ctx ~name:binding_name ~value:macro_fn;
          ([MacroBinding { name = add_id_scope scope name; value; public; kind }], [[ scope ]]))
    | None ->
      ([MacroBinding { name; value = expand ctx value; public; kind }], [[]])
    end
  | MacroCallBinding { f; args } ->
    let head_macro =
      match f.kind, macro_member_key ctx f with
      | FieldAccess _, Some (key, entry) -> Some (key, Some entry, false)
      | Var id, _ -> macro_head_key ctx id
      | _, _ -> None
    in
    begin match f.kind with
    | Var _ | FieldAccess _ ->
      begin match head_macro with
      | Some (key, Some macro_entry, _) ->
        let macro_fn = macro_entry.Expand_ctx.value in
        let macro_nominals = macro_entry.Expand_ctx.syntax_nominals in
        begin match ctx.Expand_ctx.eval_and_apply with
        | Some apply_fn ->
          let macro_kind = match Expand_ctx.lookup_macro_kind ctx key with
            | Some k -> k | None -> Syntax.MacroKind.default in
          let ctx_kind = Expand_ctx.get_expansion_position ctx in
        let macro_base = match macro_kind with Syntax.MacroKind.Expr _ -> Syntax.MacroKind.Expr (None, None) | _ as k -> k in
        let ctx_base = match ctx_kind with Syntax.MacroKind.Expr _ -> Syntax.MacroKind.Expr (None, None) | _ as k -> k in
        if macro_base <> ctx_base then
            failwith (Printf.sprintf "macro '%s' has kind %s but was used in %s context"
                        key (Syntax.MacroKind.to_string macro_kind) (Syntax.MacroKind.to_string ctx_kind));
           Expand_ctx.with_macro_fuel ctx ~name:key (fun () ->
             let app = application ctx in
             let fn = List.fold_left (fun fn arg ->
                let arg_stx = Macro_eval.wrap_stx ~nominals:macro_nominals (app.receive arg) in
                apply_fn fn arg_stx) macro_fn args in
             let rec force_val v =
               match v with
               | Core.VLam _ | Core.VFlex _ ->
                   let dummy = Core.VStx (Core.StxExpr { Syntax.kind = Atom (Atom.Unit); span = Source_span.synthetic }) in
                   force_val (apply_fn v dummy)
               | _ -> v
             in
             let fn = force_val fn in
             let result = Macro_eval.unwrap_stx_decl ?nominals:macro_nominals fn in
             (match result with
             | Some bindings ->
                 (* Stage 6: recursively process generated bindings through
                    the shared binding-list loop so generated MacroBinding
                    annotations are resolved, macros are compiled/registered,
                    and sibling-generated scopes thread in source order. *)
                 expand_struct_bindings_with_scopes ctx (List.map app.emit_binding bindings)
             | None -> failwith (Printf.sprintf "decl macro '%s' did not return declarations" key)))
        | None -> failwith "macro call requires an apply callback in expand context"
        end
      | Some (key, None, true) ->
        failwith (Printf.sprintf "macro '%s' cannot be expanded during its own definition" key)
      | Some (_, None, false) | None -> ([MacroCallBinding { f = expand ctx f; args = List.map (expand ctx) args }], [[]])
      end
    | _ -> ([MacroCallBinding { f = expand ctx f; args = List.map (expand ctx) args }], [[]])
    end

and expand_match_branch ctx = function
  | ValueBranch (p, body) ->
    let ctx' = Expand_ctx.copy ctx in
    let binder_scopes = expand_pat_binders ctx' p in
    ValueBranch (expand_pat ctx' (add_pat_scopes binder_scopes p), expand ctx' (add_scopes binder_scopes body))
  | EffectBranch { op; arg_pat; body } ->
    let ctx' = Expand_ctx.copy ctx in
    let binder_scopes = expand_pat_binders ctx' arg_pat in
    EffectBranch { op = expand_path ctx op; arg_pat = expand_pat ctx' (add_pat_scopes binder_scopes arg_pat); body = expand ctx' (add_scopes binder_scopes body) }

and expand_pat_binders ctx pat =
  let rec collect (acc : Syntax.id list) = function
    | PatBind id ->
      if List.exists (fun (existing : Syntax.id) -> String.equal existing.name id.name) acc then acc else id :: acc
    | PatCon (_, ps) | PatProd ps -> List.fold_left collect acc ps
    | PatRecord { fields; _ } ->
      List.fold_left (fun acc (_, p) -> match p with Some p -> collect acc p | None -> acc) acc fields
    | PatStructType { fields; _ } ->
      List.fold_left (fun acc (_, p) -> collect acc p) acc fields
    | PatOr (l, r) -> collect (collect acc l) r
    | PatAtom _ | PatType _ | PatWild -> acc
  in
  collect [] pat
  |> List.rev
  |> List.map (fun (id : Syntax.id) ->
      let scope, _resolved_name = Expand_ctx.extend_at_fresh ctx ~name:id.name ~base_scope:id.scope in
      scope)

and expand_pat ctx k = match k with
  | PatBind id ->
    let info = Expand_ctx.resolve ctx id in
    let resolved = match info with Some i -> i.Binding.resolved_name | None -> id.name in
    PatBind { id with name = resolved }
  | PatCon (path, ps) -> PatCon (expand_path ctx path, List.map (expand_pat ctx) ps)
  | PatRecord { typ; fields; partial } ->
    PatRecord { typ = expand_path ctx typ; fields = List.map (fun (n, p) -> (n, Option.map (expand_pat ctx) p)) fields; partial }
  | PatStructType { fields; partial } ->
    PatStructType { fields = List.map (fun (n, p) -> (n, expand_pat ctx p)) fields; partial }
  | PatOr (l, r) -> PatOr (expand_pat ctx l, expand_pat ctx r)
  | PatProd ps -> PatProd (List.map (expand_pat ctx) ps)
  | PatAtom _ -> k
  | PatType _ -> k
  | PatWild -> k

let expand_expr ?loader stx =
  let ctx = Expand_ctx.create ?loader () in
  expand ctx stx

let expand_module ?loader stx =
  let ctx = Expand_ctx.create ?loader () in
  expand ctx stx
