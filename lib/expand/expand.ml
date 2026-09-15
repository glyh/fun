open Syntax

let id_name (id : Syntax.id) : string = id.name

let param_name (param : Syntax.param) : string = id_name param.name

let add_id_scope (scope : Scope_set.t) (id : Syntax.id) : Syntax.id =
  { name = id.name; span = id.span; scope = Scope_set.union id.scope scope }

(* Scope sets union associatively, so several scopes are added in one pass. *)
let union_scopes scopes = List.fold_left Scope_set.union Scope_set.empty scopes

let add_id_scopes scopes id = add_id_scope (union_scopes scopes) id

let bind_id scope resolved_name (id : Syntax.id) : Syntax.id =
  { name = resolved_name; span = id.span; scope = Scope_set.union id.scope scope }

(* A declaration binder (M12): a fresh resolved name, bound at a fresh scope, as
   a [let] binder is. What the declaration exports is its written label
   ([Syntax.label]), never the resolved name. *)
let bind_declaration ?(kind = Binding.Value) ?base_scope (ctx : Expand_ctx.t) (name : Syntax.id) =
  let base_scope = Option.value base_scope ~default:name.scope in
  let scope, resolved_name = Expand_ctx.extend_at_fresh_kinded ctx ~span:name.span ~name:name.name ~base_scope ~kind () in
  (scope, bind_id scope resolved_name name)

(** The one traversal every scope, intro, rename and fill goes through: [id] on
    every identifier - occurrence and binder, and a token's scope set, seen as
    an id - then [form], [binding], [pat] and [token] bottom-up on what they
    rebuild, and [rule] on the rules a syntax declaration writes. The rule an
    instantiation names is data from where its form was declared, and is left
    alone. *)
type mapper = {
  id : Syntax.id -> Syntax.id;
  form : t -> t;
  binding : struct_binding -> struct_binding;
  pat : pat -> pat;
  token : Token_tree.token -> Token_tree.token;
  rule : mapper -> rule -> rule;
  (* The rule an instantiation names: left alone except by filling, since a
     rule with holes to fill can only be one the same quoted syntax declared. *)
  used_rule : mapper -> rule -> rule;
}

let token_id = Syntax.token_id

let rec map_forms_with (m : mapper) (stx : t) : t = m.form { stx with kind = go_kind m stx.kind }

and map_path m (p : Syntax.path) : Syntax.path = { p with head = m.id p.head }

and map_param m (param : Syntax.param) : Syntax.param =
  { name = m.id param.name;
    type_ = Option.map (map_forms_with m) param.type_;
    trait_bounds = List.map (map_path m) param.trait_bounds;
    explicitness = param.explicitness }

and map_terms m terms =
  List.map
    (fun (term : Token_tree.t) ->
      match term.datum with
      | Token tok -> { term with datum = Token (m.token { tok with scope = (m.id (token_id tok)).scope }) }
      | Group (d, items, span) -> { term with datum = Group (d, map_terms m items, span) })
    terms

and map_rule_default m (r : rule) : rule =
  let rec part = function
    | PartToken t -> PartToken (List.hd (map_terms m [ t ]))
    | PartGroup (d, parts, span) -> PartGroup (d, List.map part parts, span)
    | PartHole _ as h -> h
  in
  { r with
    pattern = List.map part r.pattern;
    replacement =
      (match r.replacement with
       | ReplaceExpr e -> ReplaceExpr (map_forms_with m e)
       | ReplaceDecls ds -> ReplaceDecls (List.map (go_struct_binding m) ds)) }

and map_role m (role : role) : role =
  match role.meaning with
  | Rules { rules_kind; rules } -> { role with meaning = Rules { rules_kind; rules = List.map (m.rule m) rules } }
  | ApplyValue | AssignRef | CallMacro | OrderGroup -> role

and map_capture m = function
  | CapExpr e -> CapExpr (map_forms_with m e)
  | CapBlock ts -> CapBlock (map_terms m ts)
  | CapId tok -> CapId (m.token { tok with scope = (m.id (token_id tok)).scope })
  | CapPattern p -> CapPattern (go_pat m p)
  | CapDecls ds -> CapDecls (List.map (go_struct_binding m) ds)

and map_instantiation m (inst : instantiation) : instantiation =
  { inst with form = m.id inst.form; rule = m.used_rule m inst.rule;
              captures = List.map (fun (n, c) -> (n, map_capture m c)) inst.captures }

and go_kind m (k : kind) : kind =
  let go = map_forms_with m and on_id = m.id in
  match k with
  | Var id -> Var (on_id id)
  | Atom _ | Elaborated _ -> k
  | Stx s -> Stx (go s)
  | Quote { template; holes } -> Quote { template = go template; holes = List.map (fun (n, h) -> (n, go h)) holes }
  | QuoteDecls { items; holes } ->
    QuoteDecls { items = List.map (go_struct_binding m) items; holes = List.map (fun (n, h) -> (n, go h)) holes }
  | Self -> k
  | SelfType -> k
  | Ap (f, e, a) -> Ap (go f, e, go a)
  | Lam (p, body) -> Lam (map_param m p, go body)
  | Let { name; type_; value; body; recursive } ->
    Let { name = on_id name; type_ = Option.map go type_; value = go value; body = go body; recursive }
  | Annotated { inner; typ } -> Annotated { inner = go inner; typ = go typ }
  | Prod xs -> Prod (List.map go xs)
  | ProdTy xs -> ProdTy (List.map go xs)
  | Arrow (expl, name, dom, eff, cod) ->
    Arrow (expl, Option.map on_id name, go dom, Option.map (fun e -> { effects = List.map go e.effects; tail = Option.map go e.tail }) eff, go cod)
  | FieldAccess (e, n) -> FieldAccess (go e, n)
  | Proj (e, n) -> Proj (go e, n)
  | RecordConstruct { typ; fields } ->
    RecordConstruct { typ = go typ; fields = List.map (fun (n, e) -> (n, go e)) fields }
  | Struct { bindings } -> Struct { bindings = List.map (go_struct_binding m) bindings }
  | Module { bindings } -> Module { bindings = List.map (go_struct_binding m) bindings }
  | Import { path; scope } -> Import { path; scope = (on_id (Syntax.fresh_id ~scope "")).scope }
  | Open (md, body, label) -> Open (go md, go body, label)
  | OpenChoice c -> OpenChoice { c with name = on_id c.name }
  | RecordTypeDef { name; params; fields; body } ->
    RecordTypeDef { name = on_id name; params = List.map on_id params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | TypeDef { name; params; ctors; body } ->
    TypeDef { name = on_id name; params = List.map on_id params; ctors = List.map (fun (n, ps) -> (on_id n, List.map go ps)) ctors; body = go body }
  | EffectDef { name; params; ops; body } ->
    EffectDef { name = on_id name; params = List.map on_id params; ops = List.map (fun op -> { op with input = go op.input; output = go op.output }) ops; body = go body }
  | TraitDef { name; params; fields; body } ->
    TraitDef { name = on_id name; params = List.map on_id params; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | ImplDef { name; trait; args; fields; body } ->
    ImplDef { name; trait = map_path m trait; args = List.map go args; fields = List.map (fun (n, e) -> (n, go e)) fields; body = go body }
  | Perform { op; arg } -> Perform { op = map_path m op; arg = go arg }
  | Resume e -> Resume (go e)
  | RefNew e -> RefNew (go e)
  | RefGet e -> RefGet (go e)
  | RefSet (l, r) -> RefSet (go l, go r)
  | Match (scrut, brs) -> Match (go scrut, List.map (go_match_branch m) brs)
  | Block terms -> Block (map_terms m terms)
  | Instantiate inst -> Instantiate (map_instantiation m inst)
  | MacroDef { name; value; body; kind; output } -> MacroDef { name = on_id name; value = go value; body = go body; kind; output = Option.map go output }
  | SyntaxDef { name; role; body } -> SyntaxDef { name = on_id name; role = map_role m role; body = go body }
  | MacroCall (f, args) -> MacroCall (go f, List.map (map_capture m) args)
  | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } ->
    SyntaxOperatorUse { operator = on_id operator; fixity; operands = List.map go operands; declaration_span; use_span; unit }

and go_struct_binding m (binding : Syntax.struct_binding) : Syntax.struct_binding =
  let go = map_forms_with m and on_id = m.id in
  m.binding
    (match binding with
     | LetBinding { name; value; public; recursive } -> LetBinding { name = on_id name; value = go value; public; recursive }
     | MethodBinding { name; params; body; public } ->
       MethodBinding { name = on_id name; params = List.map (map_param m) params; body = go body; public }
     | TypeBinding { members; public } ->
       TypeBinding { members = List.map (fun (d : type_decl) ->
                       { name = on_id d.name; params = List.map on_id d.params;
                         ctors = List.map (fun (n, ps) -> (on_id n, List.map go ps)) d.ctors }) members;
                     public }
     | RecordTypeBinding { name; params; fields; public } ->
       RecordTypeBinding { name = on_id name; params = List.map on_id params; fields = List.map (fun (n, e) -> (n, go e)) fields; public }
     | EffectBinding { name; params; ops; public } ->
       EffectBinding { name = on_id name; params = List.map on_id params;
                       ops = List.map (fun op -> { op with input = go op.input; output = go op.output }) ops; public }
     | TraitBinding { name; params; fields; public } ->
       TraitBinding { name = on_id name; params = List.map on_id params; fields = List.map (fun (n, e) -> (n, go e)) fields; public }
     | ImplBinding { name; trait; args; fields; public } ->
       ImplBinding { name; trait = map_path m trait; args = List.map go args; fields = List.map (fun (n, e) -> (n, go e)) fields; public }
     | MacroBinding { name; value; public; kind; output } -> MacroBinding { name = on_id name; value = go value; public; kind; output = Option.map go output }
     | MacroCallBinding { f; args } -> MacroCallBinding { f = go f; args = List.map (map_capture m) args }
     | PatternSynBinding { name; params; rhs; public } ->
       PatternSynBinding { name = on_id name; params = List.map on_id params; rhs = go_pat m rhs; public }
     | FieldBinding { name; type_ } -> FieldBinding { name; type_ = go type_ }
     | OpenBinding (md, label) -> OpenBinding (go md, label)
     | SyntaxBinding { name; role; public } -> SyntaxBinding { name = on_id name; role = map_role m role; public }
     | HoleBinding id -> HoleBinding (on_id id)
     | Items terms -> Items (map_terms m terms)
     | InstantiateBinding inst -> InstantiateBinding (map_instantiation m inst))

and go_match_branch m = function
  | ValueBranch (p, body) -> ValueBranch (go_pat m p, map_forms_with m body)
  | EffectBranch { op; arg_pat; body } ->
    EffectBranch { op = map_path m op; arg_pat = go_pat m arg_pat; body = map_forms_with m body }

and go_pat m k =
  m.pat
    (match k with
     | PatCon (path, ps) -> PatCon (map_path m path, List.map (go_pat m) ps)
     | PatRecord { typ; fields; partial } ->
       PatRecord { typ = map_path m typ; fields = List.map (fun (n, p) -> (n, Option.map (go_pat m) p)) fields; partial }
     | PatStructType { fields; partial } -> PatStructType { fields = List.map (fun (n, p) -> (n, go_pat m p)) fields; partial }
     | PatOr (l, r) -> PatOr (go_pat m l, go_pat m r)
     | PatProd ps -> PatProd (List.map (go_pat m) ps)
     | PatAtom _ | PatType _ | PatWild -> k
     | PatBind id -> PatBind (m.id id))

let mapper ?(form = Fun.id) id =
  { id; form; binding = Fun.id; pat = Fun.id; token = Fun.id; rule = map_rule_default; used_rule = (fun _ r -> r) }

(** Apply [on_id] to every identifier, and [on_form] to every form, bottom-up. *)
let map_forms on_id on_form stx = map_forms_with (mapper ~form:on_form on_id) stx

let map_ids on_id stx = map_forms on_id Fun.id stx

let map_binding_ids on_id binding = go_struct_binding (mapper on_id) binding

let map_pat_ids on_id pat = go_pat (mapper on_id) pat

(* The names a declaration binds - not the ids it refers to. *)
let map_binders (f : Syntax.id -> Syntax.id) (binding : struct_binding) : struct_binding =
  match binding with
  | LetBinding b -> LetBinding { b with name = f b.name }
  | MethodBinding b -> MethodBinding { b with name = f b.name }
  | TypeBinding { members; public } ->
    TypeBinding { members = List.map (fun (d : type_decl) -> { d with name = f d.name; ctors = List.map (fun (n, ps) -> (f n, ps)) d.ctors }) members; public }
  | RecordTypeBinding b -> RecordTypeBinding { b with name = f b.name }
  | EffectBinding b -> EffectBinding { b with name = f b.name }
  | TraitBinding b -> TraitBinding { b with name = f b.name }
  | ImplBinding b -> ImplBinding { b with name = Option.map f b.name }
  | MacroBinding b -> MacroBinding { b with name = f b.name }
  | PatternSynBinding b -> PatternSynBinding { b with name = f b.name }
  | SyntaxBinding b -> SyntaxBinding { b with name = f b.name }
  | MacroCallBinding _ | OpenBinding _ | FieldBinding _ | HoleBinding _ | Items _ | InstantiateBinding _ -> binding

(** Add a scope mark to every identifier's scope set - and every unread token's. *)
let add_scope (s : Scope_set.t) (stx : t) : t = map_ids (add_id_scope s) stx

(* Declaration holes [$d] in items are spliced: each is a lone item. *)
let splice_decl_holes captures (bindings : struct_binding list) =
  List.concat_map
    (function
      | HoleBinding { name; _ } as b -> (
          match Option.bind (Syntax.hole_name name) (fun h -> List.assoc_opt h captures) with
          | Some (CapDecls ds) -> ds
          | Some _ -> Expand_error.raise_at (UnfitHole { hole = name; position = "a declaration" })
          | None -> [ b ])
      | b -> [ b ])
    bindings

(* Fill a rule's replacement with what its holes captured (M9). A hole is an id
   spelled [$x]: in expression position it takes the capture itself, as a name
   the captured identifier, as a pattern the captured pattern, as an item the
   captured declarations, and as a [{ … }] the captured block. A rule the
   replacement declares binds its own holes, which filling leaves alone. *)
let rec fill (captures : (string * capture) list) : mapper =
  let find name = Option.bind (Syntax.hole_name name) (fun h -> List.assoc_opt h captures) in
  let unfit what name = Expand_error.raise_at (UnfitHole { hole = name; position = what }) in
  let id (i : Syntax.id) =
    match find i.name with
    | Some (CapId tok) -> token_id tok
    | _ -> i
  in
  let form (stx : t) =
    match stx.kind with
    | Var { name; _ } -> (
        match find name with
        | Some (CapExpr e) -> e
        | Some (CapBlock ts) -> { stx with kind = Block ts }
        | Some (CapId _) | None -> stx
        | Some (CapPattern _ | CapDecls _) -> unfit "an expression" name)
    | Module { bindings } -> { stx with kind = Module { bindings = splice_decl_holes captures bindings } }
    | Struct { bindings } -> { stx with kind = Struct { bindings = splice_decl_holes captures bindings } }
    | Block [ { datum = Token { kind = Ident name; _ }; _ } ] -> (
        match find name with
        | Some (CapBlock ts) -> { stx with kind = Block ts }
        | Some (CapExpr e) -> e
        | Some _ -> unfit "a { … } body" name
        | None -> stx)
    | _ -> stx
  in
  let binding = function
    | Items [ { datum = Token { kind = Ident name; _ }; _ } ] as b -> (
        match find name with
        | Some (CapBlock ts) -> Items ts
        | Some _ -> unfit "a { … } body" name
        | None -> b)
    | b -> b
  in
  let pat = function
    | PatBind { name; _ } as p -> (
        match find name with
        | Some (CapPattern q) -> q
        | Some (CapId _) | None -> p
        | Some _ -> unfit "a pattern" name)
    | p -> p
  in
  let token (tok : Token_tree.token) =
    match tok.kind with
    | Ident name -> (match find name with Some (CapId t) -> { t with span = tok.span } | _ -> tok)
    | _ -> tok
  in
  let rule m (r : rule) =
    let inner = pattern_hole_names r.pattern in
    let captures = List.filter (fun (n, _) -> not (List.mem n inner)) captures in
    ignore m;
    map_rule_default (fill captures) r
  in
  { id; form; binding; pat; token; rule; used_rule = rule }

and pattern_hole_names parts =
  List.concat_map
    (function PartHole { hole; _ } -> [ hole ] | PartGroup (_, ps, _) -> pattern_hole_names ps | PartToken _ -> [])
    parts

(** The one hygiene contract of a macro application (M2), for every path that
    applies one - a syntax form's use included. What the application receives
    gets a fresh use-site scope and a fresh intro scope; what it returns has the
    intro scope flipped. Ids it received lose the intro scope again; ids the
    macro wrote gain it, so they cannot capture the caller's and the caller's
    cannot capture them. *)
type application = {
  receive : Syntax.t -> Syntax.t;
  receive_capture : capture -> capture;
  emit : Syntax.t -> Syntax.t;
  emit_binding : Syntax.struct_binding -> Syntax.struct_binding;
}

let application ?unit (ctx : Expand_ctx.t) : application =
  let use_site = Expand_ctx.fresh_scope_set ctx in
  let intro = Expand_ctx.fresh_scope_set ctx in
  let flip (id : Syntax.id) =
    let scope =
      if Scope_set.subset intro id.scope then Scope_set.diff id.scope intro
      else Scope_set.union id.scope intro
    in
    { id with scope }
  in
  List.iter
    (fun s ->
      Hashtbl.replace ctx.Expand_ctx.intro_scopes s ();
      Option.iter (Hashtbl.replace ctx.Expand_ctx.intro_scope_units s) unit)
    intro;
  let receive_id id = add_id_scope intro (add_id_scope use_site id) in
  (* A declaration an application returns into a definition context binds for
     the rest of that context, so its binders lose the use-site scope (Flatt
     2016, use-site scopes): a name the caller passed binds where the caller
     can see it. *)
  let prune_use_site (id : Syntax.id) = { id with scope = Scope_set.diff id.scope use_site } in
  { receive = map_ids receive_id;
    receive_capture = map_capture (mapper receive_id);
    emit = map_ids flip;
    emit_binding = (fun b -> map_binders prune_use_site (map_binding_ids flip b)) }


let add_param_scope (scope : Scope_set.t) (param : Syntax.param) : Syntax.param =
  { param with name = add_id_scope scope param.name; type_ = Option.map (add_scope scope) param.type_ }

let add_scopes (scopes : Scope_set.t list) (stx : Syntax.t) : Syntax.t =
  if scopes = [] then stx else add_scope (union_scopes scopes) stx

let add_pat_scope (scope : Scope_set.t) pat = map_pat_ids (add_id_scope scope) pat

let add_pat_scopes (scopes : Scope_set.t list) pat =
  if scopes = [] then pat else add_pat_scope (union_scopes scopes) pat

let add_struct_binding_scopes scopes binding =
  if scopes = [] then binding else map_binding_ids (add_id_scope (union_scopes scopes)) binding

(* The syntax operator an application came from, for its errors to name. *)
let syntax_operator_site (arg : Syntax.t) : Expand_error.site option =
  match arg.kind with
  | SyntaxOperatorUse { operator; declaration_span; use_span; _ } ->
      Some { operator = operator.name; use_span; declaration_span }
  | _ -> None

(* M8: a macro's kind must match the expansion position it is used in; the
   check runs before the macro does. *)
let check_macro_kind ~key ~macro_kind ~ctx_kind =
  if Syntax.MacroKind.(position macro_kind <> position ctx_kind) then
    Expand_error.raise_at (KindMismatch { macro = key; kind = macro_kind; position = ctx_kind })

let expand_id_params (ctx : Expand_ctx.t) scopes params =
  let rec go active_scopes param_scopes acc = function
    | [] -> (List.rev acc, List.rev param_scopes)
    | param :: rest ->
      let param = add_id_scopes active_scopes param in
      let scope, resolved_name =
        Expand_ctx.extend_at_fresh ctx ~span:param.span ~name:param.name ~base_scope:param.scope ()
      in
      go (active_scopes @ [ scope ]) (scope :: param_scopes)
        (bind_id scope resolved_name param :: acc) rest
  in
  go scopes [] [] params

(** The main expander: walks the syntax tree, allocates fresh scopes for
    each binder, adds those scopes to identifier occurrences in the binder's
    body. This implements hygienic lexical scoping. *)

(** Flatten a curried application spine into its head and the argument list
    in application order (leftmost-written argument first). *)
let rec flatten_ap (stx : t) (acc : (Explicitness.t * t) list) :
    t * (Explicitness.t * t) list =
  match stx.kind with
  | Ap (f, e, a) -> flatten_ap f ((e, a) :: acc)
  | _ -> (stx, acc)

(* M8: a macro runs only on the arguments it declares - one per explicit
   parameter, [m()] passing the [()] its empty parameter list declares. *)
let macro_arity (ctx : Expand_ctx.t) key =
  List.length (Option.value ~default:[] (Expand_ctx.lookup_macro_params ctx key))

let check_argument_count (ctx : Expand_ctx.t) ~key args =
  let expected = macro_arity ctx key in
  if List.length args <> expected then
    Expand_error.raise_at (ArgumentCount { macro = key; expected; got = List.length args })

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
  | Some { Binding.kind = Binding.Value | Binding.Role; _ } -> None
  | Some { Binding.kind = Binding.Macro; resolved_name; _ } ->
      Some
        ( resolved_name,
          Expand_ctx.lookup_macro_entry ctx resolved_name,
          Expand_ctx.is_provisional_macro ctx resolved_name )
  (* An id resolves by scope set alone (M12): there is no id built from a
     string to fall back for, so an unbound name is not a macro. *)
  | None -> None

(* The unit a module expression denotes, when it denotes one: [import "m"]
   directly, or a name bound to one. Macros are members of a unit, so this is
   what both [M.answer(0)] and [open M] need in order to find them. *)
let rec unit_path_of (ctx : Expand_ctx.t) (m : t) : string option =
  match m.kind with
  | Import { path; _ } -> Some path
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
            ~resolved_name:(Expand_ctx.unit_macro_key ~path ~name) ())
        (Expand_ctx.unit_macro_names ctx path)

(* The scope set to hang a dotted macro call's synthesised head on: the one the
   module expression itself carries, so the rewritten head stays in the same
   hygienic position as what it replaces. *)
let member_scope (m : t) : Scope_set.t =
  match m.kind with Var id -> id.scope | Import { scope; _ } -> scope | _ -> Scope_set.empty

(* The parameter kinds of the macro a call's head names - found as the call
   will find the macro - for the enforester to read its arguments as (M9). *)
let macro_params_of (ctx : Expand_ctx.t) (head : t) =
  let key =
    match head.kind, macro_member_key ctx head with
    | FieldAccess _, Some (key, _) -> Some key
    | Var id, _ -> Option.map (fun (key, _, _) -> key) (macro_head_key ctx id)
    | _ -> None
  in
  Option.bind key (Expand_ctx.lookup_macro_params ctx)

(* The reader of the forms expansion reaches, with the roles bound here. *)
let lazy_env (ctx : Expand_ctx.t) =
  Enforest_util.lazy_env ~macro_params:(macro_params_of ctx) ctx.Expand_ctx.binding_table

let expand_capture expand = function
  | CapExpr e -> CapExpr (expand e)
  | c -> c

(* Where an occurrence resolves (M12): its binder's resolved name, or an open
   choice when some open may supply it or no binder takes it. *)
let resolve_occurrence (ctx : Expand_ctx.t) (id : Syntax.id) : (string, Syntax.open_choice) Either.t =
  if Expand_ctx.is_resolved_name id.name then Left id.name else
  let binder = Expand_ctx.resolve ctx id in
  match binder, Expand_ctx.open_candidates ctx id binder with
  | Some info, [] -> Left info.resolved_name
  | _, opens -> Right { opens; fallback = Option.map (fun i -> i.Binding.resolved_name) binder }

(* A path's head is an occurrence like any other, resolved the way a bare name
   is. Its members are labels, left alone. *)
let expand_path (ctx : Expand_ctx.t) (p : Syntax.path) : Syntax.path =
  match resolve_occurrence ctx p.head with
  | Left name -> { p with head = { p.head with name } }
  | Right choice -> { p with head_choice = Some choice }

(* An expanded macro body inside the unit opens around its definition (M3):
   its scope, and nothing ambient. *)
let in_definition_site_opens (ctx : Expand_ctx.t) (name : Syntax.id) (body : Syntax.t) : Syntax.t =
  List.fold_right
    (fun path body ->
      { body with kind = Open (synth (Import { path; scope = Scope_set.empty }), body, Compiler_names.Module_name.unit_open_label path) })
    (Expand_ctx.enclosing_unit_opens ctx name.scope)
    body

(* A declaration in a block, as the form that scopes it over the rest. *)
let decl_over (binding : struct_binding) (body : t) : t =
  let over kind = { kind; span = body.span } in
  match binding with
  | LetBinding { name; value; recursive; public = false } -> over (Let { name; type_ = None; value; body; recursive })
  | SyntaxBinding { name; role; public = false } -> over (SyntaxDef { name; role; body })
  | MacroBinding { name; value; kind; output; public = false } -> over (MacroDef { name; value; body; kind; output })
  | TypeBinding { members = [ { name; params; ctors } ]; public = false } -> over (TypeDef { name; params; ctors; body })
  | RecordTypeBinding { name; params; fields; public = false } -> over (RecordTypeDef { name; params; fields; body })
  | EffectBinding { name; params; ops; public = false } -> over (EffectDef { name; params; ops; body })
  | TraitBinding { name; params; fields; public = false } -> over (TraitDef { name; params; fields; body })
  | ImplBinding { name; trait; args; fields; public = false } -> over (ImplDef { name; trait; args; fields; body })
  | OpenBinding (m, label) -> over (Open (m, body, label))
  | _ -> Enforest_util.error "a declaration syntax form in a block writes only private lets, types, effects, traits, impls, opens, macros and syntax"

(* The roles unit [path] exports, when [m] is an import of it: bound like any
   binder, at [base_scope] plus [scope] - the region of the open or the binder
   that imported it, so a block's import does not reach past the block. An
   imported rule's replacement was parsed in that unit, so the scopes on its ids
   mean nothing here and are dropped; the ids it introduces resolve through the
   unit's open. *)
let import_roles (ctx : Expand_ctx.t) ~base_scope ~scope (m : t) =
  match m.kind, ctx.Expand_ctx.load_syntax with
  | Import { path; _ }, Some load ->
      let roles = Binding.from_unit path (load path) in
      (match Binding.duplicate_exports_message roles with
       | Some msg -> raise (Enforest_util.Error msg)
       | None -> ());
      let unscoped = map_role (mapper (fun id -> { id with scope = Scope_set.empty })) in
      List.iter
        (fun (name, (role : Syntax.role)) ->
          Expand_ctx.bind ctx ~role:(unscoped role) ~span:role.declared_at ~name ~base_scope ~kind:Binding.Role
            ~resolved_name:(Compiler_names.Module_name.unit_open_label path) scope)
        roles
  | _ -> ()

let rec expand (ctx : Expand_ctx.t) (stx : t) : t =
  match stx.kind with
  | Var id ->
    begin match resolve_occurrence ctx id with
    | Left name -> { stx with kind = Var { id with name } }
    | Right { opens; fallback } -> { stx with kind = OpenChoice { name = id; opens; fallback } }
    end
  | OpenChoice _ -> stx
  | Atom _ | Self | SelfType | Stx _ | Elaborated _ -> stx
  | Quote { template; holes } ->
    (* The template is data: nothing in it is resolved or renamed here. *)
    let prune (id : Syntax.id) = { id with scope = Expand_ctx.prune_to_definition_site ctx id.scope } in
    { stx with kind = Quote { template = map_ids prune template;
                              holes = List.map (fun (n, h) -> (n, expand ctx h)) holes } }
  | QuoteDecls { items; holes } ->
    let prune (id : Syntax.id) = { id with scope = Expand_ctx.prune_to_definition_site ctx id.scope } in
    { stx with kind = QuoteDecls { items = List.map (map_binding_ids prune) items;
                                   holes = List.map (fun (n, h) -> (n, expand ctx h)) holes } }
  | Import { path; _ } ->
    (* An import loads its unit - syntax exports, then macros - wherever it is
       written; its roles are bound only by the open or binder around it
       ([import_roles]). *)
    Option.iter (fun load -> ignore (load path)) ctx.Expand_ctx.load_syntax;
    Option.iter (fun f -> f ctx path) ctx.Expand_ctx.load_macros;
    stx
  | Block terms -> (
    (* Read the body's first statement with the roles bound here, scoped over
       the rest, which stays unread until expansion reaches it (M9). *)
    let env = lazy_env ctx in
    match Enforest.parse_block_decl_form env terms with
    | Some (inst, rest) ->
      let body =
        match Enforest_util.drop_separators rest with
        | [] -> { stx with kind = Atom Atom.Unit }
        | more -> { stx with kind = Block more }
      in
      expand ctx
        (instantiate ctx inst (fun app captures -> function
           | ReplaceDecls ds ->
             let filled = splice_decl_holes captures (List.map (go_struct_binding (fill captures)) ds) in
             List.fold_right decl_over (List.map app.emit_binding filled) body
           | ReplaceExpr _ -> Expand_error.raise_at (NotDeclarations { macro = inst.form.name })))
    | None -> expand ctx (Enforest.parse_block_head env stx.span terms))
  | Instantiate inst ->
    instantiate ctx inst (fun app captures -> function
      | ReplaceExpr e -> expand ctx (app.emit (map_forms_with (fill captures) e))
      | ReplaceDecls _ -> Expand_error.raise_at (NotSyntax { macro = inst.form.name; got = "declarations" }))
  | Lam (param, body) ->
    let pname = param_name param in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~span:param.name.span ~name:pname ~base_scope:param.name.scope () in
    let body = expand ctx (add_scope scope body) in
    let param = { param with name = bind_id scope resolved_name param.name; type_ = Option.map (expand ctx) param.type_ } in
    { stx with kind = Lam (param, body) }
  | Let { name; type_; value; body; recursive } ->
    let binding_name = id_name name in
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~span:name.span ~name:binding_name ~base_scope:name.scope () in
    let value = if recursive then expand ctx (add_scope scope value) else expand ctx value in
    (* [M = import "m"] makes [M] a handle on the unit, so [M.answer(0)] can
       find its macros. Checked after expansion, since the import may itself be
       what a macro produced. *)
    (match value.kind with
     | Import { path; _ } -> Expand_ctx.bind_module_unit ctx ~resolved_name ~path
     | _ -> ());
    import_roles ctx ~base_scope:name.scope ~scope value;
    let body = expand ctx (add_scope scope body) in
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
      let take = macro_arity ctx key in
      let rec split k xs =
        if k <= 0 then ([], xs)
        else match xs with
          | x :: tl -> let a, b = split (k - 1) tl in (x :: a, b)
          | [] -> ([], [])
      in
      let macro_spine, rest = split take spine in
      let macro_args = List.map (fun (_, a) -> CapExpr a) macro_spine in
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
        Expand_error.raise_at (ExpandedDuringDefinition { macro = id.name })
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
    let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~span:name.span ~name:name.name ~base_scope:name.scope () in
    let name = bind_id scope resolved_name name in
    let expand_scoped e = expand ctx (add_scope scope e) in
    let eff = Option.map (fun e -> { effects = List.map expand_scoped e.effects; tail = Option.map expand_scoped e.tail }) eff in
    { stx with kind = Arrow (expl, Some name, dom, eff, expand_scoped cod) }
  | Arrow (expl, None, dom, eff, cod) ->
    { stx with kind = Arrow (expl, None, expand ctx dom, Option.map (fun e -> { effects = List.map (expand ctx) e.effects; tail = Option.map (expand ctx) e.tail }) eff, expand ctx cod) }
  | FieldAccess (e, n) -> { stx with kind = FieldAccess (expand ctx e, n) }
  | Proj (e, n) -> { stx with kind = Proj (expand ctx e, n) }
  | RecordConstruct { typ; fields } ->
    { stx with kind = RecordConstruct { typ = expand ctx typ; fields = List.map (fun (n, e) -> (n, expand ctx e)) fields } }
  | Struct { bindings } ->
    { stx with kind = Struct { bindings = expand_struct_bindings ~in_struct:true ctx bindings } }
  | Module { bindings } ->
    { stx with kind = Module { bindings = expand_struct_bindings ctx bindings } }
  | Open (m, body, _) ->
    (* Expand the module expression first: an [open (import "m")] is what loads
       that unit's macros, and they have to be there before the open can bind
       them for the body. *)
    let m' = expand ctx m in
    let open_scope, label = Expand_ctx.enter_open ctx ~occurrence:(member_scope m) m in
    import_roles ctx ~base_scope:(member_scope m) ~scope:open_scope m';
    let scopes = open_scope :: open_unit_macro_scopes ctx m in
    { stx with kind = Open (m', expand ctx (add_scopes scopes body), label) }
  | RecordTypeDef { name; params; fields; body } ->
    let scope, name = bind_declaration ctx name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = RecordTypeDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; body = expand ctx (add_scope scope body) } }
  | TypeDef { name; params; ctors; body } ->
    let scope, name = bind_declaration ctx name in
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
             let ctor_scope, cname = bind_declaration ~base_scope:(Scope_set.union scope cname.scope) ctx cname in
             ((cname, payload), ctor_scope))
           ctors)
    in
    { stx with kind = TypeDef { name; params; ctors = List.map (fun (n, ps) -> (n, List.map (fun p -> expand ctx (add_scopes (scope :: param_scopes) p)) ps)) ctors; body = expand ctx (add_scopes (scope :: ctor_scopes) body) } }
  | EffectDef { name; params; ops; body } ->
    let scope, name = bind_declaration ctx name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = EffectDef { name; params; ops = List.map (fun op -> { op with input = expand ctx (add_scopes param_scopes op.input); output = expand ctx (add_scopes param_scopes op.output) }) ops; body = expand ctx (add_scope scope body) } }
  | TraitDef { name; params; fields; body } ->
    let scope, name = bind_declaration ctx name in
    let params, param_scopes = expand_id_params ctx [] params in
    { stx with kind = TraitDef { name; params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; body = expand ctx (add_scope scope body) } }
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
  | SyntaxDef { name; role; body } ->
    let scope = Expand_ctx.extend_role ctx ~role ~name in
    expand ctx (add_scope scope body)
  | MacroDef { name; value; body; kind; output } ->
    begin match ctx.Expand_ctx.elaborate with
    | Some elab ->
      let resolved_kind = Syntax.macro_kind ~output kind value in
      let signature = Syntax.macro_signature ~output value in
      let params, value = Syntax.macro_params value in
      let value = Expand_ctx.in_macro_definition ctx (fun () -> expand ctx value) in
      let macro_fn = elab (in_definition_site_opens ctx name value) in
      let signature = Option.map (compile_signature ctx elab name) signature in
      (* Promote the macro into the scope-aware binding table with a fresh
         hygienic [resolved_name] and a [Macro] kind, then key its compiled
         entry by that [resolved_name]. This replaces the old macro_table
         save/restore shadowing hack with ordinary lexical scoping: an inner
         macro (or a value) named [name] cleanly shadows an outer one, and the
         macro's call sites resolve to it through normal binding resolution. *)
      let scope, resolved_name =
        Expand_ctx.extend_at_fresh_kinded ctx ~span:name.span ~name:name.name ~base_scope:name.scope ~kind:Binding.Macro () in
      Expand_ctx.register_macro ?signature ctx ~name:resolved_name ~value:macro_fn;
      Expand_ctx.register_macro_kind ctx ~name:resolved_name ~kind:resolved_kind ~params;
      expand ctx (add_scope scope body)
    | None ->
      Expand_error.raise_at (MissingCallback { callback = "elaborate" })
    end
  | MacroCall (f, args) ->
    (* [MacroCall] nodes are now compiler-derived (there is no surface [@]
       syntax). They still arrive here from the operator-macro prefix path.
       The argument list is exact (not a curried spine), so it is passed
       through verbatim. *)
    let unexpanded () = { stx with kind = MacroCall (expand ctx f, List.map (expand_capture (expand ctx)) args) } in
    begin match f.kind, macro_member_key ctx f with
    | FieldAccess (m, _), Some (key, macro_entry) ->
      let head_stx = { f with kind = Var { Syntax.name = key; span = f.span; scope = member_scope m } } in
      run_macro_call ctx stx ~key ~macro_entry ~head:head_stx args
    | Var id, _ ->
      begin match macro_head_key ctx id with
      | Some (key, Some macro_entry, _) ->
        let head_stx = { f with kind = Var { id with name = key } } in
        run_macro_call ctx stx ~key ~macro_entry ~head:head_stx args
      | Some (_, None, true) ->
        Expand_error.raise_at (ExpandedDuringDefinition { macro = id.name })
      | Some (_, None, false) | None -> unexpanded ()
      end
    | _ -> unexpanded ()
    end
  | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } ->
    (* Take the body from the unit whose declaration won the fixity. The node
       carries that unit, so there is nothing to resolve here and no way for the
       precedence and the body to come from different units. An operator declared
       in this file resolves like any macro head. *)
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
          | _ -> None)
    in
    begin match operator_entry with
    | Some macro_entry ->
      let macro_fn = macro_entry.Expand_ctx.value in
      let macro_nominals = macro_entry.Expand_ctx.syntax_nominals in
      begin match ctx.Expand_ctx.eval_and_apply with
      | Some apply_fn ->
        let apply_fn = apply_fn ctx.Expand_ctx.budget in
        let site : Expand_error.site = { operator = operator.name; use_span; declaration_span } in
        Expand_ctx.macro_application ~site ctx ~name:operator.name ~expand:(expand ctx) (fun () ->
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
          | None ->
              Expand_error.raise_at ~site (NotSyntax { macro = operator.name; got = Macro_eval.value_tag result })
          end)
      | None -> Expand_error.raise_at (MissingCallback { callback = "eval_and_apply" })
      end
    | _ when (match macro_head_key ctx operator with Some (_, _, provisional) -> provisional | None -> false) ->
      Expand_error.raise_at (ExpandedDuringDefinition { macro = operator.name })
    | _ ->
      { stx with kind = SyntaxOperatorUse { operator; fixity; operands = List.map (expand ctx) operands; declaration_span; use_span; unit } }
    end

(* A syntax form's use (M9): a macro application whose macro fills its rule's
   replacement with what the use captured, under the one budget. *)
and instantiate : 'a. Expand_ctx.t -> instantiation -> (application -> (string * capture) list -> rule_replacement -> 'a) -> 'a =
  fun ctx inst k ->
  Expand_ctx.macro_application ctx ~name:inst.form.name ~expand:(expand ctx) (fun () ->
    let app = application ?unit:inst.from_unit ctx in
    k app (List.map (fun (n, c) -> (n, app.receive_capture c)) inst.captures) inst.rule.replacement)

(* A macro's signature, elaborated where the macro is defined, as its value is:
   a name in it that resolves to nothing, or a [T] that is not a type, is an
   error at the definition. *)
and compile_signature (ctx : Expand_ctx.t) elab name (s : Syntax.macro_signature) : Expand_ctx.signature =
  let type_ = Expand_ctx.in_macro_definition ctx (fun () -> expand ctx s.signature) in
  { type_ = elab (in_definition_site_opens ctx name type_); binders = s.binders; params = s.params }

(** Run a resolved procedural-macro call: check kind compatibility against the
    current context, then either defer a type-aware macro to the elaborator
    (producing the internal [MacroCall] node with [Stx]-wrapped args) or expand
    it in place by applying the compiled transformer to its syntax arguments.
    [macro_args] is the exact argument list; [head] is the head with its
    macro-table key as name (so a deferred node resolves in the elaborator). *)
and run_macro_call (ctx : Expand_ctx.t) (stx : t) ~(key : string)
    ~(macro_entry : Expand_ctx.macro_entry) ~(head : t) (macro_args : capture list) : t =
  let macro_fn = macro_entry.Expand_ctx.value in
  let macro_nominals = macro_entry.Expand_ctx.syntax_nominals in
  let macro_kind =
    match Expand_ctx.lookup_macro_kind ctx key with
    | Some k -> k
    | None -> Syntax.MacroKind.default
  in
  (* An application form is an expression: a call in item position is a
     [MacroCallBinding]. *)
  check_macro_kind ~key ~macro_kind ~ctx_kind:Syntax.MacroKind.Expr;
  check_argument_count ctx ~key macro_args;
  match macro_entry.Expand_ctx.signature with
  | Some _ ->
    (* Its signature promises types: the call waits for the elaborator, its
       arguments travelling as syntax objects, marked [Stx]. *)
    let wrap_stx = function CapExpr arg -> CapExpr { arg with kind = Syntax.Stx arg } | c -> c in
    { stx with kind = MacroCall (head, List.map wrap_stx macro_args) }
  | None -> begin match ctx.Expand_ctx.eval_and_apply with
    | Some apply_fn ->
      let apply_fn = apply_fn ctx.Expand_ctx.budget in
      let site =
        match macro_args with CapExpr arg :: _ -> syntax_operator_site arg | _ -> None
      in
      Expand_ctx.macro_application ?site ctx ~name:key ~expand:(expand ctx) (fun () ->
        let app = application ctx in
        let result =
          List.fold_left (fun fn arg ->
              apply_fn fn (Macro_eval.wrap_capture ~nominals:macro_nominals (app.receive_capture arg)))
            macro_fn macro_args
        in
        begin match Macro_eval.unwrap_stx ?nominals:macro_nominals result with
        | Some expanded -> expand ctx (app.emit expanded)
        | None -> Expand_error.raise_at ?site (NotSyntax { macro = key; got = Macro_eval.value_tag result })
        end)
    | None -> Expand_error.raise_at (MissingCallback { callback = "eval_and_apply" })
  end

and expand_struct_bindings_with_scopes ?(after_binding = fun _ -> ()) ?(in_struct = false) (ctx : Expand_ctx.t) bindings =
  let rec go active_scopes acc all_scopes = function
    | [] -> (List.rev acc, all_scopes)
    | Items terms :: rest -> (
      (* The next item, read with the roles bound so far (M9). An item's extent
         is structural (a top-level separator), so only that item takes the
         active scopes; the rest takes them when it is reached. *)
      let stmt, after = Enforest_util.take_statement terms in
      let stmt = map_terms (mapper (add_id_scope (union_scopes active_scopes))) stmt in
      let head = Enforest.parse_module_statement (lazy_env ctx) stmt in
      let after = if Enforest_util.drop_separators after = [] then [] else [ Items after ] in
      go active_scopes acc all_scopes (head @ after @ rest))
    | binding :: rest ->
      let binding = add_struct_binding_scopes active_scopes binding in
      let expanded_bindings, introduced_scopes_list = expand_struct_binding ~in_struct ctx binding in
      let acc =
        List.fold_left (fun acc b -> b :: acc) acc expanded_bindings
      in
      after_binding expanded_bindings;
      let active_scopes = active_scopes @ List.flatten introduced_scopes_list in
      go active_scopes acc (all_scopes @ introduced_scopes_list) rest
  in
  go [] [] [] bindings

and expand_struct_bindings ?in_struct (ctx : Expand_ctx.t) bindings =
  let expanded_bindings, _introduced_scopes_list = expand_struct_bindings_with_scopes ?in_struct ctx bindings in
  List.filter (function Syntax.MacroBinding _ -> false | _ -> true) expanded_bindings

and expand_method_params_body ctx params body =
  let rec go active_scopes param_scopes acc = function
    | [] ->
      (List.rev acc, expand ctx (add_scopes param_scopes body))
    | param :: rest ->
      let param = if active_scopes = [] then param else add_param_scope (union_scopes active_scopes) param in
      let pname = param_name param in
      let scope, resolved_name = Expand_ctx.extend_at_fresh ctx ~span:param.name.span ~name:pname ~base_scope:param.name.scope () in
      let param = { param with name = bind_id scope resolved_name param.name; type_ = Option.map (expand ctx) param.type_ } in
      go (active_scopes @ [ scope ]) (param_scopes @ [ scope ]) (param :: acc) rest
  in
  go [] [] [] params

and expand_struct_binding ?(in_struct = false) (ctx : Expand_ctx.t) (binding : Syntax.struct_binding) : Syntax.struct_binding list * Scope_set.t list list =
  (* A struct exports no syntax: declarations a syntax form or macro writes
     into one are held to what its own items may be. *)
  (match binding with
   | SyntaxBinding { public = true; role; _ } when in_struct ->
     Enforest_util.error (if role.meaning = CallMacro then "pub operator is not supported inside structs" else "pub syntax is not supported inside structs")
   | MacroBinding { public = true; _ } when in_struct -> Enforest_util.error "pub macro is not supported inside structs"
   | _ -> ());
  match binding with
  | LetBinding { name; value; public; recursive } ->
    let written = name in
    let scope, name = bind_declaration ctx name in
    let binding_name = id_name name in
    let value = if recursive then expand ctx (add_scope scope value) else expand ctx value in
    (* Same handle as the expression-level [Let]: [I = import "inner"] inside a
       module makes [I.answer(0)] expand. *)
    (match value.kind with
     | Import { path; _ } ->
       Expand_ctx.bind_module_unit ctx ~resolved_name:binding_name ~path;
       if public then Expand_ctx.record_own_unit_member ctx ~name:(Syntax.label binding_name) ~path
     | _ -> ());
    import_roles ctx ~base_scope:written.scope ~scope value;
    ([LetBinding { name; value; public; recursive }], [[ scope ]])
  | MethodBinding { name; params; body; public } ->
    let scope, name = bind_declaration ctx name in
    let params, body = expand_method_params_body ctx params body in
    ([MethodBinding { name; params; body; public }], [[ scope ]])
  | TypeBinding { members; public } ->
    (* Every member name is introduced before any payload is expanded, so a
       chain's members see each other; separate statements stay sequential. *)
    let member_scopes, member_names = List.split (List.map (fun (m : type_decl) -> bind_declaration ctx m.name) members) in
    let members = List.map2 (fun (m : type_decl) name -> { m with name }) members member_names in
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
                      let ctor_scope, cname = bind_declaration ~base_scope:(Scope_set.union scope cname.scope) ctx cname in
                      ((cname, payload), ctor_scope))
                    m.ctors)
             in
             let payload_scopes = member_scopes @ param_scopes in
             ( { name = m.name; params;
                 ctors = List.map (fun (n, ps) -> (n, List.map (fun p -> expand ctx (add_scopes payload_scopes p)) ps)) ctors },
               ctor_scopes ))
           members member_scopes)
    in
    ([TypeBinding { members; public }], [member_scopes @ List.concat ctor_scopes])
  | RecordTypeBinding { name; params; fields; public } ->
    let scope, name = bind_declaration ctx name in
    let params, param_scopes = expand_id_params ctx [] params in
    ([RecordTypeBinding { name;
                          params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; public }],
     [[ scope ]])
  | EffectBinding { name; params; ops; public } ->
    let scope, name = bind_declaration ctx name in
    let params, param_scopes = expand_id_params ctx [] params in
    ([EffectBinding { name;
                      params; ops = List.map (fun op -> { op with input = expand ctx (add_scopes param_scopes op.input); output = expand ctx (add_scopes param_scopes op.output) }) ops; public }],
     [[ scope ]])
  | TraitBinding { name; params; fields; public } ->
    let scope, name = bind_declaration ctx name in
    let params, param_scopes = expand_id_params ctx [] params in
    ([TraitBinding { name;
                     params; fields = List.map (fun (n, e) -> (n, expand ctx (add_scopes param_scopes e))) fields; public }],
     [[ scope ]])
  | ImplBinding { name; trait; args; fields; public } ->
    ([ImplBinding { name; trait = expand_path ctx trait; args = List.map (expand ctx) args;
                   fields = List.map (fun (n, e) -> (n, expand ctx e)) fields; public }],
     [[]])
  | PatternSynBinding { name; params; rhs; public } ->
    (* The right-hand side's heads resolve here, before the synonym's own name
       is bound; its parameters are pattern variables and stay as written. *)
    let rhs = expand_pat ctx rhs in
    let scope, name = bind_declaration ctx name in
    ([PatternSynBinding { name; params; rhs; public }], [[ scope ]])
  | HoleBinding id -> Expand_error.raise_at (UnfilledHole { hole = id.name })
  | SyntaxBinding { name; role; public } ->
    (* The role is bound for the items read after it, whose scope they carry. *)
    let scope = Expand_ctx.extend_role ctx ~role ~name in
    if public then ctx.Expand_ctx.syntax_exports <- ctx.Expand_ctx.syntax_exports @ [ (name.name, role) ];
    ([], [ [ scope ] ])
  | Items _ -> assert false
  | InstantiateBinding inst ->
    instantiate ctx inst (fun app captures -> function
      | ReplaceDecls ds ->
        let filled = splice_decl_holes captures (List.map (go_struct_binding (fill captures)) ds) in
        expand_struct_bindings_with_scopes ~in_struct ctx (List.map app.emit_binding filled)
      | ReplaceExpr _ -> Expand_error.raise_at (NotDeclarations { macro = inst.form.name }))
  | FieldBinding _ when not in_struct -> Enforest_util.error "a field [name : type] belongs in a struct"
  | FieldBinding { name; type_ } -> ([ FieldBinding { name; type_ = expand ctx type_ } ], [ [] ])
  | OpenBinding (m, _) ->
    (* An open binds no name of its own. Its scope marks the later bindings as
       inside it, so a name there can resolve to an open choice. *)
    let m' = expand ctx m in
    let open_scope, label = Expand_ctx.enter_open ctx ~occurrence:(member_scope m) m in
    import_roles ctx ~base_scope:(member_scope m) ~scope:open_scope m';
    ([OpenBinding (m', label)], [ open_scope :: open_unit_macro_scopes ctx m ])
   | MacroBinding { name; value; public; kind; output } ->
    begin match ctx.Expand_ctx.elaborate with
    | Some elab ->
      let resolved_kind = Syntax.macro_kind ~output kind value in
      let signature = Syntax.macro_signature ~output value in
      let params, value = Syntax.macro_params value in
      (* Stage 7: introduce name scope and register provisional macro BEFORE
         expansion/elaboration so the macro's own name is known during its
         definition (for future re-expansion-recursion support). The
         provisional marker prevents premature callable lookup. *)
      let scope, name = bind_declaration ~kind:Binding.Macro ctx name in
      let binding_name = id_name name in
      let macro_snapshot = Expand_ctx.snapshot_macro ctx binding_name in
      Expand_ctx.register_provisional_macro ctx ~name:binding_name ();
      Expand_ctx.register_macro_kind ctx ~name:binding_name ~kind:resolved_kind ~params;
      Fun.protect
        ~finally:(fun () ->
          if Expand_ctx.is_provisional_macro ctx binding_name then
            Expand_ctx.restore_macro_snapshot ctx ~name:binding_name macro_snapshot)
        (fun () ->
          let value = Expand_ctx.in_macro_definition ctx (fun () -> expand ctx value) in
          let macro_fn = elab (in_definition_site_opens ctx name value) in
          let signature = Option.map (compile_signature ctx elab name) signature in
          Expand_ctx.fill_provisional_macro ?signature ctx ~name:binding_name ~value:macro_fn;
          ([MacroBinding { name; value; public; kind; output }], [[ scope ]]))
    | None ->
      ([MacroBinding { name; value = expand ctx value; public; kind; output }], [[]])
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
          check_macro_kind ~key ~macro_kind ~ctx_kind:Syntax.MacroKind.Decl;
          check_argument_count ctx ~key args;
          let apply_fn = apply_fn ctx.Expand_ctx.budget in
           Expand_ctx.macro_application ctx ~name:key ~expand:(expand ctx) (fun () ->
             let app = application ctx in
             let fn = List.fold_left (fun fn arg ->
                apply_fn fn (Macro_eval.wrap_capture ~nominals:macro_nominals (app.receive_capture arg))) macro_fn args in
             (* Every argument is given (M8). What is still a function is an
                output left polymorphic - [{ Nil }] is [{A} -> List(A)], since
                a Decl macro's body has no declared output type to insert its
                implicit against - so it is instantiated, with a type, never
                with syntax the macro was not given. *)
             let rec instantiate v = match v with Core.VLam _ -> instantiate (apply_fn v Core.VU) | _ -> v in
             let result = Macro_eval.unwrap_stx_decl ?nominals:macro_nominals (instantiate fn) in
             (match result with
             | Some bindings ->
                 (* Stage 6: recursively process generated bindings through
                    the shared binding-list loop so generated MacroBinding
                    annotations are resolved, macros are compiled/registered,
                    and sibling-generated scopes thread in source order. *)
                 expand_struct_bindings_with_scopes ~in_struct ctx (List.map app.emit_binding bindings)
             | None -> Expand_error.raise_at (NotDeclarations { macro = key })))
        | None -> Expand_error.raise_at (MissingCallback { callback = "eval_and_apply" })
        end
      | Some (key, None, true) ->
        Expand_error.raise_at (ExpandedDuringDefinition { macro = key })
      | Some (_, None, false) | None -> ([MacroCallBinding { f = expand ctx f; args = List.map (expand_capture (expand ctx)) args }], [[]])
      end
    | _ -> ([MacroCallBinding { f = expand ctx f; args = List.map (expand_capture (expand ctx)) args }], [[]])
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
      let scope, _resolved_name = Expand_ctx.extend_at_fresh ctx ~span:id.span ~name:id.name ~base_scope:id.scope () in
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
