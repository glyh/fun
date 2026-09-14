type macro_entry = {
  value : Core.value;
  syntax_nominals : Macro_eval.syntax_nominals option;
}

type macro_snapshot = {
  entry : macro_entry option;
  kind : Syntax.MacroKind.t option;
  provisional : bool;
}

type t = {
  binding_table : Binding.t;
  mutable scope_counter : int;
  (* While a macro's own definition is being expanded, the first scope minted
     for it: quoted syntax is pruned of every scope from here on (see
     [in_macro_definition]). *)
  mutable macro_definition_floor : int option;
  (* Every open expansion has entered: the scope it adds to its region, and its
     label. An id carrying the scope is inside that open. *)
  mutable opens : (int * string) list;
  mutable name_counter : int;
  mutable macro_table : (string, macro_entry) Hashtbl.t;
  mutable macro_kind_table : (string, Syntax.MacroKind.t) Hashtbl.t;
  mutable provisional_macros : (string, unit) Hashtbl.t;
  mutable expansion_position : Syntax.MacroKind.t;
  mutable elaborate : (Syntax.t -> Core.value) option;
  (* Applies a macro value to an argument, spending from the budget it is handed. *)
  mutable eval_and_apply : (Eval_budget.t -> Core.value -> Core.value -> Core.value) option;
  mutable load_macros : (t -> string -> unit) option;
  mutable syntax_nominals : Macro_eval.syntax_nominals option;
  (* The evaluation budget macro applications count against (M5). Shared, not
     copied, by [copy]. *)
  budget : Eval_budget.t;
  (* Macros are MEMBERS of a compilation unit, not names a bare [import]
     injects. [unit_macros] records, per unit path, the macro names that unit
     exports; each is registered in [macro_table] under [unit_macro_key], a key
     no source name can collide with - so two units exporting the same macro
     name no longer overwrite each other. [module_units] maps the resolved name
     of a binding like [M = import "m"] back to that unit, which is what lets
     [M.answer(0)] find the macro.
     See docs/wayfinder/tickets/imported-module-elaboration-context.md. *)
  mutable unit_macros : (string, string list) Hashtbl.t;
  mutable module_units : (string, string) Hashtbl.t;
  (* [unit_members] is, per unit path, that unit's public members which are
     themselves units - so [M.I.answer(0)] can find [answer] two dots down.
     [own_unit_members] is the same list for the unit THIS expander is currently
     expanding, harvested when its expansion finishes. *)
  mutable unit_members : (string, (string * string) list) Hashtbl.t;
  mutable own_unit_members : (string * string) list;
  (* Every intro scope a macro application minted here (template instances
     record theirs in [Syntax_template.template_intro_scopes]). *)
  intro_scopes : (int, unit) Hashtbl.t;
  (* Per open label, the names of the roles visible in that open's region -
     where it is written, or declared inside it. What an open supplies is known
     only to the elaborator, which rejects a member of one of these names (M7). *)
  open_roles : (string, string list) Hashtbl.t;
  loader : unit option;
}

let create ?loader () =
  { binding_table = Binding.create ();
    scope_counter = 0;
    macro_definition_floor = None;
    opens = [];
    name_counter = 0;
    macro_table = Hashtbl.create 8;
    macro_kind_table = Hashtbl.create 8;
    provisional_macros = Hashtbl.create 4;
    expansion_position = Syntax.MacroKind.Expr;
    elaborate = None;
    eval_and_apply = None;
    load_macros = None;
    syntax_nominals = None;
    budget = Eval_budget.create ();
    unit_macros = Hashtbl.create 4;
    module_units = Hashtbl.create 4;
    unit_members = Hashtbl.create 4;
    own_unit_members = [];
    intro_scopes = Hashtbl.create 16;
    open_roles = Hashtbl.create 8;
    loader }

let set_syntax_nominals ctx nominals = ctx.syntax_nominals <- Some nominals
let get_syntax_nominals ctx = ctx.syntax_nominals

let fresh_scope (ctx : t) : int =
  let s = ctx.scope_counter in
  ctx.scope_counter <- s + 1;
  s

let fresh_scope_set (ctx : t) : Scope_set.t =
  Scope_set.singleton (fresh_scope ctx)

(* Expand a macro's definition. Quoted syntax in it keeps the scopes of where
   the macro was defined, but not those of the binding forms inside the macro
   itself - its parameters and local lets - which do not exist where its output
   lands (Flatt 2016, quote-syntax pruning). *)
let in_macro_definition (ctx : t) f =
  let saved = ctx.macro_definition_floor in
  ctx.macro_definition_floor <- Some ctx.scope_counter;
  Fun.protect ~finally:(fun () -> ctx.macro_definition_floor <- saved) f

(* The units opened around a definition whose binder carries [scope],
   outermost first (M3). A macro body is compiled during expansion, before its
   surroundings are elaborated. Of its definition site, only these opens exist
   yet - an import can be loaded, a local cannot be evaluated - so they are
   what its body is elaborated inside. *)
let enclosing_unit_opens (ctx : t) (scope : Scope_set.t) : string list =
  ctx.opens
  |> List.filter (fun (s, label) ->
         Scope_set.subset (Scope_set.singleton s) scope
         && String.starts_with ~prefix:(Compiler_names.Module_name.unit_open_label "") label)
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map (fun (_, label) ->
         let prefix = String.length (Compiler_names.Module_name.unit_open_label "") in
         String.sub label prefix (String.length label - prefix))
  |> List.fold_left (fun acc p -> if List.mem p acc then acc else acc @ [ p ]) []

let note_open_role (ctx : t) label name =
  let names = Option.value ~default:[] (Hashtbl.find_opt ctx.open_roles label) in
  if not (List.mem name names) then Hashtbl.replace ctx.open_roles label (name :: names)

let roles_in_open (ctx : t) label = Option.value ~default:[] (Hashtbl.find_opt ctx.open_roles label)

(* Enter an open of [m], written with scope set [occurrence]: a fresh scope for
   its region, and its label. The roles visible where it is written are noted
   against it - except an imported unit's own, opened with it. *)
let enter_open (ctx : t) ?(occurrence = Scope_set.empty) (m : Syntax.t) : Scope_set.t * string =
  let scope = fresh_scope ctx in
  let label =
    match m.kind with
    | Syntax.Import path -> Compiler_names.Module_name.unit_open_label path
    | _ -> "open:" ^ string_of_int scope
  in
  Hashtbl.iter
    (fun name infos ->
      if List.exists
           (fun (info : Binding.binding_info) ->
             info.kind <> Binding.Value && Scope_set.subset info.scope occurrence
             && not (String.equal info.resolved_name label))
           infos
      then note_open_role ctx label name)
    ctx.binding_table;
  ctx.opens <- (scope, label) :: ctx.opens;
  (Scope_set.singleton scope, label)

(* The opens that may supply [id], innermost first (M: open choice). An open
   counts when [id] is inside it and [binder], if any, is not - a binder inside
   the open shadows it. An id no binder took, introduced by a template imported
   from a unit, may also mean that unit's names. *)
let open_candidates (ctx : t) (id : Syntax.id) (binder : Binding.binding_info option) : string list =
  let inside s scope = Scope_set.subset (Scope_set.singleton s) scope in
  let region =
    ctx.opens
    |> List.filter (fun (s, _) ->
           inside s id.scope && match binder with Some b -> not (inside s b.Binding.scope) | None -> true)
    |> List.sort (fun (a, _) (b, _) -> compare b a)
    |> List.map snd
  in
  let units =
    match binder with
    | Some _ -> []
    | None ->
        List.filter_map
          (fun s -> Option.map Compiler_names.Module_name.unit_open_label (Hashtbl.find_opt Syntax_template.intro_scope_units s))
          id.scope
  in
  List.fold_left (fun acc l -> if List.mem l acc then acc else acc @ [ l ]) [] (region @ units)

let prune_to_definition_site (ctx : t) (scope : Scope_set.t) =
  match ctx.macro_definition_floor with
  | Some floor -> Scope_set.filter (fun s -> s < floor) scope
  | None -> scope

(* A local binder's resolved name is always fresh, so it is unique among every
   name the elaborator can see - not only among binders this table knows. Names
   that reach the elaborator through [open], the prelude's included, are not in
   the table; reusing a first binder's spelling made a local [False] and the
   prelude's [False] the same string (template-literals-resolve-at-use-site). *)
let fresh_resolved_name (ctx : t) name =
  let i = ctx.name_counter in
  ctx.name_counter <- i + 1;
  Printf.sprintf "%s__%d" name i

let is_intro_scope (ctx : t) s =
  Hashtbl.mem ctx.intro_scopes s || Hashtbl.mem Syntax_template.template_intro_scopes s

(* M7: a syntactic role - a syntax form, operator or macro - never mixes with
   another binder of its name where both are visible. A new binder written
   with scope set [occurrence] conflicts with an existing binder of the other
   sort whose scope set is a subset of it, unless the scopes the new binder has
   beyond it include an intro scope: an application wrote the binder, and
   hygiene keeps the two apart. A fixity-only declaration [attaches] to the
   value visible where it is written. Checked in the funnel every binder goes
   through, so every binder kind - and its order against the role - is covered. *)
let check_role_mixing (ctx : t) ~name ~occurrence ~kind ~attaches ~span =
  let is_role (k : Binding.binding_kind) = k <> Binding.Value in
  let conflicts (info : Binding.binding_info) =
    is_role info.kind <> is_role kind
    && Scope_set.subset info.scope occurrence
    && not (List.exists (is_intro_scope ctx) (Scope_set.diff occurrence info.scope))
    && not (attaches && info.kind = Binding.Value)
  in
  if List.exists conflicts (Option.value ~default:[] (Hashtbl.find_opt ctx.binding_table name)) then
    Expand_error.raise_at (RoleConflict { name; span })

let bind (ctx : t) ?(attaches = false) ?(span = Source_span.synthetic) ~name ~base_scope ~kind ~resolved_name scope =
  check_role_mixing ctx ~name ~occurrence:base_scope ~kind ~attaches ~span;
  if kind <> Binding.Value then
    List.iter (fun (o, label) -> if Scope_set.contains base_scope o then note_open_role ctx label name) ctx.opens;
  Binding.extend ctx.binding_table ~name ~scope:(Scope_set.union base_scope scope) ~kind ~resolved_name

let extend_at (ctx : t) ?span ~name ~base_scope ~resolved_name () =
  let scope = fresh_scope_set ctx in
  bind ctx ?span ~name ~base_scope ~kind:Binding.Value ~resolved_name scope;
  scope

(* A syntax template or fixity declaration, as a binder (see [Syntax.SyntaxBinding]). *)
let extend_role (ctx : t) ~attaches ~(name : Syntax.id) =
  let scope = fresh_scope_set ctx in
  bind ctx ~attaches ~span:name.span ~name:name.name ~base_scope:name.scope ~kind:Binding.Role
    ~resolved_name:name.name scope;
  scope

(* Roles an [import] harvested into the enforester, with the unit each came
   from: visible to the whole unit. *)
let add_imported_roles (ctx : t) (roles : (string * string option) list) =
  List.iter
    (fun (name, unit) ->
      let resolved_name = match unit with Some path -> Compiler_names.Module_name.unit_open_label path | None -> name in
      Binding.extend ctx.binding_table ~name ~scope:Scope_set.empty ~kind:Binding.Role ~resolved_name)
    roles

(** Like [extend_at] but tags the binding with an explicit [kind], so a
    procedural-macro definition can register itself as a [Macro] binding in the
    scope-aware table (name resolution then dispatches expand-vs-call by kind). *)
let extend_at_kinded (ctx : t) ?span ~name ~base_scope ~kind ~resolved_name () =
  let scope = fresh_scope_set ctx in
  bind ctx ?span ~name ~base_scope ~kind ~resolved_name scope;
  scope

(** Like [extend_at] but allocates a fresh [resolved_name] (uniquified when
    the written name is already bound) and tags an explicit [kind], so nested
    definitions shadow lexically. Used for expression-level macro definitions. *)
let extend_at_fresh_kinded (ctx : t) ?span ~name ~base_scope ?(kind = Binding.Value) () =
  let resolved_name = fresh_resolved_name ctx name in
  let scope = fresh_scope_set ctx in
  bind ctx ?span ~name ~base_scope ~kind ~resolved_name scope;
  (scope, resolved_name)

let extend_at_fresh (ctx : t) ?span ~name ~base_scope () =
  extend_at_fresh_kinded ctx ?span ~name ~base_scope ()

let copy (ctx : t) : t =
  { binding_table = Binding.copy ctx.binding_table;
    scope_counter = ctx.scope_counter;
    macro_definition_floor = ctx.macro_definition_floor;
    opens = ctx.opens;
    name_counter = ctx.name_counter;
    macro_table = Hashtbl.copy ctx.macro_table;
    macro_kind_table = Hashtbl.copy ctx.macro_kind_table;
    provisional_macros = Hashtbl.copy ctx.provisional_macros;
    expansion_position = ctx.expansion_position;
    elaborate = ctx.elaborate;
    eval_and_apply = ctx.eval_and_apply;
    load_macros = ctx.load_macros;
    syntax_nominals = ctx.syntax_nominals;
    budget = ctx.budget;
    unit_macros = Hashtbl.copy ctx.unit_macros;
    module_units = Hashtbl.copy ctx.module_units;
    unit_members = Hashtbl.copy ctx.unit_members;
    own_unit_members = ctx.own_unit_members;
    intro_scopes = Hashtbl.copy ctx.intro_scopes;
    open_roles = Hashtbl.copy ctx.open_roles;
    loader = ctx.loader }

let register_macro_with_nominals ctx ~syntax_nominals ~name ~value =
  Hashtbl.replace ctx.macro_table name { value; syntax_nominals }

let register_macro ctx ~name ~value =
  register_macro_with_nominals ctx ~syntax_nominals:ctx.syntax_nominals ~name ~value

let register_macro_kind (ctx : t) ~name ~kind =
  Hashtbl.replace ctx.macro_kind_table name kind

(* The [macro_table] key under which unit [path]'s macro [name] is filed. The
   separator cannot occur in a source identifier, so a unit's macro is reachable
   only through the paths that deliberately look it up: a dotted call on a bound
   import, or a name an [open] introduced. *)
let unit_macro_key ~path ~name = path ^ "\x00" ^ name

(* Record that unit [path] exports macro [name]. *)
let note_unit_macro ctx ~path ~name =
  let names = Option.value ~default:[] (Hashtbl.find_opt ctx.unit_macros path) in
  if not (List.mem name names) then
    Hashtbl.replace ctx.unit_macros path (name :: names)

let register_unit_macro ctx ~path ~name ~value ~kind ~syntax_nominals =
  let key = unit_macro_key ~path ~name in
  register_macro_with_nominals ctx ~syntax_nominals ~name:key ~value;
  register_macro_kind ctx ~name:key ~kind;
  note_unit_macro ctx ~path ~name

let unit_macro_names ctx path =
  Option.value ~default:[] (Hashtbl.find_opt ctx.unit_macros path)

(* [M = import "m"] makes [M] a handle on a unit, so [M.answer(0)] can expand. *)
let bind_module_unit ctx ~resolved_name ~path =
  Hashtbl.replace ctx.module_units resolved_name path

let module_unit ctx resolved_name = Hashtbl.find_opt ctx.module_units resolved_name

(* [pub I = import "inner"] makes [I] a unit-valued member of the unit being
   expanded, so an importer can reach through it. *)
let record_own_unit_member ctx ~name ~path =
  ctx.own_unit_members <- (name, path) :: ctx.own_unit_members

let unit_member ctx ~path ~name =
  match Hashtbl.find_opt ctx.unit_members path with
  | Some members -> List.assoc_opt name members
  | None -> None

(* Carry what a unit's own expander learned into the expander that imports it:
   the macros of every unit it pulled in, and every unit-valued member of every
   unit it knows about. Without this the knowledge stops at the file boundary
   and a macro two dots away is invisible. *)
let absorb_units ~(from : t) (ctx : t) =
  Hashtbl.iter
    (fun path names ->
      List.iter
        (fun name ->
          let key = unit_macro_key ~path ~name in
          match Hashtbl.find_opt from.macro_table key with
          | Some entry ->
              Hashtbl.replace ctx.macro_table key entry;
              (match Hashtbl.find_opt from.macro_kind_table key with
               | Some kind -> Hashtbl.replace ctx.macro_kind_table key kind
               | None -> ());
              note_unit_macro ctx ~path ~name
          | None -> ())
        names)
    from.unit_macros;
  Hashtbl.iter (fun path members -> Hashtbl.replace ctx.unit_members path members) from.unit_members

let lookup_macro (ctx : t) name =
  Option.map (fun entry -> entry.value) (Hashtbl.find_opt ctx.macro_table name)

let lookup_macro_entry (ctx : t) name =
  Hashtbl.find_opt ctx.macro_table name

let lookup_macro_kind (ctx : t) name =
  Hashtbl.find_opt ctx.macro_kind_table name

let is_provisional_macro ctx name =
  Hashtbl.mem ctx.provisional_macros name

let snapshot_macro ctx name =
  { entry = Hashtbl.find_opt ctx.macro_table name;
    kind = Hashtbl.find_opt ctx.macro_kind_table name;
    provisional = is_provisional_macro ctx name }

let restore_macro_snapshot ctx ~name snapshot =
  (match snapshot.entry with
   | Some entry -> Hashtbl.replace ctx.macro_table name entry
   | None -> Hashtbl.remove ctx.macro_table name);
  (match snapshot.kind with
   | Some kind -> Hashtbl.replace ctx.macro_kind_table name kind
   | None -> Hashtbl.remove ctx.macro_kind_table name);
  if snapshot.provisional then Hashtbl.replace ctx.provisional_macros name ()
  else Hashtbl.remove ctx.provisional_macros name

let register_provisional_macro ctx ~name () =
  Hashtbl.replace ctx.provisional_macros name ()

let fill_provisional_macro ctx ~name ~value =
  Hashtbl.remove ctx.provisional_macros name;
  register_macro ctx ~name ~value

(* Run one macro application, and the expansion of its output, as a call under
   the evaluation budget. An overrun or evaluation failure inside it is this
   application's error, at [site] when it came from a syntax operator. *)
let macro_application ?site ctx ~name f =
  let error e = Expand_error.Error { error = e; site } in
  let application = Eval_budget.{
    exceeded = (fun ~limit ~call -> error (BudgetExceeded { macro = name; limit; call }));
    failed = (fun message -> error (EvalFailed { macro = name; message })) } in
  Eval_budget.macro_application ctx.budget ~call:(Printf.sprintf "macro '%s'" name) ~application f

let set_expansion_position (ctx : t) kind =
  ctx.expansion_position <- kind

let get_expansion_position (ctx : t) =
  ctx.expansion_position

let resolve (ctx : t) (id : Syntax.id) : Binding.binding_info option =
  Binding.resolve ctx.binding_table id
