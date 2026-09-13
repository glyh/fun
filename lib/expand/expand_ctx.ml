type macro_entry = {
  value : Core.value;
  syntax_nominals : Macro_eval.syntax_nominals option;
}

type macro_snapshot = {
  entry : macro_entry option;
  kind : Syntax.MacroKind.t option;
  provisional : bool;
}

let default_macro_fuel_limit = 256

type t = {
  binding_table : Binding.t;
  mutable scope_counter : int;
  (* While a macro's own definition is being expanded, the first scope minted
     for it: quoted syntax is pruned of every scope from here on (see
     [in_macro_definition]). *)
  mutable macro_definition_floor : int option;
  mutable name_counter : int;
  mutable macro_table : (string, macro_entry) Hashtbl.t;
  mutable macro_kind_table : (string, Syntax.MacroKind.t) Hashtbl.t;
  mutable provisional_macros : (string, unit) Hashtbl.t;
  mutable expansion_position : Syntax.MacroKind.t;
  mutable resolve_macro_kind : (Syntax.MacroAnnotation.t -> Syntax.MacroKind.t * Syntax.param option) option;
  mutable elaborate : (Surface.t -> Core.value) option;
  mutable eval_and_apply : (Core.value -> Core.value -> Core.value) option;
  mutable load_macros : (t -> string -> unit) option;
  mutable syntax_nominals : Macro_eval.syntax_nominals option;
  mutable macro_fuel_limit : int;
  mutable macro_fuel : int ref;
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
  loader : unit option;
}

let create ?loader () =
  { binding_table = Binding.create ();
    scope_counter = 0;
    macro_definition_floor = None;
    name_counter = 0;
    macro_table = Hashtbl.create 8;
    macro_kind_table = Hashtbl.create 8;
    provisional_macros = Hashtbl.create 4;
    expansion_position = Syntax.MacroKind.(Expr (None, None));
    resolve_macro_kind = None;
    elaborate = None;
    eval_and_apply = None;
    load_macros = None;
    syntax_nominals = None;
    macro_fuel_limit = default_macro_fuel_limit;
    macro_fuel = ref default_macro_fuel_limit;
    unit_macros = Hashtbl.create 4;
    module_units = Hashtbl.create 4;
    unit_members = Hashtbl.create 4;
    own_unit_members = [];
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

let prune_to_definition_site (ctx : t) (scope : Scope_set.t) =
  match ctx.macro_definition_floor with
  | Some floor -> Scope_set.filter (fun s -> s < floor) scope
  | None -> scope

let fresh_resolved_name (ctx : t) name =
  if Binding.has_name ctx.binding_table name then begin
    let i = ctx.name_counter in
    ctx.name_counter <- i + 1;
    Printf.sprintf "%s__%d" name i
  end else
    name

let extend (ctx : t) ~name ~resolved_name =
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope ~kind:Binding.Value ~resolved_name;
  scope

let extend_fresh (ctx : t) ~name =
  let resolved_name = fresh_resolved_name ctx name in
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope ~kind:Binding.Value ~resolved_name;
  (scope, resolved_name)

let extend_at (ctx : t) ~name ~base_scope ~resolved_name =
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope:(Scope_set.union base_scope scope) ~kind:Binding.Value ~resolved_name;
  scope

(** Like [extend_at] but tags the binding with an explicit [kind], so a
    procedural-macro definition can register itself as a [Macro] binding in the
    scope-aware table (name resolution then dispatches expand-vs-call by kind). *)
let extend_at_kinded (ctx : t) ~name ~base_scope ~kind ~resolved_name =
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope:(Scope_set.union base_scope scope) ~kind ~resolved_name;
  scope

(** Like [extend_at] but allocates a fresh [resolved_name] (uniquified when
    the written name is already bound) and tags an explicit [kind], so nested
    definitions shadow lexically. Used for expression-level macro definitions. *)
let extend_at_fresh_kinded (ctx : t) ~name ~base_scope ?(kind = Binding.Value) () =
  let resolved_name = fresh_resolved_name ctx name in
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope:(Scope_set.union base_scope scope) ~kind ~resolved_name;
  (scope, resolved_name)

let extend_at_fresh (ctx : t) ~name ~base_scope =
  extend_at_fresh_kinded ctx ~name ~base_scope ()

let copy (ctx : t) : t =
  { binding_table = Binding.copy ctx.binding_table;
    scope_counter = ctx.scope_counter;
    macro_definition_floor = ctx.macro_definition_floor;
    name_counter = ctx.name_counter;
    macro_table = Hashtbl.copy ctx.macro_table;
    macro_kind_table = Hashtbl.copy ctx.macro_kind_table;
    provisional_macros = Hashtbl.copy ctx.provisional_macros;
    expansion_position = ctx.expansion_position;
    resolve_macro_kind = ctx.resolve_macro_kind;
    elaborate = ctx.elaborate;
    eval_and_apply = ctx.eval_and_apply;
    load_macros = ctx.load_macros;
    syntax_nominals = ctx.syntax_nominals;
    macro_fuel_limit = ctx.macro_fuel_limit;
    macro_fuel = ctx.macro_fuel;
    unit_macros = Hashtbl.copy ctx.unit_macros;
    module_units = Hashtbl.copy ctx.module_units;
    unit_members = Hashtbl.copy ctx.unit_members;
    own_unit_members = ctx.own_unit_members;
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

let reserve_macro_fuel ctx ~name =
  if !(ctx.macro_fuel) <= 0 then
    failwith (Printf.sprintf "macro expansion exceeded fuel limit (%d) when expanding '%s'"
                ctx.macro_fuel_limit name);
  ctx.macro_fuel := !(ctx.macro_fuel) - 1

let release_macro_fuel ctx =
  ctx.macro_fuel := !(ctx.macro_fuel) + 1

let with_macro_fuel ctx ~name f =
  reserve_macro_fuel ctx ~name;
  Fun.protect ~finally:(fun () -> release_macro_fuel ctx) f

let set_expansion_position (ctx : t) kind =
  ctx.expansion_position <- kind

let get_expansion_position (ctx : t) =
  ctx.expansion_position

let resolve (ctx : t) (id : Syntax.id) : Binding.binding_info option =
  Binding.resolve ctx.binding_table id
