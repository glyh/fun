type macro_entry = {
  value : Core.value;
  syntax_nominals : Macro_eval.syntax_nominals option;
}

type macro_snapshot = {
  entry : macro_entry option;
  kind : Syntax.MacroKind.t option;
  provisional : bool;
}

type phase = Runtime | CompileTime

let default_macro_fuel_limit = 256

type t = {
  binding_table : Binding.t;
  phase : phase;
  mutable scope_counter : int;
  mutable name_counter : int;
  mutable macro_table : (string, macro_entry) Hashtbl.t;
  mutable macro_kind_table : (string, Syntax.MacroKind.t) Hashtbl.t;
  mutable provisional_macros : (string, unit) Hashtbl.t;
  mutable context_kind : Syntax.MacroKind.t;
  mutable resolve_macro_kind : (Syntax.MacroAnnotation.t -> Syntax.MacroKind.t * Syntax.param option) option;
  mutable elaborate : (Surface.t -> Core.value) option;
  mutable eval_and_apply : (Core.value -> Core.value -> Core.value) option;
  mutable load_macros : (t -> string -> unit) option;
  mutable syntax_nominals : Macro_eval.syntax_nominals option;
  mutable macro_fuel_limit : int;
  mutable macro_fuel : int ref;
  loader : unit option;
}

let create ?loader () =
  { binding_table = Binding.create ();
    phase = Runtime;
    scope_counter = 0;
    name_counter = 0;
    macro_table = Hashtbl.create 8;
    macro_kind_table = Hashtbl.create 8;
    provisional_macros = Hashtbl.create 4;
    context_kind = Syntax.MacroKind.(Expr (None, None));
    resolve_macro_kind = None;
    elaborate = None;
    eval_and_apply = None;
    load_macros = None;
    syntax_nominals = None;
    macro_fuel_limit = default_macro_fuel_limit;
    macro_fuel = ref default_macro_fuel_limit;
    loader }

let set_syntax_nominals ctx nominals = ctx.syntax_nominals <- Some nominals
let get_syntax_nominals ctx = ctx.syntax_nominals

let fresh_scope (ctx : t) : int =
  let s = ctx.scope_counter in
  ctx.scope_counter <- s + 1;
  s

let fresh_scope_set (ctx : t) : Scope_set.t =
  Scope_set.singleton (fresh_scope ctx)

let fresh_resolved_name (ctx : t) name =
  if Binding.has_name ctx.binding_table name then begin
    let i = ctx.name_counter in
    ctx.name_counter <- i + 1;
    Printf.sprintf "%s__%d" name i
  end else
    name

let extend (ctx : t) ~name ~resolved_name =
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope ~resolved_name;
  scope

let extend_fresh (ctx : t) ~name =
  let resolved_name = fresh_resolved_name ctx name in
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope ~resolved_name;
  (scope, resolved_name)

let extend_at (ctx : t) ~name ~base_scope ~resolved_name =
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope:(Scope_set.union base_scope scope) ~resolved_name;
  scope

let extend_at_fresh (ctx : t) ~name ~base_scope =
  let resolved_name = fresh_resolved_name ctx name in
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope:(Scope_set.union base_scope scope) ~resolved_name;
  (scope, resolved_name)

let copy (ctx : t) : t =
  { binding_table = Binding.copy ctx.binding_table;
    phase = ctx.phase;
    scope_counter = ctx.scope_counter;
    name_counter = ctx.name_counter;
    macro_table = Hashtbl.copy ctx.macro_table;
    macro_kind_table = Hashtbl.copy ctx.macro_kind_table;
    provisional_macros = Hashtbl.copy ctx.provisional_macros;
    context_kind = ctx.context_kind;
    resolve_macro_kind = ctx.resolve_macro_kind;
    elaborate = ctx.elaborate;
    eval_and_apply = ctx.eval_and_apply;
    load_macros = ctx.load_macros;
    syntax_nominals = ctx.syntax_nominals;
    macro_fuel_limit = ctx.macro_fuel_limit;
    macro_fuel = ctx.macro_fuel;
    loader = ctx.loader }

let register_macro_with_nominals ctx ~syntax_nominals ~name ~value =
  Hashtbl.replace ctx.macro_table name { value; syntax_nominals }

let register_macro ctx ~name ~value =
  register_macro_with_nominals ctx ~syntax_nominals:ctx.syntax_nominals ~name ~value

let register_macro_kind (ctx : t) ~name ~kind =
  Hashtbl.replace ctx.macro_kind_table name kind

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

let set_context_kind (ctx : t) kind =
  ctx.context_kind <- kind

let get_context_kind (ctx : t) =
  ctx.context_kind

let resolve (ctx : t) (id : Syntax.id) : Binding.binding_info option =
  Binding.resolve ctx.binding_table id
