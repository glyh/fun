type phase = Runtime | CompileTime

type t = {
  binding_table : Binding.t;
  phase : phase;
  mutable scope_counter : int;
  mutable name_counter : int;
  mutable macro_table : (string, Core.value) Hashtbl.t;
  mutable macro_has_problem : (string, unit) Hashtbl.t;
  mutable current_problem : Core.value option;
  mutable elaborate : (Surface.t -> Core.value) option;
  mutable eval_and_apply : (Core.value -> Core.value -> Core.value) option;
  mutable load_macros : (t -> string -> unit) option;
  mutable syntax_nominals : Macro_eval.syntax_nominals option;
  loader : unit option;
}

let create ?loader () =
  { binding_table = Binding.create ();
    phase = Runtime;
    scope_counter = 0;
    name_counter = 0;
    macro_table = Hashtbl.create 8;
    macro_has_problem = Hashtbl.create 8;
    current_problem = None;
    elaborate = None;
    eval_and_apply = None;
    load_macros = None;
    syntax_nominals = None;
    loader }

let set_syntax_nominals ctx nominals = ctx.syntax_nominals <- Some nominals
let get_syntax_nominals ctx = ctx.syntax_nominals

let fresh_scope (ctx : t) : int =
  let s = ctx.scope_counter in
  ctx.scope_counter <- s + 1;
  s

let fresh_scope_set (ctx : t) : Scope_set.t =
  Scope_set.singleton (fresh_scope ctx)

(** Suffix a fresh internal name (e.g. "x__0") when a binder shadows a
    previously seen written name.  This is only for debugging / lowering
    hygiene tests: the elaborator's own sequential [Ctx.define]/[lookup]
    already handles shadowing correctly regardless of the lowered string.
    Scope-set resolution remains the authoritative hygiene model. *)
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
    macro_has_problem = Hashtbl.copy ctx.macro_has_problem;
    current_problem = ctx.current_problem;
    elaborate = ctx.elaborate;
    eval_and_apply = ctx.eval_and_apply;
    load_macros = ctx.load_macros;
    syntax_nominals = ctx.syntax_nominals;
    loader = ctx.loader }

let register_macro (ctx : t) ~name ~value =
  Hashtbl.replace ctx.macro_table name value

let register_macro_with_problem (ctx : t) ~name ~value =
  Hashtbl.replace ctx.macro_table name value;
  Hashtbl.replace ctx.macro_has_problem name ()

let set_current_problem (ctx : t) problem =
  ctx.current_problem <- Some problem

let get_current_problem (ctx : t) =
  ctx.current_problem

let macro_has_problem (ctx : t) name =
  Hashtbl.mem ctx.macro_has_problem name

let get_current_problem_or_default (ctx : t) ~default =
  match ctx.current_problem with
  | Some p -> p
  | None -> default

let lookup_macro (ctx : t) name =
  Hashtbl.find_opt ctx.macro_table name

let resolve (ctx : t) (id : Syntax.id) : Binding.binding_info option =
  Binding.resolve ctx.binding_table id
