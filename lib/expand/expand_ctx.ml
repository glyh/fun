type phase = Runtime | CompileTime

type t = {
  binding_table : Binding.t;
  phase : phase;
  mutable scope_counter : int;
  loader : unit option;
}

let create ?loader () =
  { binding_table = Binding.create ();
    phase = Runtime;
    scope_counter = 0;
    loader }

let fresh_scope (ctx : t) : int =
  let s = ctx.scope_counter in
  ctx.scope_counter <- s + 1;
  s

let fresh_scope_set (ctx : t) : Scope_set.t =
  Scope_set.singleton (fresh_scope ctx)

let extend (ctx : t) ~name ~resolved_name =
  let scope = fresh_scope_set ctx in
  Binding.extend ctx.binding_table ~name ~scope ~resolved_name;
  scope

let copy (ctx : t) : t =
  { binding_table = Binding.copy ctx.binding_table;
    phase = ctx.phase;
    scope_counter = ctx.scope_counter;
    loader = ctx.loader }

let with_binding (ctx : t) ~name ~resolved_name ~f =
  let scope = extend ctx ~name ~resolved_name in
  f scope ctx

let resolve (ctx : t) (id : Syntax.id) : Binding.binding_info option =
  Binding.resolve ctx.binding_table id
