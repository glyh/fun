(** Stage 3: Additive driver skeleton for macro interleaving.

    This module runs the expander over a module's top-level bindings and
    collects compiled macro exports. It is NOT yet an incremental
    semantic driver — all bindings are expanded in one pass with the
    initial prelude context. Per-binding semantic advancement belongs
    to Stage 4.

    The output matches the current [Parse_expand.parse_module_with_ctx]
    pipeline exactly, so existing tests pass unchanged. *)

open Core

type macro_export = {
  name : string;
  kind : Syntax.MacroKind.t;
  compiled : Core.value;
  syntax_nominals : Macro_eval.syntax_nominals option;
}

type driver_output = {
  surface : Surface.t;
  expand_ctx : Expand_ctx.t;
  elab_ctx : Elab_ctx.Ctx.t;
  macro_exports : macro_export list;
}

let run (stx : Syntax.t) : driver_output =
  let bindings =
    match stx.kind with
    | Syntax.Module { bindings } -> bindings
    | _ -> invalid_arg "Macro_driver.run: expected Syntax.Module"
  in
  (* Initialise elaboration context (built-in types, stdlib). *)
  let elab_ctx = Elaborate.init_ctx () in
  let syntax_nominals = Elaborate.syntax_nominals elab_ctx in
  (* Build expand context with the same callbacks used by [eval_decl_module]. *)
  let expand_ctx = Expand_ctx.create () in
  Expand_ctx.set_syntax_nominals expand_ctx syntax_nominals;
  Expand_ctx.set_context_kind expand_ctx Syntax.MacroKind.Decl;
  expand_ctx.Expand_ctx.elaborate
    <- Some (fun expr ->
         let core, _ty = Elaborate.on_expr elab_ctx expr in
         Elaborate.Ctx.eval elab_ctx core);
  expand_ctx.Expand_ctx.eval_and_apply
    <- Some (fun fn arg ->
         let mc = MetaContext.create () in
         Nbe.apply mc fn arg);
  (* Expand all top-level bindings wholesale (do NOT duplicate the per-binding
     scope loop from expand.ml). *)
  let expanded_bindings =
    Expand.expand_struct_bindings expand_ctx bindings
  in
  (* Rebuild lowered surface, preserving the original [stx] span. *)
  let surface =
    Lower_surface.lower_expr
      { stx with kind = Syntax.Module { bindings = expanded_bindings } }
  in
  (* Collect compiled macro exports from the expand context's macro table. *)
  let macro_exports =
    Hashtbl.fold
      (fun name entry acc ->
        let kind =
          match Hashtbl.find_opt expand_ctx.Expand_ctx.macro_kind_table name with
          | Some k -> k
          | None -> Syntax.MacroKind.default
        in
        { name; kind; compiled = entry.Expand_ctx.value;
          syntax_nominals = entry.Expand_ctx.syntax_nominals }
        :: acc)
      expand_ctx.Expand_ctx.macro_table []
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  (* Copy compiled macros into the elaboration context so it can resolve
     macro calls during later elaboration (matching [eval_decl_module]).

     STAGE 3 NOTE: [elab_ctx] is NOT yet incrementally advanced per-binding.
     The semantic type namespace remains the initial prelude state. *)
  Hashtbl.iter
    (fun name entry ->
      let kind =
        match Hashtbl.find_opt expand_ctx.Expand_ctx.macro_kind_table name with
        | Some k -> k
        | None -> Syntax.MacroKind.default
      in
      Hashtbl.replace elab_ctx.Elab_ctx.Ctx.macro_table name
        (entry.Expand_ctx.value, kind, entry.Expand_ctx.syntax_nominals))
    expand_ctx.Expand_ctx.macro_table;
  elab_ctx.Elab_ctx.Ctx.expand_ctx <- Some expand_ctx;
  { surface; expand_ctx; elab_ctx; macro_exports }
