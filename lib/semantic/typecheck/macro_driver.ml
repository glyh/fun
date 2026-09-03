(** Stages 3–8: semantic driver for macro interleaving.

    Runs the expander over a module's top-level bindings and collects
    compiled macro exports. Stage 4 adds semantic kind pre-resolution
    and Lam-stripping for constraint annotations. Stage 5 replaces the
    global lock table with an injected [resolve_macro_kind] callback on
    [Expand_ctx.t]. Stage 7 adds scoped per-binding semantic advancement
    so that top-level source-order prior type/record declarations affect
    later macro annotation resolution. Stage 8 adds driver-based import
    loading: [visit_macros] compiles an imported module's public macros
    through a full driver run (with semantic kind resolution against the
    imported module's own advancing context), replacing the old
    [Core_loader.visit_macros] path. *)

open Core

type macro_export = {
  name : string;
  kind : Syntax.MacroKind.t;
  compiled : Core.value;
  syntax_nominals : Macro_eval.syntax_nominals option;
  public : bool;
}

type driver_output = {
  surface : Surface.t;
  expand_ctx : Expand_ctx.t;
  elab_ctx : Elab_ctx.Ctx.t;
  macro_exports : macro_export list;
}

let rec run ?loader (stx : Syntax.t) : driver_output =
  let bindings =
    match stx.kind with
    | Syntax.Module { bindings } -> bindings
    | _ -> invalid_arg "Macro_driver.run: expected Syntax.Module"
  in
  (* Initialise elaboration context (built-in types, stdlib). *)
  let elab_ctx0 = Elaborate.init_ctx () in
  (* Open stdlib so the per-binding advancement context matches module
     elaboration semantics (stdlib types are available during resolution).
     [init_ctx] defines stdlib as a module but does not open it. *)
  let elab_ctx0 = Elaborate.open_stdlib elab_ctx0 in
  let elab_ctx = ref elab_ctx0 in
  let syntax_nominals = Elaborate.syntax_nominals !elab_ctx in
  (* Build expand context with the same callbacks used by [eval_decl_module]. *)
  let expand_ctx = Expand_ctx.create () in
  Expand_ctx.set_syntax_nominals expand_ctx syntax_nominals;
  Expand_ctx.set_context_kind expand_ctx Syntax.MacroKind.Decl;
  (* Use [Elab_driver.infer] directly instead of [Elaborate.on_expr] because
     [!elab_ctx] already has stdlib opened. [on_expr] would open stdlib again,
     causing a double-open with wrong de Bruijn indices for macro body
     compilation. *)
  expand_ctx.Expand_ctx.elaborate
    <- Some (fun expr ->
         let core, _ty = Elab_driver.infer !elab_ctx expr in
         Elaborate.Ctx.eval !elab_ctx core);
  expand_ctx.Expand_ctx.eval_and_apply
    <- Some (fun fn arg ->
         let mc = MetaContext.create () in
         Nbe.apply mc fn arg);
  (* Stage 5 / Stage 7: inject semantic macro kind resolver callback.
     The callback reads the current (!elab_ctx) so that prior bindings
     advanced by the [after_binding] hook are visible for resolution. *)
  expand_ctx.Expand_ctx.resolve_macro_kind <- Some (fun ann ->
    Macro_resolver.resolve_kind !elab_ctx ann);
  (* Stage 8: driver-based import loading. Imported macros are compiled
     through [visit_macros] (a nested driver run); the elaboration context
     carries the loader so imports elaborated by the [after_binding] hook
     extend the semantic namespace for later annotation resolution. *)
  (match loader with
   | Some loader ->
       expand_ctx.Expand_ctx.load_macros <- Some (visit_macros loader);
       elab_ctx := Elab_ctx.Ctx.with_loader !elab_ctx loader
   | None -> ());
  (* Stage 7: per-binding semantic advancement hook.
     After each source binding is expanded, lower and elaborate every
     non-macro expanded binding to advance the elaborator context.
     MacroBinding and MacroCallBinding are skipped — they are handled
     by the expander and do not contribute to the semantic namespace.
     The hook is intentionally NOT passed to recursive calls inside
     expand_struct_binding (e.g. Decl macro expansion), so generated
     type→generated-macro interleaving within the same Decl output is
     deferred. *)
  let after_binding expanded =
    List.iter (fun (b : Syntax.struct_binding) ->
      match b with
      | Syntax.MacroBinding _ | Syntax.MacroCallBinding _ -> ()
      | _ ->
          let lowered = Lower_surface.lower_struct_binding b in
          let ctx', _, _ = Elab_infer.elab_module_binding Elab_driver.ops !elab_ctx lowered in
          elab_ctx := ctx')
      expanded
  in
  (* Expand all top-level bindings with scoped per-binding advancement.
     Macro bindings are retained in the expanded list for surface output;
     the original [expand_struct_bindings] filtering is applied below. *)
  let expanded_bindings, _scopes =
    Expand.expand_struct_bindings_with_scopes ~after_binding expand_ctx bindings
  in
  (* Filter out MacroBinding nodes from the surface (matching the behaviour
     of [Expand.expand_struct_bindings]). MacroCallBinding nodes are kept. *)
  let surface_bindings =
    List.filter (function Syntax.MacroBinding _ -> false | _ -> true) expanded_bindings
  in
  (* Rebuild lowered surface, preserving the original [stx] span. *)
  let surface =
    Lower_surface.lower_expr
      { stx with kind = Syntax.Module { bindings = surface_bindings } }
  in
  (* Collect compiled macro exports from the expand context's macro table.
     Publicness is determined by this module's own [MacroBinding] nodes;
     macros registered by imports are never re-exported. *)
  let public_macro_names =
    List.filter_map
      (function
        | Syntax.MacroBinding { name; public = true; _ } -> Some name.Syntax.name
        | _ -> None)
      expanded_bindings
  in
  let macro_exports =
    Hashtbl.fold
      (fun name entry acc ->
        let kind =
          match Hashtbl.find_opt expand_ctx.Expand_ctx.macro_kind_table name with
          | Some k -> k
          | None -> Syntax.MacroKind.default
        in
        { name; kind; compiled = entry.Expand_ctx.value;
          syntax_nominals = entry.Expand_ctx.syntax_nominals;
          public = List.mem name public_macro_names }
        :: acc)
      expand_ctx.Expand_ctx.macro_table []
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  (* Copy compiled macros into the elaboration context so it can resolve
     macro calls during later elaboration (matching [eval_decl_module]). *)
  Hashtbl.iter
    (fun name entry ->
      let kind =
        match Hashtbl.find_opt expand_ctx.Expand_ctx.macro_kind_table name with
        | Some k -> k
        | None -> Syntax.MacroKind.default
      in
      Hashtbl.replace !elab_ctx.Elab_ctx.Ctx.macro_table name
        (entry.Expand_ctx.value, kind, entry.Expand_ctx.syntax_nominals))
    expand_ctx.Expand_ctx.macro_table;
  !elab_ctx.Elab_ctx.Ctx.expand_ctx <- Some expand_ctx;
  { surface; expand_ctx; elab_ctx = !elab_ctx; macro_exports }

(** Stage 8: driver-based import loading. Compiles the public macros of
    the module at [path] through a full driver run — so their annotations
    are resolved semantically against that module's own prior type
    namespace — then registers them into [ctx]. Results are cached in the
    loader's [macro_cache]; circular macro visits are rejected. *)
and visit_macros (loader : Core_loader.t) (ctx : Expand_ctx.t) (path : string) : unit =
  (* [import "std"] is the reserved builtin prelude, not a file. Its public
     syntax ([if]/[&&]/[||] and the operators) is delivered through the loader's
     injected [builtin_syntax], so there is nothing to harvest from a "std" file
     here. *)
  if String.equal path Compiler_names.Module_name.std_import_path then () else
  let resolved = Core_loader.resolved_path loader path in
  if not (Sys.file_exists resolved) then raise (Core_loader.ImportNotFound path);
  let register_cached macros =
    List.iter
      (fun (name, value, kind, syntax_nominals) ->
        Expand_ctx.register_macro_with_nominals ctx ~syntax_nominals ~name ~value;
        Expand_ctx.register_macro_kind ctx ~name ~kind)
      macros
  in
  match Hashtbl.find_opt loader.Core_loader.macro_cache resolved with
  | Some macros -> register_cached macros
  | None ->
      if Hashtbl.mem loader.Core_loader.macro_active resolved then
        raise (Core_loader.CircularMacroVisit path);
      Hashtbl.replace loader.Core_loader.macro_active resolved path;
      let macros =
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove loader.Core_loader.macro_active resolved)
          (fun () ->
            let source = Core_loader.read_module_source resolved in
            let stx =
              Enforest.parse_module ~file:resolved
                ~load_syntax:(Core_loader.load_syntax_exports loader)
                source
            in
            let output = run ~loader stx in
            List.filter_map
              (fun (e : macro_export) ->
                if e.public then Some (e.name, e.compiled, e.kind, e.syntax_nominals)
                else None)
              output.macro_exports)
      in
      Hashtbl.replace loader.Core_loader.macro_cache resolved macros;
      register_cached macros
