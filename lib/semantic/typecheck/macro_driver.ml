(** Stages 3–8: semantic driver for macro interleaving.

    Runs the expander over a module's top-level bindings and collects
    compiled macro exports. Each source binding is elaborated as soon as it
    is expanded, so a later macro body is compiled against the unit as of its
    definition. [visit_macros] compiles an imported module's public macros
    through a full driver run of that module. A macro's kind is syntactic
    ([Syntax.macro_kind]); nothing here decides it. *)


type macro_export = {
  name : string;
  kind : Syntax.MacroKind.t;
  params : Syntax.hole_kind list;
  entry : Expand_ctx.macro_entry;
  public : bool;
}

type driver_output = {
  expanded : Syntax.t;
  expand_ctx : Expand_ctx.t;
  elab_ctx : Elab_ctx.Ctx.t;
  macro_exports : macro_export list;
}

let rec run ?loader ?load_syntax (stx : Syntax.t) : driver_output =
  let bindings =
    match stx.kind with
    | Syntax.Module { bindings } -> bindings
    | _ -> invalid_arg "Macro_driver.run: expected Syntax.Module"
  in
  (* Initialise elaboration context (built-in types, stdlib). *)
  (* The unit's own context as it advances binding by binding: nothing is open
     in it but what the unit opens itself (M3). *)
  let elab_ctx = ref (Elaborate.init_ctx ()) in
  let syntax_nominals = Elaborate.syntax_nominals !elab_ctx in
  (* Build expand context with the same callbacks used by [eval_decl_module]. *)
  let expand_ctx = Expand_ctx.create () in
  Expand_ctx.set_syntax_nominals expand_ctx syntax_nominals;
  expand_ctx.Expand_ctx.load_syntax <- load_syntax;
  (* A macro body is compiled in the unit's context as of its definition. *)
  expand_ctx.Expand_ctx.elaborate
    <- Some (fun expr ->
         Elab_entry.reporting_budget (fun () ->
             let core, _ty = Elab_driver.infer !elab_ctx expr in
             Elaborate.Ctx.eval !elab_ctx core));
  expand_ctx.Expand_ctx.eval_and_apply
    <- Some Nbe.apply_macro;
  (* Installed before any binding elaborates, so every open the unit elaborates
     is checked against the roles visible in its region (M7). *)
  !elab_ctx.Elab_ctx.Ctx.macro_runtime <- Elab_ctx.Ctx.macro_runtime_of_expander expand_ctx;
  (* Stage 8: driver-based import loading. Imported macros are compiled
     through [visit_macros] (a nested driver run); the elaboration context
     carries the loader so imports elaborated by the [after_binding] hook
     extend the context later macro bodies are compiled in. *)
  (match loader with
   | Some loader ->
       expand_ctx.Expand_ctx.load_macros <- Some (visit_macros loader);
       elab_ctx := Elab_ctx.Ctx.with_loader !elab_ctx loader
   | None -> ());
  (* Stage 7: per-binding semantic advancement hook.
     After each source binding is expanded, elaborate every
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
          let ctx', _, _ =
            Elab_entry.reporting_budget (fun () -> Elab_infer.elab_module_binding Elab_driver.ops !elab_ctx b)
          in
          elab_ctx := ctx')
      expanded
  in
  (* Expand all top-level bindings with scoped per-binding advancement.
     Macro bindings are retained in the expanded list for the output;
     the original [expand_struct_bindings] filtering is applied below. *)
  let expanded_bindings, _scopes =
    Expand.expand_struct_bindings_with_scopes ~after_binding expand_ctx bindings
  in
  (* Filter out MacroBinding nodes from the output (matching the behaviour
     of [Expand.expand_struct_bindings]). MacroCallBinding nodes are kept. *)
  let kept_bindings =
    List.filter (function Syntax.MacroBinding _ -> false | _ -> true) expanded_bindings
  in
  let expanded = { stx with kind = Syntax.Module { bindings = kept_bindings } }
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
        { name; kind; entry;
          params = Option.value ~default:[] (Expand_ctx.lookup_macro_params expand_ctx name);
          public = List.mem name public_macro_names }
        :: acc)
      expand_ctx.Expand_ctx.macro_table []
    |> List.sort (fun a b -> String.compare a.name b.name)
  in
  { expanded; expand_ctx; elab_ctx = !elab_ctx; macro_exports }

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
      (fun (name, entry, kind, params) -> Expand_ctx.register_unit_macro ctx ~path ~name ~entry ~kind ~params)
      macros
  in
  (* Whatever the unit's own expander learned has to cross into this one, or a
     macro one unit further away stays invisible here. *)
  let absorb () =
    match Hashtbl.find_opt loader.Core_loader.driver_expanded_cache resolved with
    | Some (_, driver_ctx) -> Expand_ctx.absorb_units ~from:driver_ctx ctx
    | None -> ()
  in
  match Hashtbl.find_opt loader.Core_loader.macro_cache resolved with
  | Some macros -> register_cached macros; absorb ()
  | None ->
      if Hashtbl.mem loader.Core_loader.macro_active resolved then
        raise (Core_loader.CircularMacroVisit path);
      Hashtbl.replace loader.Core_loader.macro_active resolved path;
      let macros =
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove loader.Core_loader.macro_active resolved)
          (fun () ->
            let source = Core_loader.read_module_source resolved in
            let stx = Enforest.parse_module ~file:resolved source in
            let output = run ~loader ~load_syntax:(Core_loader.load_syntax_exports loader) stx in
            (* Keep the expanded unit, not just the exports. This is the one
               pass that expands the unit with macros live; without it the
               loader re-expands the file with no [elaborate] callback and every
               macro call inside it dies as an unbound variable. *)
            Hashtbl.replace loader.Core_loader.driver_expanded_cache resolved
              (output.expanded, output.expand_ctx);
            Hashtbl.replace output.expand_ctx.Expand_ctx.unit_members path
              output.expand_ctx.Expand_ctx.own_unit_members;
            List.filter_map
              (fun (e : macro_export) ->
                if e.public then Some (e.name, e.entry, e.kind, e.params)
                else None)
              output.macro_exports)
      in
      Hashtbl.replace loader.Core_loader.macro_cache resolved macros;
      register_cached macros;
      absorb ()
