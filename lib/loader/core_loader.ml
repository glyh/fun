exception CircularImport of string
exception CircularMacroVisit of string
exception CircularSyntaxVisit of string
exception ImportNotFound of string

let () =
  Printexc.register_printer (function
    | CircularImport path -> Some ("CircularImport \"" ^ path ^ "\"")
    | CircularMacroVisit path -> Some ("CircularMacroVisit \"" ^ path ^ "\"")
    | CircularSyntaxVisit path -> Some ("CircularSyntaxVisit \"" ^ path ^ "\"")
    | ImportNotFound path -> Some ("ImportNotFound \"" ^ path ^ "\"")
    | _ -> None)

type t = {
  base_dir : string;
  (* Prelude syntax exports, injected at [create] by a layer that can name the
     prelude (the loader layer cannot). Seeded into every module parse so
     imported [.fun] files see the stdlib operators/[if], and returned for the
     reserved [import "std"] path. Replaces the old [builtin_syntax_hook] ref. *)
  builtin_syntax : (string * Syntax.role) list;
  runtime_expanded_cache : (string, Syntax.t) Hashtbl.t;
  runtime_elab_cache : (string, Core.term * Core.value * Core.value) Hashtbl.t;
  (* The expanded unit [Macro_driver.run] produced, with its expander.
     The driver interleaves expansion and elaboration, so a macro a unit defines
     is compiled and its calls inside that unit expand. Re-expanding the unit
     here instead would run an expander with no [elaborate] callback, which
     compiles no macro and leaves every macro call in the file unexpanded. *)
  driver_expanded_cache : (string, Syntax.t * Expand_ctx.t) Hashtbl.t;
  macro_cache : (string, (string * Core.value * Syntax.MacroKind.t * Macro_eval.syntax_nominals option) list) Hashtbl.t;
  syntax_cache : (string, (string * Syntax.role) list) Hashtbl.t;
  active : (string, string) Hashtbl.t;
  macro_active : (string, string) Hashtbl.t;
  syntax_active : (string, string) Hashtbl.t;
}

let create ~base_dir ?(builtin_syntax = []) () =
  { base_dir;
    builtin_syntax;
    runtime_expanded_cache = Hashtbl.create 16;
    runtime_elab_cache = Hashtbl.create 16;
    driver_expanded_cache = Hashtbl.create 16;
    macro_cache = Hashtbl.create 16;
    syntax_cache = Hashtbl.create 16;
    active = Hashtbl.create 16;
    macro_active = Hashtbl.create 16;
    syntax_active = Hashtbl.create 16 }

let resolved_path t path =
  Filename.concat t.base_dir (path ^ ".fun")

let read_module_source resolved =
  In_channel.with_open_text resolved In_channel.input_all

let rec load_syntax_exports t path =
  (* [import "std"] is the reserved builtin prelude, not a file: its syntax
     exports are the injected [builtin_syntax] (operators, [if]/[&&]/[||]), not
     the contents of a [std.fun] file. *)
  if String.equal path Compiler_names.Module_name.std_import_path then t.builtin_syntax else
  let resolved = resolved_path t path in
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  match Hashtbl.find_opt t.syntax_cache resolved with
  | Some exports -> exports
  | None ->
      if Hashtbl.mem t.syntax_active resolved then raise (CircularSyntaxVisit path);
      Hashtbl.replace t.syntax_active resolved path;
      let exports =
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove t.syntax_active resolved)
          (fun () ->
             read_module_source resolved
             |> Parse_expand.syntax_exports ~file:resolved ~load_syntax:(load_syntax_exports t))
      in
      Hashtbl.replace t.syntax_cache resolved exports;
      exports

let parse_runtime_module t ?eval_and_apply ?syntax_nominals path =
  let resolved = resolved_path t path in
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  (* Restores THIS unit's own compiled macros when it is re-parsed from cache,
     so its later bindings still see its earlier ones. Not the import path: a
     macro an [import] delivers arrives through [Macro_driver.visit_macros],
     filed under its own unit. *)
  let load_macros ctx _path =
    match Hashtbl.find_opt t.macro_cache resolved with
    | Some macros ->
        List.iter (fun (name, value, kind, syntax_nominals) ->
          Expand_ctx.register_macro_with_nominals ctx ~syntax_nominals ~name ~value;
          Expand_ctx.register_macro_kind ctx ~name ~kind)
          macros
    | None -> ()
  in
  (match eval_and_apply with
   | Some _ ->
       let source = read_module_source resolved in
        let expanded, ctx = Parse_expand.parse_module_with_ctx ?eval_and_apply ?syntax_nominals ~load_macros ~load_syntax:(load_syntax_exports t) source in
       (* Cache macros from expansion context so elaborator can find them *)
        Hashtbl.iter (fun name entry ->
          let kind = match Hashtbl.find_opt ctx.Expand_ctx.macro_kind_table name with
            | Some k -> k | None -> Syntax.MacroKind.default in
          Hashtbl.replace t.macro_cache resolved
            ((name, entry.Expand_ctx.value, kind, entry.Expand_ctx.syntax_nominals) :: (Option.value ~default:[] (Hashtbl.find_opt t.macro_cache resolved))))
          ctx.Expand_ctx.macro_table;
       expanded
   | None ->
       match Hashtbl.find_opt t.runtime_expanded_cache resolved with
       | Some expanded -> expanded
       | None ->
           let source = read_module_source resolved in
           let expanded = Parse_expand.parse_module ~load_macros ~load_syntax:(load_syntax_exports t) source in
           Hashtbl.replace t.runtime_expanded_cache resolved expanded;
           expanded)

let load t path f =
  let resolved = resolved_path t path in
  if Hashtbl.mem t.active resolved then raise (CircularImport path);
  let parsed = parse_runtime_module t path in
  Hashtbl.replace t.active resolved path;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.active resolved)
    (fun () -> f parsed)

let load_elaborated t path ~elaborate ~eval_and_apply ~syntax_nominals =
  let resolved = resolved_path t path in
  if Hashtbl.mem t.active resolved then raise (CircularImport path);
  match Hashtbl.find_opt t.runtime_elab_cache resolved with
  | Some result -> result
  | None ->
      let load_macros ctx _path =
        match Hashtbl.find_opt t.macro_cache resolved with
        | Some macros ->
            List.iter (fun (name, value, kind, syntax_nominals) ->
              Expand_ctx.register_macro_with_nominals ctx ~syntax_nominals ~name ~value;
              Expand_ctx.register_macro_kind ctx ~name ~kind)
              macros
        | None -> ()
      in
      if not (Sys.file_exists resolved) then raise (ImportNotFound path);
      let expanded, expand_ctx =
        match Hashtbl.find_opt t.driver_expanded_cache resolved with
        | Some cached -> cached
        | None ->
            Parse_expand.parse_module_with_ctx ~eval_and_apply ~syntax_nominals
              ~load_macros ~load_syntax:(load_syntax_exports t) (read_module_source resolved)
      in
      Hashtbl.replace t.active resolved path;
      let result =
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove t.active resolved)
          (fun () -> elaborate expanded expand_ctx)
      in
      Hashtbl.replace t.runtime_elab_cache resolved result;
      result

(* Stage 8/9: the old [visit_macros] path that compiled imported macros
   separately (with parser-heuristic kind resolution) has been retired.
   Imported macros are now compiled through [Macro_driver.visit_macros],
   which shares this loader's [macro_cache] and [macro_active] state. *)
