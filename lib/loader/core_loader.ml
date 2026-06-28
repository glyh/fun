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
  parsed_cache : (string, Surface.t) Hashtbl.t;
  runtime_surface_cache : (string, Surface.t) Hashtbl.t;
  runtime_elab_cache : (string, Core.term * Core.value * Core.value) Hashtbl.t;
  macro_cache : (string, (string * Core.value * Syntax.MacroKind.t) list) Hashtbl.t;
  syntax_cache : (string, Operator_env.export list) Hashtbl.t;
  active : (string, string) Hashtbl.t;
  macro_active : (string, string) Hashtbl.t;
  syntax_active : (string, string) Hashtbl.t;
}

let create ~base_dir =
  { base_dir;
    parsed_cache = Hashtbl.create 16;
    runtime_surface_cache = Hashtbl.create 16;
    runtime_elab_cache = Hashtbl.create 16;
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
             |> Enforest.parse_public_syntax_exports ~file:resolved ~load_syntax:(load_syntax_exports t))
      in
      Hashtbl.replace t.syntax_cache resolved exports;
      exports

let parse_raw_module_source ?file t source =
  Enforest.parse_module ?file ~load_syntax:(load_syntax_exports t) source |> Lower_surface.lower_expr

let parse_raw_module t path =
  let resolved = resolved_path t path in
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  match Hashtbl.find_opt t.parsed_cache resolved with
  | Some surface -> surface
  | None ->
      let source = read_module_source resolved in
      let surface = parse_raw_module_source ~file:resolved t source in
      Hashtbl.replace t.parsed_cache resolved surface;
      surface

let parse_runtime_module t ?eval_and_apply path =
  let resolved = resolved_path t path in
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  let load_macros ctx _path =
    match Hashtbl.find_opt t.macro_cache resolved with
    | Some macros ->
        List.iter (fun (name, value, kind) ->
          Expand_ctx.register_macro ctx ~name ~value;
          Expand_ctx.register_macro_kind ctx ~name ~kind)
          macros
    | None -> ()
  in
  (match eval_and_apply with
   | Some _ ->
       let source = read_module_source resolved in
       Parse_expand.parse_module ?eval_and_apply ~load_macros ~load_syntax:(load_syntax_exports t) source
   | None ->
       match Hashtbl.find_opt t.runtime_surface_cache resolved with
       | Some surface -> surface
       | None ->
           let source = read_module_source resolved in
           let surface = Parse_expand.parse_module ~load_macros ~load_syntax:(load_syntax_exports t) source in
           Hashtbl.replace t.runtime_surface_cache resolved surface;
           surface)

let load t path f =
  let resolved = resolved_path t path in
  if Hashtbl.mem t.active resolved then raise (CircularImport path);
  let parsed = parse_runtime_module t path in
  Hashtbl.replace t.active resolved path;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.active resolved)
    (fun () -> f parsed)

let load_elaborated t path ~elaborate ~eval_and_apply =
  let resolved = resolved_path t path in
  if Hashtbl.mem t.active resolved then raise (CircularImport path);
  match Hashtbl.find_opt t.runtime_elab_cache resolved with
  | Some result -> result
  | None ->
      let parsed = parse_runtime_module t path ~eval_and_apply in
      Hashtbl.replace t.active resolved path;
      let result =
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove t.active resolved)
          (fun () -> elaborate parsed)
      in
      Hashtbl.replace t.runtime_elab_cache resolved result;
      result

let rec visit_macros t (ctx : Expand_ctx.t) path =
  let resolved = resolved_path t path in
  let register_cached macros =
    List.iter (fun (name, value, kind) ->
      Expand_ctx.register_macro ctx ~name ~value;
      Expand_ctx.register_macro_kind ctx ~name ~kind)
      macros
  in
  match Hashtbl.find_opt t.macro_cache resolved with
  | Some macros -> register_cached macros
  | None ->
      if Hashtbl.mem t.macro_active resolved then raise (CircularMacroVisit path);
      let module_expr = parse_raw_module t path in
      let bindings =
        match module_expr with
        | Surface.Module { bindings } | Surface.Struct { bindings; _ } -> bindings
        | _ -> raise (Invalid_argument ("module expected: " ^ path))
      in
      Hashtbl.replace t.macro_active resolved path;
      let macros =
        Fun.protect
          ~finally:(fun () -> Hashtbl.remove t.macro_active resolved)
          (fun () ->
             List.fold_left
               (fun acc -> function
                   | Surface.MacroBinding { name; value; public = true; kind; _ } -> (
                      match ctx.Expand_ctx.elaborate with
                      | Some elaborate ->
                          let eval_and_apply = ctx.Expand_ctx.eval_and_apply in
                          let lowered =
                            Parse_expand.expand_lower
                              ~elaborate
                              ?eval_and_apply
                              ~load_macros:(visit_macros t)
                              value
                          in
                          let macro_fn = elaborate lowered in
                          Expand_ctx.register_macro ctx ~name ~value:macro_fn;
                          let resolved_kind = match kind with Some k -> k | None -> Syntax.MacroKind.default in
                          Expand_ctx.register_macro_kind ctx ~name ~kind:resolved_kind;
                          (name, macro_fn, resolved_kind) :: acc
                      | None -> acc)
                 | _ -> acc)
               [] bindings
             |> List.rev)
      in
      Hashtbl.replace t.macro_cache resolved macros
