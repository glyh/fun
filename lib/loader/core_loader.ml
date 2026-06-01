exception CircularImport of string
exception ImportNotFound of string

type t = {
  base_dir : string;
  parsed_cache : (string, Surface.t) Hashtbl.t;
  runtime_cache : (string, Surface.t) Hashtbl.t;
  active : (string, string) Hashtbl.t;
  macro_active : (string, string) Hashtbl.t;
}

let create ~base_dir =
  { base_dir;
    parsed_cache = Hashtbl.create 16;
    runtime_cache = Hashtbl.create 16;
    active = Hashtbl.create 16;
    macro_active = Hashtbl.create 16 }

let resolved_path t path =
  Filename.concat t.base_dir (path ^ ".fun")

let read_module_source resolved =
  In_channel.with_open_text resolved In_channel.input_all

let parse_raw_module t path =
  let resolved = resolved_path t path in
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  match Hashtbl.find_opt t.parsed_cache resolved with
  | Some surface -> surface
  | None ->
      let source = read_module_source resolved in
      let surface = Parse_expand.parse_with Core_parser.module_eof source in
      Hashtbl.replace t.parsed_cache resolved surface;
      surface

let parse_runtime_module t path =
  let resolved = resolved_path t path in
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  match Hashtbl.find_opt t.runtime_cache resolved with
  | Some surface -> surface
  | None ->
      let source = read_module_source resolved in
      let surface = Parse_expand.parse_module source in
      Hashtbl.replace t.runtime_cache resolved surface;
      surface

let load t path f =
  let resolved = resolved_path t path in
  if Hashtbl.mem t.active resolved then raise (CircularImport path);
  let parsed = parse_runtime_module t path in
  Hashtbl.replace t.active resolved path;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.active resolved)
    (fun () -> f parsed)

let rec visit_macros t (ctx : Expand_ctx.t) path =
  let resolved = resolved_path t path in
  if Hashtbl.mem t.macro_active resolved then raise (CircularImport path);
  let module_expr = parse_raw_module t path in
  let bindings =
    match module_expr with
    | Surface.Module { bindings } | Surface.Struct { bindings; _ } -> bindings
    | _ -> raise (Invalid_argument ("module expected: " ^ path))
  in
  Hashtbl.replace t.macro_active resolved path;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.macro_active resolved)
    (fun () ->
       List.iter
         (function
           | Surface.MacroBinding { name; value; public = true } -> (
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
                   Expand_ctx.register_macro ctx ~name ~value:macro_fn
               | None -> ())
           | _ -> ())
         bindings)
