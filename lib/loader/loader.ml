exception CircularImport of string
exception ImportNotFound of string

type result = Typed_ir.Expr.t * Type.TypeDefs.t
type t = string -> result

type cache_entry = In_progress | Done of result

let parse_module source =
  let lexbuf = Sedlexing.Utf8.from_string source in
  let lexer = Sedlexing.with_tokenizer Syntax.Lexer.token lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Syntax.Parser.module_eof
    lexer

let create ~base_dir ~(typecheck_fn : ?loader:t -> Syntax.Ast.Expr.t -> result) : t =
  let cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 16 in
  let rec loader path =
    let resolved = Filename.concat base_dir (path ^ ".fun") in
    match Hashtbl.find_opt cache resolved with
    | Some (Done r) -> r
    | Some In_progress -> raise (CircularImport path)
    | None ->
        if not (Sys.file_exists resolved) then raise (ImportNotFound path);
        Hashtbl.replace cache resolved In_progress;
        let source = In_channel.with_open_text resolved In_channel.input_all in
        let (args, body, defs) = parse_module source in
        let expr = Syntax.Ast.Expr.StructDef { args; body; members = defs } in
        let result = typecheck_fn ~loader expr in
        Hashtbl.replace cache resolved (Done result);
        result
  in
  loader
