exception CircularImport of string
exception ImportNotFound of string

type cache_entry = Parsed of Surface.t

type t = {
  base_dir : string;
  cache : (string, cache_entry) Hashtbl.t;
  active : (string, string) Hashtbl.t;
}

let create ~base_dir =
  { base_dir; cache = Hashtbl.create 16; active = Hashtbl.create 16 }

let load t path f =
  let resolved = Filename.concat t.base_dir (path ^ ".fun") in
  if Hashtbl.mem t.active resolved then raise (CircularImport path);
  if not (Sys.file_exists resolved) then raise (ImportNotFound path);
  let parsed =
    match Hashtbl.find_opt t.cache resolved with
    | Some (Parsed surface) -> surface
    | None ->
        let source = In_channel.with_open_text resolved In_channel.input_all in
        let surface = Core_lexer.parse_module source in
        Hashtbl.replace t.cache resolved (Parsed surface);
        surface
  in
  Hashtbl.replace t.active resolved path;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.active resolved)
    (fun () -> f parsed)
