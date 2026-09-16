(* Runs the shared conformance suite: every [cases/<area>/<name>.fun] program is
   elaborated and run the way the REPL does, and its result compared with
   [<name>.expect]. The same files are the .NET port's acceptance suite, so a
   case may only depend on a program's result, never on OCaml-side internals.
   See cases/README.md and docs/wayfinder/tickets/port-core-tt-to-dotnet.md. *)

open Core

type expectation = Value of string | Elaborates | Fails

let read_file path = In_channel.with_open_text path In_channel.input_all

let parse_expect text =
  match String.trim text with
  | "ok" -> Elaborates
  | "error" -> Fails
  | value -> Value value

let builtin_syntax = Macro_driver.std_syntax ()
let unit_infix = ".unit-"

let is_unit_file file =
  let rec has i =
    i + String.length unit_infix <= String.length file
    && (String.equal (String.sub file i (String.length unit_infix)) unit_infix || has (i + 1))
  in
  has 0

(* A case's extra compilation units: [<name>.unit-<unit>.fun] beside it. *)
let unit_sources dir base =
  let prefix = base ^ unit_infix in
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.filter_map (fun file ->
         if String.starts_with ~prefix file && Filename.check_suffix file ".fun" then
           let len = String.length file - String.length prefix - String.length ".fun" in
           Some (String.sub file (String.length prefix) len, Filename.concat dir file)
         else None)

let loader_for units =
  match units with
  | [] -> None
  | _ ->
      let dir = Filename.temp_dir "fun_conformance" "" in
      List.iter
        (fun (name, path) ->
          Out_channel.with_open_text (Filename.concat dir (name ^ ".fun")) (fun oc ->
              output_string oc (read_file path)))
        units;
      Some (Core_loader.create ~base_dir:dir ~builtin_syntax ())

(* Parse with macros enabled, then elaborate: a program may define and call
   macros and import units, as at the REPL. *)
let elaborate_case ?loader source =
  let macro_ctx = Elaborate.init_ctx () in
  let syntax_nominals = Elaborate.syntax_nominals macro_ctx in
  let elaborate expr =
    let core, _ty = Elaborate.on_expr ?loader macro_ctx expr in
    Elaborate.Ctx.eval macro_ctx core
  in
  let expr, expand_ctx =
    Parse_expand.parse_expr_with_ctx ~elaborate ~eval_and_apply:Nbe.apply_macro ~syntax_nominals
      ?load_macros:(Option.map Macro_driver.visit_macros loader)
      ~load_syntax:
        (match loader with
        | None -> Macro_driver.std_load_syntax
        | Some loader -> Core_loader.load_syntax_exports loader)
      ~open_prelude:true source
  in
  let ctx = Elab_ctx.Ctx.with_expander (Elaborate.init_ctx ()) expand_ctx in
  let core, _ty = Elaborate.on_expr ?loader ctx expr in
  (ctx, core)

let describe_value value =
  match value with
  | VAtom (Atom.I64 n) -> Int64.to_string n
  | VCon { name; _ } -> name
  | v -> Debug.pp_value_short (MetaContext.create ()) v

(* [None] when the case passes, [Some reason] when it fails. *)
let run_case path =
  let dir = Filename.dirname path in
  let base = Filename.remove_extension (Filename.basename path) in
  let expect = parse_expect (read_file (Filename.concat dir (base ^ ".expect"))) in
  let source = read_file path in
  let loader = loader_for (unit_sources dir base) in
  match elaborate_case ?loader source with
  | exception e -> (
      match expect with
      | Fails -> None
      | _ -> Some (Printf.sprintf "elaboration failed: %s" (Printexc.to_string e)))
  | ctx, core -> (
      match expect with
      | Fails -> Some "expected an error"
      | Elaborates -> None
      | Value expected -> (
          match Elaborate.Ctx.run ctx core with
          | exception e -> Some (Printf.sprintf "evaluation failed: %s" (Printexc.to_string e))
          | value ->
              let got = describe_value value in
              if String.equal got expected then None
              else Some (Printf.sprintf "expected %s, got %s" expected got)))

let case_files root =
  Sys.readdir root |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun area ->
         let dir = Filename.concat root area in
         if Sys.is_directory dir then
           Sys.readdir dir |> Array.to_list |> List.sort String.compare
           |> List.filter_map (fun file ->
                  if Filename.check_suffix file ".fun" && not (is_unit_file file) then
                    Some (Filename.concat dir file)
                  else None)
         else [])

let () =
  let cases = case_files "cases" in
  let failures =
    List.filter_map (fun path -> Option.map (fun why -> (path, why)) (run_case path)) cases
  in
  List.iter (fun (path, why) -> Printf.printf "FAIL %s: %s\n" path why) failures;
  Printf.printf "conformance: %d cases, %d failed\n" (List.length cases) (List.length failures);
  if failures <> [] then exit 1
