(* Differential single-file runner: the OCaml half of scripts/differential.sh.
   Mirrors test/conformance/run_conformance.ml's elaboration and evaluation and
   describe_value, so a program is judged exactly as the conformance suite judges
   it, and prints a machine-readable outcome the harness compares against the
   .NET port's `--file` mode.

   Usage:  differential.exe <file.fun>

   Output protocol (one line, newlines collapsed):
     OK            the program elaborates (sibling .expect is "ok", so it is not run)
     VALUE <s>     the program evaluated to <s> under describe_value
     ELAB <msg>    expansion/elaboration failed
     EVAL <msg>    evaluation failed

   A hang is not reported here: the harness runs this under `timeout` and records
   a hang itself. *)

open Core

type mode = Elaborates | Run

let read_file path = In_channel.with_open_text path In_channel.input_all

let unit_infix = ".unit-"

(* A case's extra compilation units: [<name>.unit-<unit>.fun] beside it, each
   importable as "<unit>" (same as run_conformance.ml). *)
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
      let dir = Filename.temp_dir "fun_differential" "" in
      List.iter
        (fun (name, path) ->
          Out_channel.with_open_text (Filename.concat dir (name ^ ".fun")) (fun oc ->
              output_string oc (read_file path)))
        units;
      let builtin_syntax = Macro_driver.std_syntax () in
      Some (Core_loader.create ~base_dir:dir ~builtin_syntax ())

(* Parse with macros enabled, then elaborate, as at the REPL (run_conformance.ml). *)
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

(* The conformance suite's value normalization (run_conformance.ml): an I64 as its
   digits, a constructor as its name, anything else a debug form. The .NET port's
   Driver.Describe matches this for exactly the cases .expect may state. *)
let describe_value value =
  match value with
  | VAtom (Atom.I64 n) -> Int64.to_string n
  | VCon { name; _ } -> name
  | v -> Debug.pp_value_short (MetaContext.create ()) v

let one_line s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> Buffer.add_char buf (if c = '\n' || c = '\r' then ' ' else c)) s;
  Buffer.contents buf

let () =
  if Array.length Sys.argv < 2 then failwith "usage: differential.exe <file.fun>";
  let path = Sys.argv.(1) in
  let dir = Filename.dirname path in
  let base = Filename.remove_extension (Filename.basename path) in
  let source = read_file path in
  let expect_file = Filename.concat dir (base ^ ".expect") in
  let mode =
    if Sys.file_exists expect_file && String.equal (String.trim (read_file expect_file)) "ok"
    then Elaborates
    else Run
  in
  let loader = loader_for (unit_sources dir base) in
  match elaborate_case ?loader source with
  | exception e -> Printf.printf "ELAB %s\n%!" (one_line (Printexc.to_string e))
  | ctx, core -> (
      match mode with
      | Elaborates -> Printf.printf "OK\n%!"
      | Run -> (
          match Elaborate.Ctx.run ctx core with
          | exception e -> Printf.printf "EVAL %s\n%!" (one_line (Printexc.to_string e))
          | value -> Printf.printf "VALUE %s\n%!" (one_line (describe_value value))))
