open Syntax

let test_spec_ident () =
  let env = Enforest_util.env () in
  let tokens = Raw_syntax.read "hello" in
  match Parse_spec.parse Parse_spec.ident env tokens with
  | Some ((id, _span), []) ->
      Alcotest.(check string) "ident name" "hello" id.name
  | _ -> Alcotest.fail "expected ident"

let test_spec_seq () =
  let env = Enforest_util.env () in
  let tokens = Raw_syntax.read "macro abc" in
  let name_spec = {
    Parse_spec.run = (fun _env -> function
      | { datum = Token { kind = Ident n; _ }; _ } :: rest -> Some (n, rest)
      | _ -> None);
    Parse_spec.name = "name";
  } in
  let macro = Parse_spec.punct KwMacro in
  let spec = Parse_spec.seq macro name_spec in
  match spec.Parse_spec.run env tokens with
  | Some ((_, name), _) ->
      Alcotest.(check string) "macro name" "abc" name
  | _ -> Alcotest.fail "expected macro name"

let suites = [
  ("parse_spec", [
    Alcotest.test_case "spec ident" `Quick test_spec_ident;
    Alcotest.test_case "spec seq" `Quick test_spec_seq;
  ]);
]
