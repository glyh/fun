(* The syntax tests assert the shape of expanded programs by the names written
   in their source. Expansion renames every local binder to a fresh resolved
   name ([x] becomes [x#0]), which those assertions are not about, so this is
   [Parse_expand] with resolved names mapped back to written ones before
   lowering. Resolution itself is tested in test_scope_sets and the backend
   suites. *)

let written_name name =
  match String.index_opt name '#' with Some i -> String.sub name 0 i | None -> name

(* An open choice is shown as the name written: these tests are about shape,
   and which open supplies a name is resolution. *)
let lower_written stx =
  let unchoose (form : Syntax.t) =
    match form.kind with
    | Syntax.OpenChoice { name; _ } -> { form with kind = Syntax.Var name }
    | _ -> form
  in
  Shape.lower_expr
    (Expand.map_forms (fun id -> { id with Syntax.name = written_name id.Syntax.name }) unchoose stx)

let expand_lower ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals stx =
  let ctx = Expand_ctx.create () in
  ctx.load_syntax <- load_syntax;
  Option.iter (fun f -> ctx.elaborate <- Some f) elaborate;
  Option.iter (fun f -> ctx.eval_and_apply <- Some f) eval_and_apply;
  Option.iter (fun f -> ctx.load_macros <- Some f) load_macros;
  Option.iter (Expand_ctx.set_syntax_nominals ctx) syntax_nominals;
  lower_written (Expand.expand ctx stx)

let parse_expr ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source =
  Enforest.parse_expr ?open_prelude source
  |> expand_lower ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals

let parse_module ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source =
  Enforest.parse_module source
  |> expand_lower ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals
