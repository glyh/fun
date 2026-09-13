(* The syntax tests assert the shape of expanded programs by the names written
   in their source. Expansion renames every local binder to a fresh resolved
   name ([x] becomes [x__0]), which those assertions are not about, so this is
   [Parse_expand] with resolved names mapped back to written ones before
   lowering. Resolution itself is tested in test_scope_sets and the backend
   suites. *)

let written_name name =
  match String.rindex_opt name '_' with
  | Some i when i >= 1 && name.[i - 1] = '_' && i + 1 < String.length name
                && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub name (i + 1) (String.length name - i - 1)) ->
      String.sub name 0 (i - 1)
  | _ -> name

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

let expand_lower ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ~expansion_position stx =
  let ctx = Expand_ctx.create () in
  Option.iter (fun f -> ctx.elaborate <- Some f) elaborate;
  Option.iter (fun f -> ctx.eval_and_apply <- Some f) eval_and_apply;
  Option.iter (fun f -> ctx.load_macros <- Some f) load_macros;
  Option.iter (Expand_ctx.set_syntax_nominals ctx) syntax_nominals;
  Expand_ctx.set_expansion_position ctx expansion_position;
  lower_written (Expand.expand ctx stx)

let parse_expr ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?open_prelude ?syntax_nominals source =
  Enforest.parse_expr ?load_syntax ?open_prelude source
  |> expand_lower ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals
       ~expansion_position:Syntax.MacroKind.(Expr (None, None))

let parse_module ?elaborate ?eval_and_apply ?load_macros ?load_syntax ?syntax_nominals source =
  Enforest.parse_module ?load_syntax source
  |> expand_lower ?elaborate ?eval_and_apply ?load_macros ?syntax_nominals ~expansion_position:Syntax.MacroKind.Decl
