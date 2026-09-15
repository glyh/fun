module Ctx = Elab_ctx.Ctx

(* The form being elaborated, for a budget overrun to name. An evaluation that
   fails while checking it is an elaboration error at that form: the innermost
   form converts it, outer ones see the elaboration error. *)
let at (ctx : Ctx.t) (expr : Syntax.t) mode f =
  let site : Eval_budget.site = { span = expr.span; mode } in
  Eval_budget.at ctx.metas.budget site (fun () ->
      try f ()
      with Nbe_error.EvalError message ->
        let site = if expr.span.Source_span.synthetic then ctx.metas.budget.site else Some site in
        raise (Elab_error.ElabError (EvaluationFailed { message; site })))

let rec ops : Elab_ops.t =
  {
    infer = (fun ctx expr -> at ctx expr "inferring the form" (fun () -> Elab_infer.infer ops ctx expr));
    check = (fun ctx expr expected -> at ctx expr "checking the form" (fun () -> Elab_check.check ops ctx expr expected));
    type_value_of_expr = (fun ctx expr -> at ctx expr "reading the type" (fun () -> Elab_type_expr.type_value_of_expr ops ctx expr));
  }

let infer ctx expr = ops.infer ctx expr
let check ctx expr expected = ops.check ctx expr expected
let type_value_of_expr ctx expr = ops.type_value_of_expr ctx expr
