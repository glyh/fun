module Ctx = Elab_ctx.Ctx

(* The form being elaborated, for a budget overrun to name. *)
let at (ctx : Ctx.t) (expr : Syntax.t) mode f =
  Eval_budget.at ctx.metas.budget { span = expr.span; mode } f

let rec ops : Elab_ops.t =
  {
    infer = (fun ctx expr -> at ctx expr "inferring the form" (fun () -> Elab_infer.infer ops ctx expr));
    check = (fun ctx expr expected -> at ctx expr "checking the form" (fun () -> Elab_check.check ops ctx expr expected));
    type_value_of_expr = (fun ctx expr -> at ctx expr "reading the type" (fun () -> Elab_type_expr.type_value_of_expr ops ctx expr));
    collect_effects = (fun ctx expr -> Elab_effect_collect.collect_effects ops ctx expr);
  }

let infer ctx expr = ops.infer ctx expr
let check ctx expr expected = ops.check ctx expr expected
let type_value_of_expr ctx expr = ops.type_value_of_expr ctx expr
let collect_effects ctx expr = ops.collect_effects ctx expr
