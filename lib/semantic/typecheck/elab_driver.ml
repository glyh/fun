module Ctx = Elab_ctx.Ctx

let rec ops : Elab_ops.t =
  {
    infer = (fun ctx expr -> Elab_infer.infer ops ctx expr);
    check = (fun ctx expr expected -> Elab_check.check ops ctx expr expected);
    type_value_of_expr = (fun ctx expr -> Elab_type_expr.type_value_of_expr ops ctx expr);
    collect_effects = (fun ctx expr -> Elab_effect_collect.collect_effects ops ctx expr);
  }

let infer ctx expr = ops.infer ctx expr
let check ctx expr expected = ops.check ctx expr expected
let type_value_of_expr ctx expr = ops.type_value_of_expr ctx expr
let collect_effects ctx expr = ops.collect_effects ctx expr
