open Core

module Ctx = Elab_ctx.Ctx

type t = {
  infer : Ctx.t -> Syntax.t -> term * value;
  check : Ctx.t -> Syntax.t -> value -> term;
  type_value_of_expr : Ctx.t -> Syntax.t -> term * value * value;
  collect_effects : Ctx.t -> Syntax.t -> Elab_effects.expr_effects;
}
