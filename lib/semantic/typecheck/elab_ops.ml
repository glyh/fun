open Core

module Ctx = Elab_ctx.Ctx

type t = {
  infer : Ctx.t -> Surface.t -> term * value;
  check : Ctx.t -> Surface.t -> value -> term;
  type_value_of_expr : Ctx.t -> Surface.t -> term * value * value;
  collect_effects : Ctx.t -> Surface.t -> Elab_effects.expr_effects;
}
