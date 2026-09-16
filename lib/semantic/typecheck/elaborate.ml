open Core
include Elab_error

module Ctx = Elab_ctx.Ctx

type expr_effect = Elab_effects.expr_effect = { core : term; value : value }
type expr_effects = Elab_effects.expr_effects = { effects : expr_effect list; tails : expr_effect list }

let init_ctx = Macro_driver.init_ctx
let resolve_stdlib = Elab_entry.resolve_stdlib
let syntax_nominals = Elab_stdlib.syntax_nominals
let on_expr = Elab_entry.on_expr
let on_expr_effects = Elab_entry.on_expr_effects
