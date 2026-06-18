open Core
include Elab_error

module Ctx = Elab_ctx.Ctx

type expr_effect = Elab_effects.expr_effect = { core : term; value : value }
type expr_effects = Elab_effects.expr_effects = { effects : expr_effect list; tail : expr_effect option }

let init_ctx = Elab_entry.init_ctx
let open_stdlib = Elab_entry.open_stdlib
let resolve_stdlib = Elab_entry.resolve_stdlib
let on_expr = Elab_entry.on_expr
let on_expr_effects = Elab_entry.on_expr_effects
