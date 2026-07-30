open Core
include Elab_error
open Elab_common
open Elab_prelude

module Ctx = Elab_ctx.Ctx

open Elab_resolve

(** Build the initial elaboration context with built-in types ([I64],
    [Bool], [Unit], [Char], [Type]) and primitive operators. *)
let init_ctx () : Ctx.t =
  let ctx = Ctx.empty () in
  let add_type ctx name v = Ctx.define ctx name VU v in
  let ctx = add_type ctx Compiler_names.Type_name.i64 (VAtomTy Atom_ty.TI64) in
  (* [Bool] is no longer a builtin atom type — it is the nominal ADT defined in
     the prelude ([type Bool = False | True]). *)
  let ctx = add_type ctx Compiler_names.Type_name.unit (VAtomTy Atom_ty.TUnit) in
  let ctx = add_type ctx Compiler_names.Type_name.char (VAtomTy Atom_ty.TChar) in
  let ctx = add_type ctx Compiler_names.Type_name.string (VAtomTy Atom_ty.TString) in
  let ctx = add_type ctx Compiler_names.Type_name.absurd (VAtomTy Atom_ty.TAbsurd) in
  let ctx = Ctx.define ctx Compiler_names.Type_name.type_ VU VU in
  let ctx = Ctx.define ctx Compiler_names.Type_name.effect_row VU VEffectRowTy in
  let ref_ty = VPi { explicitness = Explicit; domain = VU; effects = effect_row_closure ctx.env empty_effect_row; codomain = { env = ctx.env; body = U } } in
  let ctx = Ctx.define ctx Compiler_names.Type_name.ref_ ref_ty (VLam { body = { env = ctx.env; body = RefTy (Var 0) } }) in
  let ctx =
    NameMap.fold
      (fun name ty ctx ->
        Ctx.define ctx name ty (VNeutral { ty; neutral = { head = HPrim name; frames = [] } }))
      prims ctx
  in
  let stdlib_core, stdlib_ty = Elab_driver.infer ctx (Lazy.force parsed_stdlib) in
  let stdlib_value = Ctx.eval ctx stdlib_core in
  let ctx = Ctx.hide_names ctx syntax_primitive_names in
  Ctx.define ctx Compiler_names.Module_name.stdlib stdlib_ty stdlib_value

(* Return [ctx] extended with the prelude's public fields in scope — the
   ctx-builder counterpart of [on_macro_body], which brings the prelude into
   scope for a *single* expression by wrapping it in the [Open (import "std", …)]
   construct. Both bottom out in the same [open_module_value] on [std]; this form
   exists for the one caller ([Macro_driver]) that needs a *persistent*
   prelude-opened context, reused across every macro-body compilation and the
   per-binding advancement hook (which elaborates module bindings via
   [elab_module_binding] directly, not through an expression that could carry an
   [Open]). Because that context is opened once here, [Macro_driver] elaborates
   macro bodies with [Elab_driver.infer] rather than [on_macro_body] — the latter
   would open [std] a second time and shift the de Bruijn indices.

   This supersedes the parent operator-demotion ticket's "delete implicit
   open_stdlib" line: the *user-code* implicit open is gone (see [on_expr]); this
   surviving use is a deliberate, non-user-facing macro-compilation mechanism. *)
let open_stdlib ctx =
  let ix, ty = Ctx.lookup ctx Compiler_names.Module_name.stdlib in
  let value = Ctx.eval ctx (Var ix) in
  open_module_value ctx ty value

let resolve_stdlib (ctx : Ctx.t) (path : string list) : value =
  Elab_stdlib.resolve ctx path

(* Elaborate a surface expression as-is. Under the strict phase rule the prelude
   is brought into scope by an [open (import "std")] carried in [expr] itself —
   either written by the program or injected by the parser's [~open_prelude]
   convenience (which wraps the body in [Open (Import "std", body)]). This entry
   point adds no implicit open: an [expr] with no such open elaborates in the bare
   base context, so [+]/[Some]/… are unbound unless the program opened [std]. *)
let on_expr ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  Elab_driver.infer ctx expr

let on_expr_effects ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value * Elab_effects.expr_effects =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  let core, ty = Elab_driver.infer ctx expr in
  (core, ty, Elab_driver.collect_effects ctx expr)

(* Elaborate a macro transformer body. Unlike user expressions (governed by the
   strict phase rule), transformer bodies are compiler-facing — they use the
   [Syntax] API and the prelude operators — so they always elaborate with [std]
   open, via the same [open (import "std")] construct. *)
let on_macro_body ?loader (ctx : Ctx.t) (expr : Surface.t) : term * value =
  on_expr ?loader ctx
    (Surface.Open (Surface.Import Compiler_names.Module_name.std_import_path, expr))
