open Core
include Elab_error
open Elab_common
open Elab_prelude

module Ctx = Elab_ctx.Ctx

open Elab_resolve

(** Build the initial elaboration context with built-in types ([I64],
    [Bool], [Unit], [Char], [Type]) and primitive operators. *)
(* The builtins, and stage 1 of the prelude bound as [stdlib]: the base stage 2
   of the prelude is elaborated against. *)
let stage1_ctx () : Ctx.t =
  let ctx = Ctx.empty () in
  let add_type ctx name v = Ctx.define ctx name VU v in
  let ctx = add_type ctx Compiler_names.Type_name.i64 (VAtomTy Atom_ty.TI64) in
  (* [Bool] is no longer a builtin atom type — it is the nominal ADT defined in
     the prelude ([type Bool = False | True]). *)
  let ctx = add_type ctx Compiler_names.Type_name.unit (VAtomTy Atom_ty.TUnit) in
  let ctx = add_type ctx Compiler_names.Type_name.char (VAtomTy Atom_ty.TChar) in
  let ctx = add_type ctx Compiler_names.Type_name.string (VAtomTy Atom_ty.TString) in
  let ctx = add_type ctx Compiler_names.Type_name.scopes (VAtomTy Atom_ty.TScopes) in
  let ctx = add_type ctx Compiler_names.Type_name.absurd (VAtomTy Atom_ty.TAbsurd) in
  let ctx = Ctx.define ctx Compiler_names.Type_name.type_ VU VU in
  let ctx = Ctx.define ctx Compiler_names.Type_name.effect_row VU VEffectRowTy in
  (* [Ref : [h : Type] -> Type -> Type]: the heap is an implicit argument, never
     written, so each [Ref(A)] gets a fresh one. *)
  let pi explicitness domain codomain = Pi { explicitness; domain; effects = empty_effect_row; codomain } in
  let ref_ty = Ctx.eval ctx (pi Implicit U (pi Explicit U U)) in
  let ctx = Ctx.define ctx Compiler_names.Type_name.ref_ ref_ty (Ctx.eval ctx (Lam (Lam (RefTy (Var 1, Var 0))))) in
  (* The [Mutate] family, and [Mutate : [h : Type] -> [A : Type] -> Ref(h, A) -> Type]
     mapping a reference to the effect on its heap. *)
  let mutate = Compiler_names.Effect_name.mutate in
  let family = VEffect { id = mutate_effect_id; name = mutate; params = []; operations = [] } in
  let ctx = Ctx.define ctx Compiler_names.Effect_name.mutate_family VU family in
  let mutate_ty = Ctx.eval ctx (pi Implicit U (pi Implicit U (pi Explicit (RefTy (Var 1, Var 0)) U))) in
  let ctx = Ctx.define ctx mutate mutate_ty (Ctx.eval ctx (Lam (Lam (Lam (EffectRef { id = mutate_effect_id; name = mutate; params = [ Var 2 ] }))))) in
  let ctx =
    NameMap.fold
      (fun name ty ctx ->
        Ctx.define ctx name ty (VNeutral { ty; neutral = { head = HPrim name; frames = [] } }))
      prims ctx
  in
  let stdlib_core, stdlib_ty = Elab_driver.infer ctx (Lazy.force parsed_stage1) in
  let stdlib_value = Ctx.eval ctx stdlib_core in
  let ctx = Ctx.hide_names ctx syntax_primitive_names in
  let ctx = Ctx.define ctx Compiler_names.Module_name.stdlib stdlib_ty stdlib_value in
  (* Freeze this as THE base context: a compilation unit's meaning depends only
     on its own source plus what it imports and opens, so every import
     elaborates against this rather than against the import site. *)
  { ctx with base = Some ctx }


let resolve_stdlib (ctx : Ctx.t) (path : string list) : value =
  Elab_stdlib.resolve ctx path

(* Elaborate an expanded expression as-is. Under the strict phase rule the prelude
   is brought into scope by an [open (import "std")] carried in [expr] itself —
   either written by the program or injected by the parser's [~open_prelude]
   convenience (which wraps the body in [Open (Import "std", body)]). This entry
   point adds no implicit open: an [expr] with no such open elaborates in the bare
   base context, so [+]/[Some]/… are unbound unless the program opened [std]. *)
(* A budget overrun surfaces from deep inside the evaluator; the checker reports
   it as an elaboration error like any other. *)
let reporting_budget f =
  try f () with Eval_budget.Exceeded { limit; call; demand; site } -> raise (ElabError (EvaluationBudgetExceeded { limit; call; demand; site }))

let on_expr_effects ?loader (ctx : Ctx.t) (expr : Syntax.t) : term * value * Elab_effects.expr_effects =
  let ctx = match loader with Some loader -> Ctx.with_loader ctx loader | None -> ctx in
  reporting_budget (fun () ->
      let (core, ty), effects = Elab_effects.collecting ctx (fun ctx -> Elab_driver.infer ctx expr) in
      (core, ty, effects))

(* A program's entry: what it leaves unhandled is an error, not a run-time crash. *)
let on_expr ?loader (ctx : Ctx.t) (expr : Syntax.t) : term * value =
  let core, ty, effects = on_expr_effects ?loader ctx expr in
  reporting_budget (fun () -> Elab_effects.require_handled_at_entry ctx effects);
  (core, ty)

