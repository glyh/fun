include Elab_error

(* The forms of a trait bound: a set [{Eq, Show}], or one bare [Eq]. Each is a
   form the caller resolves. *)
let trait_bound_forms (expr : Syntax.t) =
  match expr.kind with
  | Syntax.TraitBoundSet forms -> forms
  | _ -> [ expr ]
