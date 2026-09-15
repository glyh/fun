include Elab_error

(* The summands of the trait-bound sugar [Eq + Show]. The [+] is sugar, matched
   as written; each summand is a form the caller resolves. *)
let rec trait_bound_forms (expr : Syntax.t) =
  match expr.kind with
  | Syntax.Ap ({ kind = Syntax.Ap (plus, Explicitness.Explicit, lhs); _ }, Explicitness.Explicit, rhs)
    when Syntax.written_name plus = Some "+" -> trait_bound_forms lhs @ trait_bound_forms rhs
  | _ -> [ expr ]
