(* [~>]: an arrow whose effects are decided by where it sits in its signature
   (effect-arrow-syntax). Parameters mint, results collect: a [~>] in a
   parameter position gets its own row variable, bound implicitly at the
   signature's root; a [~>] in a result position carries the variables its
   parameters minted - or, in a definition ([fn(…) ~> T { … }], row [inferred]),
   what the body performs. *)

open Syntax

let counter = ref 0

(* A row variable's name: [#] keeps it apart from anything written. *)
let fresh_row () =
  incr counter;
  fresh_id ("~e#" ^ string_of_int !counter)

let effect_row_ty = synth (Var (fresh_id Compiler_names.Type_name.effect_row))

let is_poly = function Some (r : effect_row) -> r.polymorphic | None -> false

let rec has_poly (t : t) =
  match t.kind with
  | Arrow (_, _, a, row, b) -> is_poly row || has_poly a || has_poly b
  | _ -> false

let rec lambda_has_poly (t : t) =
  match t.kind with
  | Lam (p, body) -> Option.fold ~none:false ~some:has_poly p.type_ || lambda_has_poly body
  | _ -> false

let row_of_vars vars =
  Some { effects = []; tails = List.map (fun v -> synth (Var v)) vars; inferred = false; polymorphic = false }

(* Each function type is read on its own: every parameter's type is a signature
   in its own right, and the chain's FINAL arrow - the one that actually calls
   the parameters - carries the variables they minted. A result arrow that only
   returns another function carries nothing. A final [~>] with no parameter to
   collect from mints its own variable (a standalone alias).

   ponytail: variables minted under a higher-order parameter are all bound at the
   root (rank 1); a callback that must itself be polymorphic is written with
   named variables. *)
let rec rewrite minted chain (t : t) =
  match t.kind with
  | Arrow (expl, name, dom, row, cod) ->
      let before = List.length !minted in
      let dom = rewrite minted [] dom in
      let chain = chain @ List.filteri (fun i _ -> i >= before) !minted in
      let final = match cod.kind with Arrow _ -> false | _ -> true in
      let row =
        match row with
        | Some r when r.polymorphic ->
            (* A definition's [~>]: what its body performs decides the row. *)
            if r.inferred then Some { r with polymorphic = false }
            else if not final then None
            else if chain = [] then begin
              let v = fresh_row () in
              minted := !minted @ [ v ];
              row_of_vars [ v ]
            end
            else row_of_vars chain
        | _ -> row
      in
      { t with kind = Arrow (expl, name, dom, row, rewrite minted chain cod) }
  | _ -> t

(* A type: its root is a result position; the minted variables become leading
   implicit binders. *)
let signature (t : t) =
  let minted = ref [] in
  let t = rewrite minted [] t in
  List.fold_right (fun v acc -> { acc with kind = Arrow (Explicitness.Implicit, Some v, effect_row_ty, None, acc) }) !minted t

(* A lambda: its parameters' types are parameter positions; the minted
   variables become leading implicit parameters. *)
let lambda (t : t) =
  let minted = ref [] in
  let rec params (t : t) =
    match t.kind with
    | Lam (p, body) ->
        let p = { p with type_ = Option.map (rewrite minted []) p.type_ } in
        { t with kind = Lam (p, params body) }
    | _ -> t
  in
  let t = params t in
  List.fold_right
    (fun v acc ->
      { acc with kind = Lam ({ name = v; type_ = Some effect_row_ty; trait_bounds = []; explicitness = Explicitness.Implicit }, acc) })
    !minted t
