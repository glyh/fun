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

(* [alias]: does this parameter's type name a value whose own row [~>] minted -
   a type alias like [Callback = Unit ~> I64]? Its binder is rank 1, bound at the
   definition that takes the parameter, so it is minted here just as an inline
   [~>] is. A binder written out in the annotation is not one of these: writing
   it is how a rank-2 callback is asked for. *)
let rec has_poly ?(alias = fun _ -> false) (t : t) =
  match t.kind with
  | Arrow (_, _, a, row, b) -> is_poly row || alias a || has_poly ~alias a || has_poly ~alias b
  | _ -> false

let rec lambda_has_poly ?(alias = fun _ -> false) (t : t) =
  match t.kind with
  | Lam (p, body) ->
      Option.fold ~none:false ~some:(fun ty -> alias ty || has_poly ~alias ty) p.type_
      || lambda_has_poly ~alias body
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
let mint minted =
  let v = fresh_row () in
  minted := !minted @ [ v ];
  v

(* A parameter naming a [~>] alias: the alias takes its row implicitly, so the
   parameter's type is that alias at a variable minted here. *)
let at_fresh_row minted (ty : t) =
  { ty with kind = Ap (ty, Explicitness.Implicit, synth (Var (mint minted))) }

(* Two kinds of minted variable. One a parameter's type mints - an inline [~>]
   or an alias naming one - belongs to the definition that takes the parameter:
   a binder of this signature. One the type's own chain mints, with no parameter
   to collect from, has no such definition yet: the type becomes a function of
   its row, so the definition that later takes it as a parameter mints it
   (rank 1). *)
let rec rewrite ?(alias = fun _ -> false) ?(in_param = false) minted lifted chain (t : t) =
  let rewrite ?(alias = alias) = rewrite ~alias in
  match t.kind with
  | Arrow (expl, name, dom, row, cod) ->
      let before = List.length !minted in
      let dom =
        if alias dom then at_fresh_row minted dom else rewrite ~in_param:true minted lifted [] dom
      in
      let chain = chain @ List.filteri (fun i _ -> i >= before) !minted in
      let final = match cod.kind with Arrow _ -> false | _ -> true in
      let row =
        match row with
        | Some r when r.polymorphic ->
            (* A definition's [~>]: what its body performs decides the row. *)
            if r.inferred then Some { r with polymorphic = false }
            else if not final then None
            else if chain = [] then row_of_vars [ mint (if in_param then minted else lifted) ]
            else row_of_vars chain
        | _ -> row
      in
      { t with kind = Arrow (expl, name, dom, row, rewrite ~in_param minted lifted chain cod) }
  | _ -> t

(* A type: its root is a result position; the minted variables become leading
   implicit binders. *)
let implicit_row_param v =
  { name = v; type_ = Some effect_row_ty; trait_bounds = []; explicitness = Explicitness.Implicit }

let signature ?(alias = fun _ -> false) (t : t) =
  let minted = ref [] and lifted = ref [] in
  let t = rewrite ~alias minted lifted [] t in
  (* A type that minted its own row is a function of it. *)
  let t = List.fold_right (fun v acc -> { acc with kind = Lam (implicit_row_param v, acc) }) !lifted t in
  List.fold_right (fun v acc -> { acc with kind = Arrow (Explicitness.Implicit, Some v, effect_row_ty, None, acc) }) !minted t

(* A lambda: its parameters' types are parameter positions; the minted
   variables become leading implicit parameters. *)
let lambda ?(alias = fun _ -> false) (t : t) =
  let minted = ref [] and lifted = ref [] in
  let rec params (t : t) =
    match t.kind with
    | Lam (p, body) ->
        let rewrite_param ty =
          if alias ty then at_fresh_row minted ty else rewrite ~alias ~in_param:true minted lifted [] ty
        in
        let p = { p with type_ = Option.map rewrite_param p.type_ } in
        { t with kind = Lam (p, params body) }
    | _ -> t
  in
  let t = params t in
  List.fold_right (fun v acc -> { acc with kind = Lam (implicit_row_param v, acc) }) !minted t
