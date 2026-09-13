include Elab_error

(* In a recursive record's field types, rewrite each use of the record -
   applied to exactly its own parameters - to [SelfType]. Expansion gave every
   local binder a fresh resolved name, so an occurrence spelled [record_name]
   is the record: nothing needs tracking for shadowing. A use applied to
   anything else, or not applied to all its parameters, is rejected. *)
let rewrite_record_self_refs record_name params (expr : Syntax.t) : Syntax.t =
  let invalid () =
    raise
      (ElabError
         (InvalidRecursiveRecord
            ("recursive record references must be same-instantiation uses of " ^ record_name)))
  in
  let rec spine acc (e : Syntax.t) =
    match e.kind with
    | Syntax.Ap (f, Explicitness.Explicit, a) -> spine (a :: acc) f
    | _ -> (e, acc)
  in
  let is_record (e : Syntax.t) = match e.kind with Syntax.Var id -> String.equal id.name record_name | _ -> false in
  let is_param param (arg : Syntax.t) = match arg.kind with Syntax.Var id -> String.equal id.name param | _ -> false in
  let self_ref (form : Syntax.t) =
    match spine [] form with
    | head, args when is_record head && List.length args = List.length params ->
        if List.for_all2 is_param params args then { form with kind = Syntax.SelfType } else invalid ()
    | _ -> form
  in
  let rewritten = Expand.map_forms Fun.id self_ref expr in
  ignore (Expand.map_forms Fun.id (fun form -> if is_record form then invalid () else form) rewritten);
  rewritten

let rec trait_bound_names (expr : Syntax.t) =
  match expr.kind with
  | Syntax.Ap ({ kind = Syntax.Ap (plus, Explicitness.Explicit, lhs); _ }, Explicitness.Explicit, rhs)
    when Syntax.written_name plus = Some "+" -> (
      match trait_bound_names lhs, trait_bound_names rhs with
      | Some lhs, Some rhs -> Some (lhs @ rhs)
      | _ -> None)
  | _ -> Option.map (fun name -> [ name ]) (Syntax.written_name expr)
