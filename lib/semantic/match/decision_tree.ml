open Ppx_hash_lib.Std.Hash.Builtin

type t = { id : int; content : content } [@@deriving hash]

and content =
  | Leaf of { branch : Branch.t; bindings : (Type.Id.t * Occurence.t) list }
  | Destruct of {
      occurence : Occurence.t;
      cases : (Type.Id.t * t) list;
      default : t;
    }
  | Switch of {
      occurence : Occurence.t;
      cases : (Syntax.Ast.Atom.t * t) list;
      default : t;
    }
[@@deriving hash]

let rec equal lhs rhs =
  match (lhs, rhs) with
  | { id = id1; _ }, { id = id2; _ } when id1 = id2 -> true
  | ( { content = Leaf { branch = b1; bindings = bs1 }; _ },
      { content = Leaf { branch = b2; bindings = bs2 }; _ } ) ->
      Branch.equal b1 b2
      && List.equal
           (fun (n1, o1) (n2, o2) ->
             String.equal n1 n2 && Occurence.equal o1 o2)
           bs1 bs2
  | ( { content = Destruct { occurence = o1; cases = c1; default = d1 }; _ },
      { content = Destruct { occurence = o2; cases = c2; default = d2 }; _ } )
    ->
      Occurence.equal o1 o2
      && List.equal
           (fun (tag1, t1) (tag2, t2) -> String.equal tag1 tag2 && equal t1 t2)
           c1 c2
      && equal d1 d2
  | ( { content = Switch { occurence = o1; cases = c1; default = d1 }; _ },
      { content = Switch { occurence = o2; cases = c2; default = d2 }; _ } ) ->
      Occurence.equal o1 o2
      && List.equal
           (fun (a1, t1) (a2, t2) -> Syntax.Ast.Atom.equal a1 a2 && equal t1 t2)
           c1 c2
      && equal d1 d2
  | _ -> false

(* Hash-consing functor: structurally identical subtrees share the same node,
   turning the decision tree into a DAG. Each [Make ()] instance has its own
   cache and id counter. *)
module Make () = struct
  module TT = Hashtbl.Make (struct
    type nonrec t = t

    let hash = hash
    let equal = equal
  end)

  let cache : t TT.t = TT.create 100
  let c = ref (-1)

  let get t =
    let c' = !c + 1 in
    match TT.find_opt cache { content = t; id = c' } with
    | Some tr -> tr
    | _ ->
        let entry = { content = t; id = c' } in
        TT.add cache entry entry;
        incr c;
        entry
end
