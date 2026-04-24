open Ppx_hash_lib.Std.Hash.Builtin

type t = { path : path; type_ : Type.T.t [@hash.ignore] } [@@deriving hash]

and path =
  | Base of Type.Id.t
  | Project of { base : t; index : int }
  | Unwrap of t

let rec equal o o' =
  match (o, o') with
  | { path = Base id1; _ }, { path = Base id2; _ } -> Type.Id.equal id1 id2
  | ( { path = Project { base = base1; index = index1 }; _ },
      { path = Project { base = base2; index = index2 }; _ } )
    when index1 == index2 ->
      equal base1 base2
  | { path = Unwrap outer1; _ }, { path = Unwrap outer2; _ } ->
      equal outer1 outer2
  | _ -> false
