open Ppx_hash_lib.Std.Hash.Builtin

type t = { path : path; type_ : Type.T.t [@hash.ignore] } [@@deriving hash]

and path =
  | Base
  | Project of { base : t; index : int }
  | Unwrap of t
  | Field of { base : t; name : string }

let rec equal o o' =
  match (o, o') with
  | { path = Base; _ }, { path = Base; _ } -> true
  | ( { path = Project { base = base1; index = index1 }; _ },
      { path = Project { base = base2; index = index2 }; _ } )
    when index1 == index2 ->
      equal base1 base2
  | { path = Unwrap outer1; _ }, { path = Unwrap outer2; _ } ->
      equal outer1 outer2
  | ( { path = Field { base = base1; name = name1 }; _ },
      { path = Field { base = base2; name = name2 }; _ } )
    when String.equal name1 name2 ->
      equal base1 base2
  | _ -> false
