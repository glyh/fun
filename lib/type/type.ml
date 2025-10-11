module Id = struct
  module T = struct
    type t = string [@@deriving eq, ord]
  end

  module Map = Map.Make (T)
  include T
end

module Var = struct
  module T = struct
    type t = { id : int; tag : string option } [@@deriving eq, ord]

    let show { id; tag } =
      match tag with
      | Some tag -> Printf.sprintf "%s.%d" tag id
      | None -> Printf.sprintf ".%d" id

    let var_uuid = ref 0

    let generate ?tag () =
      incr var_uuid;
      { id = !var_uuid; tag }

    let inherit_ var =
      incr var_uuid;
      { id = !var_uuid; tag = Some (show var) }
  end

  module Set = Set.Make (T)
  module Map = Map.Make (T)
  include T
end

module T = struct
  type t =
    | Forall of Var.Set.t * t
    | Var of Var.t
    | Con of string
    | Arrow of t * t
  [@@deriving eq]

  let pp _ = failwith "TODO"

  let equal t1 t2 =
    match (t1, t2) with
    | Forall (_binds1, _inner1), Forall (_binds2, _inner2) ->
        failwith "TODO: Check alpha equivalence"
    | _ -> equal t1 t2
end

module Builtin = struct
  open T

  let unit = Con "Unit"
  let int = Con "Int"
  let bool = Con "Bool"
  let char = Con "Char"
end
