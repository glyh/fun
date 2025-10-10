module Var = struct
  module T = struct
    type t = { id : int; tag : string option } [@@deriving eq, ord, hash]
  end

  module Set = Set.Make (T)
  include T
end

module T = struct
  type t =
    | Forall of Var.Set.t * t
    | Var of Var.t
    | Con of string
    | Arrow of t * t
  [@@deriving eq]
end

module Builtin = struct
  open T

  let unit = Con "Unit"
  let int = Con "Int"
  let bool = Con "Bool"
  let char = Con "Char"
end

module Env = struct
  module Inner = Hashtbl.Make (Var)

  type t = { map : T.t Inner.t; mutable next : int }

  let create () = { map = Inner.create 32; next = 0 }

  let generate (env : t) ?tag () =
    let id = env.next in
    env.next <- env.next + 1;
    Var.{ id; tag }

  let find (env : t) (id : Var.t) = Inner.find_opt env.map id
end
