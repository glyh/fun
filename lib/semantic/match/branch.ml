open Ppx_hash_lib.Std.Hash.Builtin

type t = Block of int [@@deriving eq, hash]
