(* [Scopes] is a reflected id's scope set: it has no literal syntax and no
   primitives, so a macro can move one but never make or inspect one (M11). It
   also carries the resolved name its id was minted with, if any: the only
   certificate under which a reflected id may carry a resolved name (M12). *)
type t = Unit | I64 of int64 | Char of char | String of string | Scopes of Scope_set.t * string option [@@deriving eq]

let pp_char = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | c -> String.make 1 c

let pp = function
  | Unit -> "()"
  | I64 i -> Int64.to_string i
  | Char c -> "'" ^ pp_char c ^ "'"
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Scopes (s, _) -> Format.asprintf "<scopes %a>" Scope_set.pp s
