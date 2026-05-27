type t = Unit | I64 of int64 | Bool of bool | Char of char | String of string [@@deriving eq]

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
  | Bool true -> "true"
  | Bool false -> "false"
  | Char c -> "'" ^ pp_char c ^ "'"
  | String s -> "\"" ^ String.escaped s ^ "\""
