open Parser
open Std

(* TODO: track locs in (row,col) tuple, now it's tracking byte offset *)
type loc = int * int

exception UnexpectedToken of { token : string; location : loc }
exception UnterminatedComment of { started_at : loc }

let () =
  Printexc.register_printer (function
    | UnexpectedToken { token; location = start, end_ } ->
        Some
          (Printf.sprintf "UnexpectedToken {token = `%s`, location = (%d, %d)}"
             token start end_)
    | _ -> None)

let id =
  [%sedlex.regexp?
    ( ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') )]

let rec token buf =
  match%sedlex buf with
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "fun" -> FUN
  | "true" -> TRUE
  | "false" -> FALSE
  | "type" -> TYPE
  | "->" -> ARROW
  | "()" -> UNIT
  | ";;" -> DOUBLESEMI
  | "==" -> EQ
  | ">=" -> GE
  | "<=" -> LE
  | ">" -> GT
  | "<" -> LT
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | "=" -> ASSIGN
  | ":" -> COLON
  | "+" -> ADD
  | "-" -> SUB
  | "*" -> MUL
  | "|" -> PIPE
  | "," -> COMMA
  | Plus '0' .. '9' ->
      let num = Sedlexing.Utf8.lexeme buf |> Int64.of_string in
      I64 num
  | "'", id ->
      let drop_first s = String.sub s 1 (String.length s - 1) in
      TY_VAR (Sedlexing.Utf8.lexeme buf |> drop_first)
  | id -> ID (Sedlexing.Utf8.lexeme buf)
  | Plus (' ' | '\t' | '\n' | '\r') -> token buf
  | "(*" -> skip_comment buf (Nonempty_list.init (Sedlexing.loc buf) [])
  | eof -> EOF
  | _ ->
      raise
        (UnexpectedToken
           { token = Sedlexing.Utf8.lexeme buf; location = Sedlexing.loc buf })

and skip_comment buf (openings : loc Nonempty_list.t) =
  match%sedlex buf with
  | "(*" -> skip_comment buf (Nonempty_list.cons (Sedlexing.loc buf) openings)
  | "*)" -> (
      match Nonempty_list.uncons openings with
      | _, [] -> token buf
      | _, hd :: rest -> skip_comment buf (Nonempty_list.init hd rest))
  | eof ->
      let top_opening = Nonempty_list.first openings in
      raise (UnterminatedComment { started_at = top_opening })
  | any -> skip_comment buf openings
  | _ -> raise (Exceptions.Unreachable [%here])
