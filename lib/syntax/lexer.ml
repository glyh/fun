open Parser
open Std

(* TODO: track locs in (row,col) tuple, now it's tracking byte offset *)
type loc = int * int

exception UnexpectedToken of { token : string; location : loc }
exception UnterminatedComment of { started_at : loc }

let rec token buf =
  match%sedlex buf with
  | "let" -> LET
  | "rec" -> REC
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "fun" -> FUN
  | "->" -> ARROW
  | "()" -> UNIT
  | ";;" -> DOUBLESEMI
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "=" -> ASSIGN
  | ":" -> COLON
  | Plus '0' .. '9' ->
      let num = Sedlexing.Utf8.lexeme buf |> int_of_string in
      INT num
  | ( ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') ) ->
      ID (Sedlexing.Utf8.lexeme buf)
  | Plus (' ' | '\t' | '\n' | '\r') -> token buf
  | "(*" -> skip_comment buf (Sedlexing.loc buf, [])
  | eof -> EOF
  | _ ->
      raise
        (UnexpectedToken
           { token = Sedlexing.Utf8.lexeme buf; location = Sedlexing.loc buf })

and skip_comment buf (openings : loc Nonempty_list.t) =
  match%sedlex buf with
  | "(*" -> skip_comment buf (Nonempty_list.cons (Sedlexing.loc buf) openings)
  | "*)" -> (
      match openings with
      | _, [] -> token buf
      | _, hd :: rest -> skip_comment buf (hd, rest))
  | eof ->
      let top_opening = Nonempty_list.first openings in
      raise (UnterminatedComment { started_at = top_opening })
  | _ -> skip_comment buf openings
