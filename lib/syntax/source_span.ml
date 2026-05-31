type t = {
  file : string option;
  start_byte : int;
  end_byte : int;
  start_line : int option;
  start_col : int option;
  end_line : int option;
  end_col : int option;
  synthetic : bool;
}

let synthetic =
  { file = None; start_byte = 0; end_byte = 0;
    start_line = None; start_col = None;
    end_line = None; end_col = None;
    synthetic = true }

let pp fmt sp =
  if sp.synthetic then
    Format.fprintf fmt "<synthetic>"
  else
    let file = match sp.file with Some f -> f | None -> "<unknown>" in
    match sp.start_line, sp.start_col, sp.end_line, sp.end_col with
    | Some sl, Some sc, Some el, Some ec ->
      Format.fprintf fmt "%s:%d:%d-%d:%d" file sl sc el ec
    | _ ->
      Format.fprintf fmt "%s:%d-%d" file sp.start_byte sp.end_byte

let make ?file ?start_line ?start_col ?end_line ?end_col ~start_byte ~end_byte () =
  { file; start_byte; end_byte; start_line; start_col; end_line; end_col; synthetic = false }

let of_lexing_positions ?file (start_pos : Lexing.position) (end_pos : Lexing.position) =
  let file = match file with Some _ -> file | None ->
    if String.equal start_pos.pos_fname "" then None else Some start_pos.pos_fname
  in
  let col pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  { file;
    start_byte = start_pos.pos_cnum;
    end_byte = end_pos.pos_cnum;
    start_line = Some start_pos.pos_lnum;
    start_col = Some (col start_pos);
    end_line = Some end_pos.pos_lnum;
    end_col = Some (col end_pos);
    synthetic = false }
