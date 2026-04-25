(* Pattern matrix for match compilation (Maranget-style).
   Rows are pattern vectors (one entry per occurrence/column), each associated
   with a branch index and accumulated variable bindings. The header tracks
   which occurrence each column corresponds to. *)

module Row = struct
  include Core.Queue

  let copy_dequeue r =
    let r = copy r in
    ignore (dequeue r);
    r
end

module Col = Dynarray

module Entry = struct
  type t = Typed_ir.Pattern.t

  let default : t = { node = Any; type_ = Type.Generic.con_0 "Don't care" }
end

type binding = Type.Id.t * Occurence.t
type header = Occurence.t Row.t

type row_data = {
  entries : Entry.t Row.t;
  index : Branch.t;
  bindings : binding Dynarray.t;
}

type t = { header : header; rows : row_data Col.t }

let create header = { header; rows = Col.create () }

let add_row (m : t) entries index =
  Col.add_last m.rows { entries; index; bindings = Dynarray.create () }

let is_empty (m : t) = Col.length m.rows = 0

let get_column (m : t) i =
  let col = Dynarray.create () in
  Dynarray.iter
    (fun (rd : row_data) -> Dynarray.add_last col (Row.get rd.entries i))
    m.rows;
  col

let find_column (m : t) ~f =
  let len = Row.length m.header in
  let rec go i =
    if i >= len then None else if f (get_column m i) then Some i else go (i + 1)
  in
  go 0

let swap_columns (m : t) i j =
  if i <> j then begin
    let tmp = Row.get m.header i in
    Row.set m.header i (Row.get m.header j);
    Row.set m.header j tmp;
    Col.iter
      (fun (rd : row_data) ->
        let tmp = Row.get rd.entries i in
        Row.set rd.entries i (Row.get rd.entries j);
        Row.set rd.entries j tmp)
      m.rows
  end
