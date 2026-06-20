module Prim = struct
  open Atom

  type reducer = t list -> t option

  let i64_binop (f : int64 -> int64 -> int64) : reducer = function
    | [ I64 a; I64 b ] -> Some (I64 (f a b))
    | _ -> None

  let i64_cmp (f : int64 -> int64 -> bool) : reducer = function
    | [ I64 a; I64 b ] -> Some (Bool (f a b))
    | _ -> None

  let bool_unop (f : bool -> bool) : reducer = function
    | [ Bool b ] -> Some (Bool (f b))
    | _ -> None

  let bool_cmp (f : bool -> bool -> bool) : reducer = function
    | [ Bool a; Bool b ] -> Some (Bool (f a b))
    | _ -> None

  let char_cmp (f : char -> char -> bool) : reducer = function
    | [ Char a; Char b ] -> Some (Bool (f a b))
    | _ -> None

  let unit_cmp (f : unit -> unit -> bool) : reducer = function
    | [ Unit; Unit ] -> Some (Bool (f () ()))
    | _ -> None

  let string_cmp (f : string -> string -> bool) : reducer = function
    | [ String a; String b ] -> Some (Bool (f a b))
    | _ -> None
end

let atom_ty_of_atom = function
  | Atom.I64 _ -> Atom_ty.TI64
  | Bool _ -> Atom_ty.TBool
  | Unit -> Atom_ty.TUnit
  | Char _ -> Atom_ty.TChar
  | String _ -> Atom_ty.TString

let prim_table : (string, Prim.reducer) Hashtbl.t =
  let open Prim in
  [ ("+", i64_binop Int64.add);
    ("-", i64_binop Int64.sub);
    ("*", i64_binop Int64.mul);
    ("/", i64_binop Int64.div);
    ("%", i64_binop Int64.rem);
    ("eq_i64", i64_cmp Int64.equal);
    ("neq_i64", i64_cmp (fun a b -> not (Int64.equal a b)));
    ("eq_bool", bool_cmp Bool.equal);
    ("neq_bool", bool_cmp (fun a b -> not (Bool.equal a b)));
    ("eq_char", char_cmp Char.equal);
    ("neq_char", char_cmp (fun a b -> not (Char.equal a b)));
    ("eq_unit", unit_cmp (fun () () -> true));
    ("neq_unit", unit_cmp (fun () () -> false));
    ("eq_string", string_cmp String.equal);
    ("neq_string", string_cmp (fun a b -> not (String.equal a b)));
    ("<", i64_cmp (fun a b -> Int64.compare a b < 0));
    (">", i64_cmp (fun a b -> Int64.compare a b > 0));
    ("<=", i64_cmp (fun a b -> Int64.compare a b <= 0));
    (">=", i64_cmp (fun a b -> Int64.compare a b >= 0));
    ("not", bool_unop not) ]
  |> List.to_seq |> Hashtbl.of_seq
