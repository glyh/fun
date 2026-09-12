module Prim = struct
  open Atom

  type reducer = t list -> t option

  (* Predicates return I64 1/0; [Bool] is a library ADT and never appears as an atom. *)
  let i64_of_bool b = I64 (if b then 1L else 0L)

  let i64_binop (f : int64 -> int64 -> int64) : reducer = function
    | [ I64 a; I64 b ] -> Some (I64 (f a b))
    | _ -> None

  (* [/] and [%] are the only primitives that can fail on well-typed input.
     They raise the same [EvalError] as [panic] rather than letting the host's
     [Division_by_zero] escape: a division by zero is a language-level runtime
     error, so it must be reported through the language's own channel and not
     left for a port to rediscover as whatever its host happens to throw.
     [None] is not available here - it means "does not reduce", which would
     silently leave a stuck neutral term instead of failing. *)
  let i64_div (f : int64 -> int64 -> int64) : reducer = function
    | [ I64 _; I64 0L ] -> raise (Nbe_error.EvalError "division by zero")
    | [ I64 a; I64 b ] -> Some (I64 (f a b))
    | _ -> None

  let i64_cmp (f : int64 -> int64 -> bool) : reducer = function
    | [ I64 a; I64 b ] -> Some (i64_of_bool (f a b))
    | _ -> None

  let char_cmp (f : char -> char -> bool) : reducer = function
    | [ Char a; Char b ] -> Some (i64_of_bool (f a b))
    | _ -> None

  let unit_cmp (f : unit -> unit -> bool) : reducer = function
    | [ Unit; Unit ] -> Some (i64_of_bool (f () ()))
    | _ -> None

  let string_cmp (f : string -> string -> bool) : reducer = function
    | [ String a; String b ] -> Some (i64_of_bool (f a b))
    | _ -> None
end

let atom_ty_of_atom = function
  | Atom.I64 _ -> Atom_ty.TI64
  | Unit -> Atom_ty.TUnit
  | Char _ -> Atom_ty.TChar
  | String _ -> Atom_ty.TString

let prim_table : (string, Prim.reducer) Hashtbl.t =
  let open Prim in
  [ ("+", i64_binop Int64.add);
    ("-", i64_binop Int64.sub);
    ("*", i64_binop Int64.mul);
    ("/", i64_div Int64.div);
    ("%", i64_div Int64.rem);
    ("eq_i64", i64_cmp Int64.equal);
    ("neq_i64", i64_cmp (fun a b -> not (Int64.equal a b)));
    ("eq_char", char_cmp Char.equal);
    ("neq_char", char_cmp (fun a b -> not (Char.equal a b)));
    ("eq_unit", unit_cmp (fun () () -> true));
    ("neq_unit", unit_cmp (fun () () -> false));
    ("eq_string", string_cmp String.equal);
    ("neq_string", string_cmp (fun a b -> not (String.equal a b)));
    ("lt_i64", i64_cmp (fun a b -> Int64.compare a b < 0));
    ("gt_i64", i64_cmp (fun a b -> Int64.compare a b > 0));
    ("le_i64", i64_cmp (fun a b -> Int64.compare a b <= 0));
    ("ge_i64", i64_cmp (fun a b -> Int64.compare a b >= 0)) ]
  |> List.to_seq |> Hashtbl.of_seq
