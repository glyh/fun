module Prim = struct
  open Atom

  (* [Stuck]: the primitive does not reduce on these atoms. [Failed]: a
     language-level runtime error, raised by the evaluator with the request's
     error (see [Nbe_support.fail]). *)
  type reduction = Reduced of t | Stuck | Failed of string
  type reducer = t list -> reduction

  (* Predicates return I64 1/0; [Bool] is a library ADT and never appears as an atom. *)
  let i64_of_bool b = I64 (if b then 1L else 0L)

  let i64_binop (f : int64 -> int64 -> int64) : reducer = function
    | [ I64 a; I64 b ] -> Reduced (I64 (f a b))
    | _ -> Stuck

  (* [/] and [%] are the only primitives that can fail on well-typed input.
     They fail like [panic] rather than letting the host's [Division_by_zero]
     escape: a division by zero is a language-level runtime error, so it must
     be reported through the language's own channel and not left for a port to
     rediscover as whatever its host happens to throw. [Stuck] is not available
     here - it would silently leave a stuck neutral term instead of failing. *)
  let i64_div (f : int64 -> int64 -> int64) : reducer = function
    | [ I64 _; I64 0L ] -> Failed "division by zero"
    | [ I64 a; I64 b ] -> Reduced (I64 (f a b))
    | _ -> Stuck

  let i64_cmp (f : int64 -> int64 -> bool) : reducer = function
    | [ I64 a; I64 b ] -> Reduced (i64_of_bool (f a b))
    | _ -> Stuck

  let char_cmp (f : char -> char -> bool) : reducer = function
    | [ Char a; Char b ] -> Reduced (i64_of_bool (f a b))
    | _ -> Stuck

  let unit_cmp (f : unit -> unit -> bool) : reducer = function
    | [ Unit; Unit ] -> Reduced (i64_of_bool (f () ()))
    | _ -> Stuck

  let string_cmp (f : string -> string -> bool) : reducer = function
    | [ String a; String b ] -> Reduced (i64_of_bool (f a b))
    | _ -> Stuck
end

let atom_ty_of_atom = function
  | Atom.I64 _ -> Atom_ty.TI64
  | Unit -> Atom_ty.TUnit
  | Char _ -> Atom_ty.TChar
  | String _ -> Atom_ty.TString
  | Scopes _ -> Atom_ty.TScopes

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
    ("ge_i64", i64_cmp (fun a b -> Int64.compare a b >= 0));
    (* The empty scope set: what an id built from a name alone carries. *)
    ("no_scopes", function [ Atom.Unit ] -> Reduced (Atom.Scopes Scope_set.empty) | _ -> Stuck) ]
  |> List.to_seq |> Hashtbl.of_seq
