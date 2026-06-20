open Core

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

module Stx = struct
  open Syntax

  let fail name msg = raise (Nbe_error.EvalError (name ^ ": " ^ msg))

  let expr kind = VStx (StxExpr { kind; span = Source_span.synthetic })

  let expr_value = function VStx (StxExpr stx) -> Some stx | _ -> None

  let make_seq = function
    | [ first; second ] -> (
      match (expr_value first, expr_value second) with
      | Some first, Some second ->
        Some
          (expr
             (Let
                {
                  name = fresh_id "_";
                  type_ = None;
                  value = first;
                  body = second;
                  recursive = false;
                }))
      | _ -> None)
    | _ -> None

  let id_name = function
    | [ VStx (StxExpr { kind = Var id; _ }) ] -> Some (VAtom (String id.name))
    | [ VStx _ ] -> fail "stx_id_name" "expected identifier syntax"
    | _ -> None

  let id_eq = function
    | [ VStx (StxExpr { kind = Var a; _ }); VStx (StxExpr { kind = Var b; _ }) ] ->
        let eq =
          String.equal a.name b.name
          && (Scope_set.subset a.scope b.scope || Scope_set.subset b.scope a.scope)
        in
        Some (VAtom (Bool eq))
    | _ -> None

  let operator_symbol = function
    | [ VStx (StxExpr { kind = SyntaxOperatorUse { operator; _ }; _ }) ] ->
        Some (VAtom (String operator.name))
    | [ VStx _ ] -> fail "stx_operator_symbol" "expected syntax operator use"
    | _ -> None

  let operator_fixity = function
    | [ VStx (StxExpr { kind = SyntaxOperatorUse { fixity; _ }; _ }) ] ->
        let name = match fixity with PrefixOp -> "prefix" | InfixOp -> "infix" in
        Some (VAtom (String name))
    | [ VStx _ ] -> fail "stx_operator_fixity" "expected syntax operator use"
    | _ -> None

  let operator_arity = function
    | [ VStx (StxExpr { kind = SyntaxOperatorUse { operands; _ }; _ }) ] ->
        Some (VAtom (I64 (Int64.of_int (List.length operands))))
    | [ VStx _ ] -> fail "stx_operator_arity" "expected syntax operator use"
    | _ -> None

  let operator_operand = function
    | [ VStx (StxExpr { kind = SyntaxOperatorUse { operands; _ }; _ }); VAtom (I64 index) ] ->
        let len = List.length operands in
        if Int64.compare index 0L < 0 || Int64.compare index (Int64.of_int len) >= 0 then
          fail "stx_operator_operand" "operand index out of bounds"
        else
          let index = Int64.to_int index in
          Some (VStx (StxExpr (List.nth operands index)))
    | [ VStx _; VAtom (I64 _) ] ->
        fail "stx_operator_operand" "expected syntax operator use"
    | _ -> None
end

let stx_prim_table : (string, value list -> value option) Hashtbl.t =
  [ ("stx_make_seq", Stx.make_seq);
    ("stx_id_name", Stx.id_name);
    ("stx_id_eq", Stx.id_eq);
    ("stx_operator_symbol", Stx.operator_symbol);
    ("stx_operator_fixity", Stx.operator_fixity);
    ("stx_operator_arity", Stx.operator_arity);
    ("stx_operator_operand", Stx.operator_operand) ]
  |> List.to_seq |> Hashtbl.of_seq
