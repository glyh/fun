open Core

(** The one declaration of every primitive: its name, its type, and how it
    reduces - which states how it fails. The elaborator binds each name with its
    type; the evaluator reduces through [reducer]. Nothing else lists primitives.
    See docs/wayfinder/tickets/unify-primitive-declaration.md. *)
module Prim = struct
  open Atom

  (* [Stuck]: the primitive does not reduce on these atoms. [Failed]: a
     language-level runtime error, raised by the evaluator with the request's
     error (see [Nbe_support.fail]). *)
  type reduction = Reduced of t | Stuck | Failed of string

  type reducer =
    | Atoms of (t list -> reduction)
        (** reduces once every argument is an atom *)
    | Special
        (** reduced by the evaluator itself, from its frames ([Nbe.try_prim_reduce]):
            [panic] fails with its message, [expand_block]/[expand_decls] ask the
            running macro application, [Tuple] builds a product type *)

  (* Predicates return I64 1/0; [Bool] is a library ADT and never appears as an atom. *)
  let i64_of_bool b = I64 (if b then 1L else 0L)

  (* I64 arithmetic is checked: overflow and division by zero are language-level
     runtime errors, never the host's wrap-around or exception. *)
  let overflow op = Failed ("integer overflow in " ^ op)

  let i64_arith op (f : int64 -> int64 -> int64 option) =
    Atoms (function
      | [ I64 a; I64 b ] -> (match f a b with Some r -> Reduced (I64 r) | None -> overflow op)
      | _ -> Stuck)

  let checked_add a b =
    let r = Int64.add a b in
    if Int64.compare a 0L >= 0 = (Int64.compare b 0L >= 0) && Int64.compare r 0L >= 0 <> (Int64.compare a 0L >= 0)
    then None else Some r

  let checked_sub a b =
    let r = Int64.sub a b in
    if Int64.compare a 0L >= 0 <> (Int64.compare b 0L >= 0) && Int64.compare r 0L >= 0 <> (Int64.compare a 0L >= 0)
    then None else Some r

  let checked_mul a b =
    if Int64.equal a 0L || Int64.equal b 0L then Some 0L
    else if (Int64.equal a (-1L) && Int64.equal b Int64.min_int) || (Int64.equal b (-1L) && Int64.equal a Int64.min_int)
    then None
    else
      let r = Int64.mul a b in
      if Int64.equal (Int64.div r b) a then Some r else None

  let i64_div op (f : int64 -> int64 -> int64) =
    Atoms (function
      | [ I64 _; I64 0L ] -> Failed "division by zero"
      | [ I64 a; I64 b ] when Int64.equal a Int64.min_int && Int64.equal b (-1L) && String.equal op "/" -> overflow op
      | [ I64 a; I64 b ] -> Reduced (I64 (f a b))
      | _ -> Stuck)

  let cmp (atoms : t -> t -> bool option) =
    Atoms (function
      | [ a; b ] -> (match atoms a b with Some r -> Reduced (i64_of_bool r) | None -> Stuck)
      | _ -> Stuck)

  let i64_cmp f = cmp (fun a b -> match a, b with I64 a, I64 b -> Some (f a b) | _ -> None)
  let char_cmp f = cmp (fun a b -> match a, b with Char a, Char b -> Some (f a b) | _ -> None)
  let unit_cmp r = cmp (fun a b -> match a, b with Unit, Unit -> Some r | _ -> None)
  let string_cmp f = cmp (fun a b -> match a, b with String a, String b -> Some (f a b) | _ -> None)
end

let atom_ty_of_atom = function
  | Atom.I64 _ -> Atom_ty.TI64
  | Unit -> Atom_ty.TUnit
  | Char _ -> Atom_ty.TChar
  | String _ -> Atom_ty.TString
  | Scopes _ -> Atom_ty.TScopes

type declaration = { name : string; ty : value; reducer : Prim.reducer }

(* Primitives never mention [Bool]: all predicates return I64 (1/0). The prelude
   wraps them into the library [Bool] ADT via [i64_to_bool]. *)
let declarations : declaration list =
  let open Prim in
  let binary a r = VAtomTy a ^-> AtomTy a ^->> AtomTy r in
  let arithmetic = binary Atom_ty.TI64 Atom_ty.TI64 in
  let predicate a = binary a Atom_ty.TI64 in
  let decl name ty reducer = { name; ty; reducer } in
  (* [[A : Type] -> body]: an implicit type binder over a pure arrow. *)
  let over_type body = VPi { explicitness = Implicit; domain = VU; effects = pure_effects; codomain = { env = []; body } } in
  [ decl "+" arithmetic (i64_arith "+" checked_add);
    decl "-" arithmetic (i64_arith "-" checked_sub);
    decl "*" arithmetic (i64_arith "*" checked_mul);
    decl "/" arithmetic (i64_div "/" Int64.div);
    decl "%" arithmetic (i64_div "%" Int64.rem);
    decl "eq_i64" (predicate Atom_ty.TI64) (i64_cmp Int64.equal);
    decl "neq_i64" (predicate Atom_ty.TI64) (i64_cmp (fun a b -> not (Int64.equal a b)));
    decl "lt_i64" (predicate Atom_ty.TI64) (i64_cmp (fun a b -> Int64.compare a b < 0));
    decl "gt_i64" (predicate Atom_ty.TI64) (i64_cmp (fun a b -> Int64.compare a b > 0));
    decl "le_i64" (predicate Atom_ty.TI64) (i64_cmp (fun a b -> Int64.compare a b <= 0));
    decl "ge_i64" (predicate Atom_ty.TI64) (i64_cmp (fun a b -> Int64.compare a b >= 0));
    decl "eq_char" (predicate Atom_ty.TChar) (char_cmp Char.equal);
    decl "neq_char" (predicate Atom_ty.TChar) (char_cmp (fun a b -> not (Char.equal a b)));
    decl "eq_unit" (predicate Atom_ty.TUnit) (unit_cmp true);
    decl "neq_unit" (predicate Atom_ty.TUnit) (unit_cmp false);
    decl "eq_string" (predicate Atom_ty.TString) (string_cmp String.equal);
    decl "neq_string" (predicate Atom_ty.TString) (string_cmp (fun a b -> not (String.equal a b)));
    (* [panic[A](message) : A] *)
    decl "panic" (over_type (AtomTy Atom_ty.TString ^->> Var 1)) Special;
    (* [expand_block[Syntax.Expr]]: typed in the prelude's [Syntax] module. *)
    decl "expand_block" (over_type (Var 0 ^->> Var 1)) Special;
    decl "expand_decls" (over_type (Var 0 ^->> Var 1)) Special;
    (* [Tuple : (n : I64) -> tuple_arity(n)]: its arity is computed from [n]. *)
    decl Compiler_names.Type_name.tuple
      (VPi { explicitness = Explicit; domain = VAtomTy Atom_ty.TI64; effects = pure_effects;
             codomain = { env = []; body = Ap (Prim Compiler_names.Type_name.tuple_arity, Explicit, Var 0) } })
      Special ]

let atom_reducers : (string, Atom.t list -> Prim.reduction) Hashtbl.t =
  declarations
  |> List.filter_map (fun d -> match d.reducer with Prim.Atoms f -> Some (d.name, f) | Special -> None)
  |> List.to_seq |> Hashtbl.of_seq
