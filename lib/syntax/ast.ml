module Id = Type.Id

module Param = struct
  type t = { name : Id.t; type_ : Type.Human.t option } [@@deriving eq]
end

let pp_type_annotated = function
  | None -> ""
  | Some typ -> Printf.sprintf ": %s " (Type.Human.pp typ)

module Atom = struct
  type t = Unit | I64 of int64 | Bool of bool [@@deriving eq]

  let pp = function
    | Unit -> "()"
    | I64 i -> Int64.to_string i
    | Bool true -> "true"
    | Bool false -> "false"
end

module rec Expr : sig
  type t =
    | Atom of Atom.t
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.Human.t }
    | Fix of t
    | Prod of t * t
  [@@deriving eq]

  val pp : t -> string
end = struct
  type t =
    | Atom of Atom.t
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Param.t * t
    | Annotated of { inner : t; typ : Type.Human.t }
    | Fix of t
    | Prod of t * t
  [@@deriving eq]

  let rec pp = function
    | Atom a -> Atom.pp a
    | Var id -> Type.Id.pp id
    | Ap (lhs, rhs) -> pp lhs ^ "(" ^ pp rhs ^ ")"
    | Let { binding; body } ->
        Printf.sprintf "%s in %s" (Binding.pp binding) (pp body)
    | If { cond; then_; else_ } ->
        Printf.sprintf "if (%s) then (%s) else (%s)" (pp cond) (pp then_)
          (pp else_)
    | Lam ({ name; type_ }, body) ->
        Printf.sprintf "fun %s%s -> (%s)" (Type.Id.pp name)
          (pp_type_annotated type_) (pp body)
    | Annotated { inner; typ } ->
        Printf.sprintf "(%s : %s)" (pp inner) (Type.Human.pp typ)
    | Fix inner -> Printf.sprintf "fix (%s)" (pp inner)
    | Prod (lhs, rhs) -> "(" ^ pp lhs ^ ", " ^ pp rhs ^ ")"
end

and Binding : sig
  type t =
    | Value of { name : Id.t; type_ : Type.Human.t option; value : Expr.t }
    | TypeDecl of {
        name : string;
        args : string list;
        rhs : (string * Type.Human.t option) Std.Nonempty_list.t;
      }
  [@@deriving eq]

  val pp : t -> string
end = struct
  type t =
    | Value of { name : Id.t; type_ : Type.Human.t option; value : Expr.t }
    | TypeDecl of {
        name : string;
        args : string list;
        rhs : (string * Type.Human.t option) Std.Nonempty_list.t;
      }
  [@@deriving eq]

  let pp = function
    | Value { name; type_; value } ->
        Printf.sprintf "let %s %s= (%s)" (Type.Id.pp name)
          (pp_type_annotated type_) (Expr.pp value)
    | TypeDecl { name; args; rhs } ->
        let ty_args =
          match args with
          | [] -> ""
          | args -> "[" ^ String.concat ", " args ^ "]"
        in
        let ty_constitutions =
          Std.Nonempty_list.(
            map
              (function
                | tag, Some ty -> tag ^ " " ^ Type.Human.pp ty
                | tag, None -> tag)
              rhs
            |> to_list)
          |> String.concat " | "
        in
        Printf.sprintf "type %s%s = %s" (Type.Id.pp name) ty_args
          ty_constitutions
end
