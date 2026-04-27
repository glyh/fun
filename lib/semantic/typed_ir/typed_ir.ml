module Atom = Syntax.Ast.Atom

module rec Pattern : sig
  type t = { node : node; type_ : Type.T.t }

  and node =
    | Bind of Type.Id.t
    | Just of Atom.t
    | Prod of t Std.Nonempty_list.t
    | Tagged of Type.Id.t * t option
    | Union of t * t
    | Any
    | Record of { fields : (string * t option) Std.Nonempty_list.t; partial : bool }
end = struct
  type t = { node : node; type_ : Type.T.t }

  and node =
    | Bind of Type.Id.t
    | Just of Atom.t
    | Prod of t Std.Nonempty_list.t
    | Tagged of Type.Id.t * t option
    | Union of t * t
    | Any
    | Record of { fields : (string * t option) Std.Nonempty_list.t; partial : bool }
end

and Expr : sig
  type t = { node : node; type_ : Type.T.t }

  and node =
    | Atom of Atom.t
    | Var of Type.Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of { param : Type.Id.t; param_type : Type.T.t; body : t }
    | Fix of t
    | Prod of t Std.Nonempty_list.t
    | Match of {
        matched : t;
        branches : (Pattern.t * t) Std.Nonempty_list.t;
      }
    | Record of (string * t) Std.Nonempty_list.t
    | FieldAccess of t * string
    | StructDef of { members : Binding.t list; pub_names : string list }
end = struct
  type t = { node : node; type_ : Type.T.t }

  and node =
    | Atom of Atom.t
    | Var of Type.Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of { param : Type.Id.t; param_type : Type.T.t; body : t }
    | Fix of t
    | Prod of t Std.Nonempty_list.t
    | Match of {
        matched : t;
        branches : (Pattern.t * t) Std.Nonempty_list.t;
      }
    | Record of (string * t) Std.Nonempty_list.t
    | FieldAccess of t * string
    | StructDef of { members : Binding.t list; pub_names : string list }
end

and Binding : sig
  type t =
    | Value of { name : Type.Id.t; value : Expr.t }
    | TypeDecl of {
        name : string;
        args : string list;
        rhs : Syntax.Ast.type_rhs;
      }
    | Open of string
end = struct
  type t =
    | Value of { name : Type.Id.t; value : Expr.t }
    | TypeDecl of {
        name : string;
        args : string list;
        rhs : Syntax.Ast.type_rhs;
      }
    | Open of string
end

let rec map_types_pattern ~(f : Type.T.t -> Type.T.t) (p : Pattern.t) :
    Pattern.t =
  let node =
    match p.node with
    | Bind _ | Just _ | Any -> p.node
    | Prod elements ->
        Prod (Std.Nonempty_list.map (map_types_pattern ~f) elements)
    | Tagged (tag, inner) ->
        Tagged (tag, Option.map (map_types_pattern ~f) inner)
    | Union (lhs, rhs) ->
        Union (map_types_pattern ~f lhs, map_types_pattern ~f rhs)
    | Record { fields; partial } ->
        Record
          { fields =
              Std.Nonempty_list.map
                (fun (name, inner) ->
                  (name, Option.map (map_types_pattern ~f) inner))
                fields;
            partial }
  in
  { node; type_ = f p.type_ }

let rec map_types_expr ~(f : Type.T.t -> Type.T.t) (e : Expr.t) : Expr.t =
  let map_e = map_types_expr ~f in
  let map_p = map_types_pattern ~f in
  let node =
    match e.node with
    | Atom _ | Var _ -> e.node
    | Ap (fn, arg) -> Ap (map_e fn, map_e arg)
    | Let { binding; body } ->
        Let { binding = map_types_binding ~f binding; body = map_e body }
    | If { cond; then_; else_ } ->
        If { cond = map_e cond; then_ = map_e then_; else_ = map_e else_ }
    | Lam { param; param_type; body } ->
        Lam { param; param_type = f param_type; body = map_e body }
    | Fix inner -> Fix (map_e inner)
    | Prod elements -> Prod (Std.Nonempty_list.map map_e elements)
    | Match { matched; branches } ->
        Match
          {
            matched = map_e matched;
            branches =
              Std.Nonempty_list.map
                (fun (pat, body) -> (map_p pat, map_e body))
                branches;
          }
    | Record fields ->
        Record (Std.Nonempty_list.map (fun (n, e) -> (n, map_e e)) fields)
    | FieldAccess (inner, field) -> FieldAccess (map_e inner, field)
    | StructDef { members; pub_names } ->
        StructDef
          { members = List.map (map_types_binding ~f) members; pub_names }
  in
  { node; type_ = f e.type_ }

and map_types_binding ~(f : Type.T.t -> Type.T.t) (b : Binding.t) : Binding.t =
  match b with
  | Value { name; value } -> Value { name; value = map_types_expr ~f value }
  | TypeDecl _ | Open _ -> b
