module Id = Type.Id

type field_accessor = string [@@deriving eq]

module rec Expr : sig
  type t =
    | Atom of Ast.Atom.t
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Ast.Param.t * t
    | Annotated of { inner : t; typ : Type.Human.t }
    | Fix of t
    | Prod of t Std.Nonempty_list.t
    | Match of { matched : t; branches : (Ast.Pattern.t * t) Std.Nonempty_list.t }
    | RecordConstruct of {
        path : string list;
        name : string;
        fields : (field_accessor * t) Std.Nonempty_list.t;
      }
    | FieldAccess of t * field_accessor
    | StructDef of {
        args : string list;
        body : Ast.struct_body;
        members : Struct_def.t list;
      }
    | Import of string
  [@@deriving eq]
end = struct
  type t =
    | Atom of Ast.Atom.t
    | Var of Id.t
    | Ap of t * t
    | Let of { binding : Binding.t; body : t }
    | If of { cond : t; then_ : t; else_ : t }
    | Lam of Ast.Param.t * t
    | Annotated of { inner : t; typ : Type.Human.t }
    | Fix of t
    | Prod of t Std.Nonempty_list.t
    | Match of { matched : t; branches : (Ast.Pattern.t * t) Std.Nonempty_list.t }
    | RecordConstruct of {
        path : string list;
        name : string;
        fields : (field_accessor * t) Std.Nonempty_list.t;
      }
    | FieldAccess of t * field_accessor
    | StructDef of {
        args : string list;
        body : Ast.struct_body;
        members : Struct_def.t list;
      }
    | Import of string
  [@@deriving eq]
end

and Binding : sig
  type t =
    | Value of { name : Id.t; type_ : Type.Human.t option; value : Expr.t }
    | Open of string
    | Export of string
  [@@deriving eq]
end = struct
  type t =
    | Value of { name : Id.t; type_ : Type.Human.t option; value : Expr.t }
    | Open of string
    | Export of string
  [@@deriving eq]
end

and Struct_def : sig
  type t = { vis : Ast.visibility; binding : Binding.t } [@@deriving eq]
end = struct
  type t = { vis : Ast.visibility; binding : Binding.t } [@@deriving eq]
end
