type hole_kind = Expr | Block | Binder | Ident | Decl

type pattern_part =
  | Literal of Raw_syntax.t
  | Group of Raw_syntax.delimiter * pattern_part list * Source_span.t
  | Hole of {
      name : string;
      kind : hole_kind;
      span : Source_span.t;
    }

type branch = {
  pattern : pattern_part list;
  replacement : Raw_syntax.t list;
  span : Source_span.t;
}

type captured = {
  syntax : Syntax.t;
  kind : hole_kind;
  decl_terms : Raw_syntax.t list option;
}

type t = {
  head : string;
  branches : branch list;
  declaration_span : Source_span.t;
  inherited_captures : (string * captured) list;
  (* The compilation unit the template was imported from, if it was: ids its
     replacement introduces mean that unit's names. *)
  unit : string option;
}

(* The unit whose template an instance's intro scope belongs to. Intro scopes
   are minted from one global counter, so the key is unique. *)
let intro_scope_units : (int, string) Hashtbl.t = Hashtbl.create 64
