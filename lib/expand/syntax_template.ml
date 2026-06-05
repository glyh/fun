type hole_kind = Expr | Binder | Ident

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
}

type t = {
  head : string;
  branches : branch list;
  declaration_span : Source_span.t;
  inherited_captures : (string * captured) list;
}
