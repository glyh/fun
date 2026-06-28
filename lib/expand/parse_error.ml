type kind =
  | Unexpected of { expected : string; got : string }
  | Unconsumed of string
  | Missing of string
  | Unsupported of string
  | Unmatched of string

type t = {
  kind : kind;
  span : Source_span.t;
}

type 'a result = Ok of 'a | WithErrors of 'a * t list

let pp_kind fmt = function
  | Unexpected { expected; got } ->
      Format.fprintf fmt "expected %s, got %s" expected got
  | Unconsumed first ->
      Format.fprintf fmt "unconsumed terms after expression: %s" first
  | Missing what ->
      Format.fprintf fmt "missing %s" what
  | Unsupported msg ->
      Format.fprintf fmt "unsupported: %s" msg
  | Unmatched what ->
      Format.fprintf fmt "unmatched %s" what

let pp fmt { kind; span } =
  Format.fprintf fmt "%a: %a" Source_span.pp span pp_kind kind
