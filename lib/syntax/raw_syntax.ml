type delimiter = Paren | Brace

type token_kind =
  | Keyword of string
  | Ident of string
  | Int of int64
  | Char of char
  | String of string
  | Unit
  | Punct of string
  | Operator of string
  | Eof

type token = {
  kind : token_kind;
  span : Source_span.t;
}

type datum =
  | Token of token
  | Group of delimiter * t list * Source_span.t

and t = {
  datum : datum;
  span : Source_span.t;
}

let token kind span = { kind; span }

let syntax_token kind span = { datum = Token (token kind span); span }

let group delimiter items span = { datum = Group (delimiter, items, span); span }
