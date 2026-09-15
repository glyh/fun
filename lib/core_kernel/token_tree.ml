(* The reader's output: tokens and delimiter groups, each token carrying its
   scope set. In the kernel so unparsed bodies can sit inside [Syntax.t]. *)

type delimiter = Paren | Bracket | Brace [@@deriving show]

type token_kind =
  | Ident of string
  | Int of int64
  | Char of char
  | String of string
  | Unit
  | KwLet
  | KwFun
  | KwThen
  | KwSig
  | KwFn
  | KwDo
  | KwEnd
  | KwElse
  | KwMatch
  | KwWith
  | KwEffect
  | KwModule
  | KwStruct
  | KwEnum
  | KwImpl
  | KwTrait
  | KwPub
  | KwImport
  | KwOpen
  | KwMacro
  | KwPattern
  | KwSelf
  | KwSelfType
  | KwRef
  | KwDeref
  | KwRec
  | KwCan
  | KwPerform
  | KwResume
  | KwMethod
  | KwUnit
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Colon
  | Equals
  | Semi
  | Bar
  | ThinArrow
  | DatumComment
  | Operator of string
  | Eof
[@@deriving show]

(* [scope] is the token's scope set: the reader gives every token the empty
   set; enforestation adds the scopes of the definition contexts, template
   instances and binders around it before the token is read as a form, so a
   syntactic role is resolved against the token's scopes (M7). *)
type token = {
  kind : token_kind;
  span : Source_span.t;
  scope : Scope_set.t; [@opaque]
}
[@@deriving show]

type datum =
  | Token of token
  | Group of delimiter * t list * Source_span.t
[@@deriving show]

and t = {
  datum : datum;
  span : Source_span.t;
}
[@@deriving show]

let token ?(scope = Scope_set.empty) kind span = { kind; span; scope }

let syntax_token ?scope kind span = { datum = Token (token ?scope kind span); span }

(* Add [s] to the scope set of every token in [terms], inside groups too. *)
let rec add_scope (s : Scope_set.t) (terms : t list) : t list =
  List.map
    (fun term ->
      match term.datum with
      | Token tok -> { term with datum = Token { tok with scope = Scope_set.union tok.scope s } }
      | Group (d, items, span) -> { term with datum = Group (d, add_scope s items, span) })
    terms

let group delimiter items span = { datum = Group (delimiter, items, span); span }


(* Every keyword and punctuation token by its spelling, both ways: reflection
   carries these tokens as their spelling (M9). *)
let keyword_spellings =
  [ (KwLet, "let"); (KwFun, "fun"); (KwThen, "then"); (KwSig, "sig"); (KwFn, "fn"); (KwDo, "do");
    (KwEnd, "end"); (KwElse, "else"); (KwMatch, "match"); (KwWith, "with"); (KwEffect, "effect");
    (KwModule, "module"); (KwStruct, "struct"); (KwEnum, "enum"); (KwImpl, "impl"); (KwTrait, "trait");
    (KwPub, "pub"); (KwImport, "import"); (KwOpen, "open"); (KwMacro, "macro"); (KwPattern, "pattern");
    (KwSelf, "self"); (KwSelfType, "Self"); (KwRef, "ref"); (KwDeref, "deref"); (KwRec, "rec");
    (KwCan, "can"); (KwPerform, "perform"); (KwResume, "resume"); (KwMethod, "method"); (KwUnit, "Unit") ]

let punct_spellings =
  [ (LParen, "("); (RParen, ")"); (LBracket, "["); (RBracket, "]"); (LBrace, "{"); (RBrace, "}");
    (Comma, ","); (Dot, "."); (Colon, ":"); (Equals, "="); (Semi, ";"); (Bar, "|"); (ThinArrow, "->");
    (DatumComment, "#_"); (Eof, "EOF") ]

let spelling_of table kind = List.assoc_opt kind table

let of_spelling table s = Option.map fst (List.find_opt (fun (_, t) -> String.equal t s) table)
