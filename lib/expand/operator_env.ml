type associativity = Left | Right

type fixity = Prefix | Infix

type expansion = BuiltinApply | BuiltinRefSet | Macro | Template of Syntax_template.t

type operator = {
  symbol : string;
  fixity : fixity;
  precedence : int;
  associativity : associativity;
  syntax_class : Syntax_class.t;
  expansion : expansion;
  declaration_span : Source_span.t;
}

type prefix = operator
type infix = operator
type export = operator

let operator ~syntax_class ~declaration_span ~symbol ~fixity ~precedence ~associativity ~expansion =
  { symbol; fixity; precedence; associativity; syntax_class; expansion; declaration_span }

let builtin_prefix symbol precedence =
  operator ~syntax_class:Syntax_class.Expr ~declaration_span:Source_span.synthetic ~symbol ~fixity:Prefix ~precedence ~associativity:Left ~expansion:BuiltinApply

let builtin_infix symbol precedence associativity =
  operator ~syntax_class:Syntax_class.Expr ~declaration_span:Source_span.synthetic ~symbol ~fixity:Infix ~precedence ~associativity ~expansion:BuiltinApply

let builtin_ref_set symbol precedence associativity =
  operator ~syntax_class:Syntax_class.Expr ~declaration_span:Source_span.synthetic ~symbol ~fixity:Infix ~precedence ~associativity ~expansion:BuiltinRefSet

let macro_prefix ?(declaration_span = Source_span.synthetic) symbol precedence =
  operator ~syntax_class:Syntax_class.Expr ~declaration_span ~symbol ~fixity:Prefix ~precedence ~associativity:Left ~expansion:Macro

let template_prefix ?(declaration_span = Source_span.synthetic) symbol template precedence =
  operator ~syntax_class:Syntax_class.Expr ~declaration_span ~symbol ~fixity:Prefix ~precedence ~associativity:Left ~expansion:(Template template)

let macro_infix ?(declaration_span = Source_span.synthetic) symbol precedence associativity =
  operator ~syntax_class:Syntax_class.Expr ~declaration_span ~symbol ~fixity:Infix ~precedence ~associativity ~expansion:Macro

let infix_table =
  [ builtin_ref_set "<-" 1 Right;
    builtin_infix "==" 5 Left;
    builtin_infix "!=" 5 Left;
    builtin_infix "<" 5 Left;
    builtin_infix ">" 5 Left;
    builtin_infix "<=" 5 Left;
    builtin_infix ">=" 5 Left;
    builtin_infix "+" 10 Left;
    builtin_infix "-" 10 Left;
    builtin_infix "*" 20 Left;
    builtin_infix "/" 20 Left;
    builtin_infix "%" 20 Left ]

let prefix_table = [ builtin_prefix "not" 30 ]

type t = {
  prefixes : prefix list;
  infixes : infix list;
}

let empty = { prefixes = []; infixes = [] }

let same_operator_key a b =
  String.equal a.symbol b.symbol && a.fixity = b.fixity && a.syntax_class = b.syntax_class

let without_operator op ops = List.filter (fun existing -> not (same_operator_key existing op)) ops

let add_prefix ?declaration_span t symbol precedence =
  let op = macro_prefix ?declaration_span symbol precedence in
  { t with prefixes = op :: without_operator op t.prefixes }

let add_template_prefix ?declaration_span t symbol template precedence =
  let op = template_prefix ?declaration_span symbol template precedence in
  { t with prefixes = op :: without_operator op t.prefixes }

let add_infix ?declaration_span t symbol precedence associativity =
  let op = macro_infix ?declaration_span symbol precedence associativity in
  { t with infixes = op :: without_operator op t.infixes }

let add_operator t op =
  match op.fixity with
  | Prefix -> { t with prefixes = op :: without_operator op t.prefixes }
  | Infix -> { t with infixes = op :: without_operator op t.infixes }

let apply_export t op = add_operator t op

let apply_exports t exports = List.fold_left apply_export t exports

let find_infix ~syntax_class t symbol =
  let matches op = String.equal op.symbol symbol && op.syntax_class = syntax_class in
  match List.find_opt matches t.infixes with
  | Some op -> Some op
  | None -> List.find_opt matches infix_table

let find_prefix ~syntax_class t symbol =
  let matches op = String.equal op.symbol symbol && op.syntax_class = syntax_class in
  match List.find_opt matches t.prefixes with
  | Some op -> Some op
  | None -> List.find_opt matches prefix_table

let fixity_name = function Prefix -> "prefix" | Infix -> "infix"

let duplicate_exports_message exports =
  let rec go seen = function
    | [] -> None
    | op :: rest -> (
        match List.find_opt (same_operator_key op) seen with
        | Some previous ->
            Some
              (Printf.sprintf
                 "ambiguous syntax extension candidates for %s %s operator %S: declarations at %s and %s"
                 (fixity_name op.fixity)
                 (Syntax_class.to_string op.syntax_class)
                 op.symbol
                 (Format.asprintf "%a" Source_span.pp previous.declaration_span)
                 (Format.asprintf "%a" Source_span.pp op.declaration_span))
        | None -> go (op :: seen) rest)
  in
  go [] exports
