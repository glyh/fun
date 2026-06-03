type associativity = Left | Right

type fixity = Prefix | Infix

type expansion = Builtin | Macro

type operator = {
  symbol : string;
  fixity : fixity;
  precedence : int;
  associativity : associativity;
  syntax_class : Syntax_class.t;
  expansion : expansion;
}

type prefix = operator
type infix = operator
type export = operator

let operator ~symbol ~fixity ~precedence ~associativity ~expansion =
  { symbol; fixity; precedence; associativity; syntax_class = Syntax_class.Expr; expansion }

let builtin_prefix symbol precedence =
  operator ~symbol ~fixity:Prefix ~precedence ~associativity:Left ~expansion:Builtin

let builtin_infix symbol precedence associativity =
  operator ~symbol ~fixity:Infix ~precedence ~associativity ~expansion:Builtin

let macro_prefix symbol precedence =
  operator ~symbol ~fixity:Prefix ~precedence ~associativity:Left ~expansion:Macro

let macro_infix symbol precedence associativity =
  operator ~symbol ~fixity:Infix ~precedence ~associativity ~expansion:Macro

let infix_table =
  [ builtin_infix "<-" 1 Right;
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

let without_prefix symbol prefixes =
  List.filter (fun (op : prefix) -> not (String.equal op.symbol symbol)) prefixes

let without_infix symbol infixes =
  List.filter (fun (op : infix) -> not (String.equal op.symbol symbol)) infixes

let add_prefix t symbol precedence =
  { t with prefixes = macro_prefix symbol precedence :: without_prefix symbol t.prefixes }

let add_infix t symbol precedence associativity =
  { t with infixes = macro_infix symbol precedence associativity :: without_infix symbol t.infixes }

let add_operator t op =
  match op.fixity with
  | Prefix -> { t with prefixes = op :: without_prefix op.symbol t.prefixes }
  | Infix -> { t with infixes = op :: without_infix op.symbol t.infixes }

let apply_export t op = add_operator t op

let apply_exports t exports = List.fold_left apply_export t exports

let is_extension_prefix t symbol =
  List.exists (fun (op : prefix) -> String.equal op.symbol symbol && op.expansion = Macro) t.prefixes

let is_extension_infix t symbol =
  List.exists (fun (op : infix) -> String.equal op.symbol symbol && op.expansion = Macro) t.infixes

let find_infix t symbol =
  match List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) t.infixes with
  | Some op -> Some op
  | None -> List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) infix_table

let find_prefix t symbol =
  match List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) t.prefixes with
  | Some op -> Some op
  | None -> List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) prefix_table
