type associativity = Left | Right

type infix = {
  symbol : string;
  precedence : int;
  associativity : associativity;
}

type prefix = { symbol : string; precedence : int }

let infix_table =
  [ { symbol = "<-"; precedence = 1; associativity = Right };
    { symbol = "=="; precedence = 5; associativity = Left };
    { symbol = "!="; precedence = 5; associativity = Left };
    { symbol = "<"; precedence = 5; associativity = Left };
    { symbol = ">"; precedence = 5; associativity = Left };
    { symbol = "<="; precedence = 5; associativity = Left };
    { symbol = ">="; precedence = 5; associativity = Left };
    { symbol = "+"; precedence = 10; associativity = Left };
    { symbol = "-"; precedence = 10; associativity = Left };
    { symbol = "*"; precedence = 20; associativity = Left };
    { symbol = "/"; precedence = 20; associativity = Left };
    { symbol = "%"; precedence = 20; associativity = Left } ]

let prefix_table = [ { symbol = "not"; precedence = 30 } ]

type t = {
  prefixes : prefix list;
  infixes : infix list;
}

type export = Prefix of prefix | Infix of infix

let empty = { prefixes = []; infixes = [] }

let without_prefix symbol prefixes =
  List.filter (fun (op : prefix) -> not (String.equal op.symbol symbol)) prefixes

let without_infix symbol infixes =
  List.filter (fun (op : infix) -> not (String.equal op.symbol symbol)) infixes

let add_prefix t symbol precedence =
  { t with prefixes = { symbol; precedence } :: without_prefix symbol t.prefixes }

let add_infix t symbol precedence associativity =
  { t with infixes = { symbol; precedence; associativity } :: without_infix symbol t.infixes }

let apply_export t = function
  | Prefix op -> add_prefix t op.symbol op.precedence
  | Infix op -> add_infix t op.symbol op.precedence op.associativity

let apply_exports t exports = List.fold_left apply_export t exports

let is_extension_prefix t symbol =
  List.exists (fun (op : prefix) -> String.equal op.symbol symbol) t.prefixes

let is_extension_infix t symbol =
  List.exists (fun (op : infix) -> String.equal op.symbol symbol) t.infixes

let find_infix t symbol =
  match List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) t.infixes with
  | Some op -> Some op
  | None -> List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) infix_table

let find_prefix t symbol =
  match List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) t.prefixes with
  | Some op -> Some op
  | None -> List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) prefix_table
