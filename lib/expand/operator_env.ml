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

let find_infix symbol =
  List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) infix_table

let find_prefix symbol =
  List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) prefix_table
