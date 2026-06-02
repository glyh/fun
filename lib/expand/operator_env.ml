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

let dynamic_prefixes : prefix list ref = ref []
let dynamic_infixes : infix list ref = ref []

type snapshot = {
  prefixes : prefix list;
  infixes : infix list;
}

type export = Prefix of prefix | Infix of infix

let snapshot () = { prefixes = !dynamic_prefixes; infixes = !dynamic_infixes }

let restore snapshot =
  dynamic_prefixes := snapshot.prefixes;
  dynamic_infixes := snapshot.infixes

let without_prefix symbol prefixes =
  List.filter (fun (op : prefix) -> not (String.equal op.symbol symbol)) prefixes

let without_infix symbol infixes =
  List.filter (fun (op : infix) -> not (String.equal op.symbol symbol)) infixes

let register_prefix symbol precedence =
  dynamic_prefixes := { symbol; precedence } :: without_prefix symbol !dynamic_prefixes

let register_infix symbol precedence associativity =
  dynamic_infixes := { symbol; precedence; associativity } :: without_infix symbol !dynamic_infixes

let exports_since snapshot =
  let prefix_exports =
    !dynamic_prefixes
    |> List.filter (fun (op : prefix) ->
           not (List.exists (fun (old : prefix) -> String.equal old.symbol op.symbol) snapshot.prefixes))
    |> List.map (fun op -> Prefix op)
  in
  let infix_exports =
    !dynamic_infixes
    |> List.filter (fun (op : infix) ->
           not (List.exists (fun (old : infix) -> String.equal old.symbol op.symbol) snapshot.infixes))
    |> List.map (fun op -> Infix op)
  in
  List.rev_append prefix_exports infix_exports

let apply_export = function
  | Prefix op -> register_prefix op.symbol op.precedence
  | Infix op -> register_infix op.symbol op.precedence op.associativity

let apply_exports exports = List.iter apply_export exports

let is_dynamic_prefix symbol =
  List.exists (fun (op : prefix) -> String.equal op.symbol symbol) !dynamic_prefixes

let is_dynamic_infix symbol =
  List.exists (fun (op : infix) -> String.equal op.symbol symbol) !dynamic_infixes

let find_infix symbol =
  match List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) !dynamic_infixes with
  | Some op -> Some op
  | None -> List.find_opt (fun (op : infix) -> String.equal op.symbol symbol) infix_table

let find_prefix symbol =
  match List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) !dynamic_prefixes with
  | Some op -> Some op
  | None -> List.find_opt (fun (op : prefix) -> String.equal op.symbol symbol) prefix_table
