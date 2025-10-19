open Model

let fn2 f = Value.Closure (fun lhs -> Closure (fun rhs -> f lhs rhs))

let i64_op op =
  fn2 (fun lhs rhs ->
      match (lhs, rhs) with
      | Norm (I64 lhs), Norm (I64 rhs) -> Norm (I64 (op lhs rhs))
      | _ -> raise (Std.Exceptions.Unreachable [%here]))

let i64_cmp op =
  fn2 (fun lhs rhs ->
      match (lhs, rhs) with
      | Norm (I64 lhs), Norm (I64 rhs) -> Norm (Bool (op lhs rhs))
      | _ -> raise (Std.Exceptions.Unreachable [%here]))

let default =
  Type.Id.Map.of_list
    [
      ( "==",
        Value.Closure
          (fun lhs -> Closure (fun rhs -> Norm (Bool (Value.equal lhs rhs)))) );
      (">", i64_cmp (fun lhs rhs -> Int64.compare lhs rhs > 0));
      ("<", i64_cmp (fun lhs rhs -> Int64.compare lhs rhs < 0));
      (">=", i64_cmp (fun lhs rhs -> Int64.compare lhs rhs >= 0));
      ("<=", i64_cmp (fun lhs rhs -> Int64.compare lhs rhs <= 0));
      ("+", i64_op Int64.add);
      ("-", i64_op Int64.sub);
      ("*", i64_op Int64.mul);
    ]
