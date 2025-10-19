open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

let default_env =
  Type.Id.Map.of_list
    [
      ( "==",
        Value.Closure
          (fun lhs -> Closure (fun rhs -> Norm (Bool (Value.equal lhs rhs)))) );
      ( "+",
        Value.Closure
          (fun lhs ->
            Closure
              (fun rhs ->
                match ((lhs : Value.t), rhs) with
                | Norm (I64 lhs), Norm (I64 rhs) ->
                    Norm (I64 (Int64.add lhs rhs))
                | _ -> raise (Std.Exceptions.Unreachable [%here]))) );
      ( "-",
        Value.Closure
          (fun lhs ->
            Closure
              (fun rhs ->
                match ((lhs : Value.t), rhs) with
                | Norm (I64 lhs), Norm (I64 rhs) ->
                    Norm (I64 (Int64.sub lhs rhs))
                | _ -> raise (Std.Exceptions.Unreachable [%here]))) );
      ( "*",
        Value.Closure
          (fun lhs ->
            Closure
              (fun rhs ->
                match ((lhs : Value.t), rhs) with
                | Norm (I64 lhs), Norm (I64 rhs) ->
                    Norm (I64 (Int64.mul lhs rhs))
                | _ -> raise (Std.Exceptions.Unreachable [%here]))) );
    ]

(* NOTE: this function expects the program is typechecked *)
let rec eval env = function
  | Ast.Expr.Atom a -> Value.Norm a
  | Var x -> (
      match Type.Id.Map.find_opt x env with
      | None -> raise (UndefinedVariable x)
      | Some v -> v)
  | If { cond; then_; else_ } -> (
      match eval env cond with
      | Norm (Bool b) -> eval env (if b then then_ else else_)
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Ap (f, x) -> (
      match eval env f with
      | Closure f -> f (eval env x)
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Lam ({ name = param; _ }, body) ->
      Closure
        (fun arg ->
          let env = Type.Id.Map.add param arg env in
          eval env body)
  | Let { binding = { name; value; _ }; body } ->
      let v = eval env value in
      eval (Type.Id.Map.add name v env) body
  | Annotated { inner; _ } -> eval env inner
  | Fix f -> (
      (* Fix(f) = f Fix(f) *)
      match eval env f with
      | Closure f ->
          let rec fix_f arg =
            let f_on_fix_f = f (Closure fix_f) in
            match f_on_fix_f with
            | Closure f_on_fix_f -> f_on_fix_f arg
            | _ -> raise (Std.Exceptions.Unreachable [%here])
          in
          Closure fix_f
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
