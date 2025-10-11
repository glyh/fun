open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

(* NOTE: this function expects the program is type checked *)
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
      | Closure (env', param, body) ->
          let x = eval env x in
          let env'' = Type.Id.Map.add param x env' in
          eval env'' body
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Lam ({ name; _ }, body) -> Closure (env, name, body)
  | Let { binding = { recursive; name; value; _ }; body } ->
      assert (not recursive);
      let v = eval env value in
      eval (Type.Id.Map.add name v env) body
  | Annotated { inner; _ } -> eval env inner
