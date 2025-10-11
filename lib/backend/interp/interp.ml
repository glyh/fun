open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

let rec eval env = function
  | Ast.Expr.Atom a -> Value.Norm a
  | Var x -> (
      match Type.Id.Map.find_opt x env with
      | None -> raise (UndefinedVariable x)
      | Some v -> v)
  | If { cond; then_; else_ } -> (
      match eval env cond with
      | VNum 0 -> eval env else_
      | VNum _ -> eval env then_
      | _ -> failwith "condition must be an integer")
  | Ap (f, arg) -> (
      match eval env f with
      | VClosure (env', param, body) ->
          let v = eval env arg in
          let env'' = Env.add param v env' in
          eval env'' body
      | _ -> failwith "application to non-function")
  | Lam ((param, _typ), body) -> VClosure (env, param, body)
  | Let { binding = { recursive; name; value; _ }; body } ->
      if recursive then (
        let rec_env = ref Env.empty in
        let v = VClosure (!rec_env, name, value) in
        rec_env := Env.add name v env;
        eval !rec_env body)
      else
        let v = eval env value in
        eval (Env.add name v env) body
  | Annotated { inner; _ } -> eval env inner
