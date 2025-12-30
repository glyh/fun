open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

(* NOTE: this function expects the program is typechecked *)
let rec eval env = function
  | Ast.Expr.Atom a -> Value.Atom a
  | Var x -> (
      match Type.Id.Map.find_opt x env with
      | None -> raise (UndefinedVariable x)
      | Some v -> v)
  | If { cond; then_; else_ } -> (
      match eval env cond with
      | Atom (Bool b) -> eval env (if b then then_ else else_)
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
  | Let { binding = Value { name; value; _ }; body } ->
      let v = eval env value in
      eval (Type.Id.Map.add name v env) body
  | Let { binding = TypeDecl { rhs; _ }; body } ->
      let env_new =
        Std.Nonempty_list.to_list rhs
        |> List.fold_left
             (fun env (tag, type_body_opt) ->
               match type_body_opt with
               | None ->
                   Type.Id.Map.add tag (Value.Tagged { tag; inner = None }) env
               | Some _ ->
                   Type.Id.Map.add tag
                     (Value.Closure
                        (fun inner -> Value.Tagged { tag; inner = Some inner }))
                     env)
             env
      in
      eval env_new body
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
  | Prod (lhs, rhs) -> Value.Prod (eval env lhs, eval env rhs)
