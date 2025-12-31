open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

(* NOTE: this backend expects the program is properly-typed *)

let rec pattern_matches (pat : Ast.Pattern.t) (v : Value.t) env =
  match (pat, v) with
  | Bind id, v -> Some (Type.Id.Map.add id v env)
  | Just expected, Atom actual when Ast.Atom.equal expected actual -> Some env
  | Prod (p1, p2), Prod (v1, v2) ->
      Option.bind (pattern_matches p1 v1 env) (pattern_matches p2 v2)
  | Tagged (tag_expected, None), Tagged { tag = tag_actual; inner = None }
    when String.equal tag_expected tag_actual ->
      Some env
  | Tagged (tag_expected, Some p), Tagged { tag = tag_actual; inner = Some v }
    when String.equal tag_expected tag_actual ->
      pattern_matches p v env
  | Union (p1, p2), v ->
      List.find_map (fun p -> pattern_matches p v env) [ p1; p2 ]
  | Any, _ -> Some env
  | _ -> None

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
  | Match { matched; branches } -> (
      let matched = eval env matched in
      let result =
        Std.Nonempty_list.to_list branches
        |> List.find_map (fun (pat, body) ->
            match pattern_matches pat matched env with
            | None -> None
            | Some env' -> Some (eval env' body))
      in
      match result with
      | None -> raise (Std.Exceptions.Unreachable [%here])
      | Some v -> v)
