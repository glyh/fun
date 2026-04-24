open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

(* NOTE: this backend expects the program is properly-typed *)

type context = { arities : (Type.Id.t * int) list }

let default_context = { arities = [] }

let arity_of (ctx : context) (type_name : Type.Id.t) =
  match List.assoc_opt type_name ctx.arities with
  | Some count -> count
  | None -> raise (Std.Exceptions.Unreachable [%here])

let with_type_decl (ctx : context) (name : Type.Id.t)
    (rhs : (Type.Id.t * Type.Human.t option) Std.Nonempty_list.t) =
  {
    arities =
      (name, List.length (Std.Nonempty_list.to_list rhs)) :: ctx.arities;
  }

let nth_nonempty (xs : 'a Std.Nonempty_list.t) i =
  List.nth_opt (Std.Nonempty_list.to_list xs) i

let rec resolve_occurrence (root : Value.t) (o : Match.Occurence.t) : Value.t option =
  match o.path with
  | Match.Occurence.Base _ -> Some root
  | Project { base; index } ->
      Option.bind (resolve_occurrence root base) (fun v ->
          match v with
          | Prod elements -> nth_nonempty elements index
          | _ -> None)
  | Unwrap outer ->
      Option.bind (resolve_occurrence root outer) (fun v ->
          match v with
          | Tagged { inner = Some inner; _ } -> Some inner
          | _ -> None)

let extend_env_with_bindings env root bindings =
  List.fold_left
    (fun acc (name, occ) ->
      Option.bind acc (fun env' ->
          Option.map
            (fun v -> Type.Id.Map.add name v env')
            (resolve_occurrence root occ)))
    (Some env)
    bindings

let branch_body_exn branches = function
  | Match.Branch.Block i -> (
      match List.nth_opt branches i with
      | Some (_, body) -> body
      | None -> raise (Std.Exceptions.Unreachable [%here]))

let rec eval_tree (ctx : context) env (root : Value.t) branches
    (tree : Match.Decision_tree.t) : Value.t =
  match tree.content with
  | Leaf { branch; bindings } ->
      let body = branch_body_exn branches branch in
      let env' =
        match extend_env_with_bindings env root bindings with
        | Some env' -> env'
        | None -> raise (Std.Exceptions.Unreachable [%here])
      in
      eval_with_context ctx env' body
  | Destruct { occurence; cases; default } ->
      let next =
        match resolve_occurrence root occurence with
        | Some (Tagged { tag; _ }) -> (
            match List.assoc_opt tag cases with
            | Some tree -> Some tree
            | None -> default)
        | _ -> default
      in
      (match next with
       | Some tree -> eval_tree ctx env root branches tree
       | None -> raise (Std.Exceptions.Unreachable [%here]))
  | Switch { occurence; cases; default } ->
      let next =
        match resolve_occurrence root occurence with
        | Some (Atom atom) -> (
            match
              List.find_map
                (fun (expected, tree) ->
                  if Ast.Atom.equal expected atom then Some tree else None)
                cases
            with
            | Some tree -> tree
            | None -> default)
        | _ -> default
      in
      eval_tree ctx env root branches next

and eval_with_context (ctx : context) env (e : Typed_ir.Expr.t) =
  match e.node with
  | Atom a -> Value.Atom a
  | Var x -> (
      match Type.Id.Map.find_opt x env with
      | None -> raise (UndefinedVariable x)
      | Some v -> v)
  | If { cond; then_; else_ } -> (
      match eval_with_context ctx env cond with
      | Atom (Bool b) -> eval_with_context ctx env (if b then then_ else else_)
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Ap (f, x) -> (
      match eval_with_context ctx env f with
      | Closure f -> f (eval_with_context ctx env x)
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Lam { param; body; _ } ->
      Closure
        (fun arg ->
          let env = Type.Id.Map.add param arg env in
          eval_with_context ctx env body)
  | Let { binding = Value { name; value }; body } ->
      let v = eval_with_context ctx env value in
      eval_with_context ctx (Type.Id.Map.add name v env) body
  | Let { binding = TypeDecl { name; rhs; _ }; body } ->
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
      let ctx_new = with_type_decl ctx name rhs in
      eval_with_context ctx_new env_new body
  | Fix f -> (
      (* Fix(f) = f Fix(f) *)
      match eval_with_context ctx env f with
      | Closure f ->
          let rec fix_f arg =
            let f_on_fix_f = f (Closure fix_f) in
            match f_on_fix_f with
            | Closure f_on_fix_f -> f_on_fix_f arg
            | _ -> raise (Std.Exceptions.Unreachable [%here])
          in
          Closure fix_f
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Prod elements ->
      Value.Prod (Std.Nonempty_list.map (eval_with_context ctx env) elements)
  | Match { matched; branches } ->
      let matched_value = eval_with_context ctx env matched in
      let branch_list = Std.Nonempty_list.to_list branches in
      let patterns = List.map fst branch_list in
      let branch_ids = List.mapi (fun i _ -> Match.Branch.Block i) branch_list in
      let base =
        {
          Match.Occurence.path = Match.Occurence.Base "__match";
          type_ = matched.type_;
        }
      in
      let tree =
        Match.Match_compile.compile ~arities:(arity_of ctx) base branch_ids patterns
      in
      eval_tree ctx env matched_value branch_list tree

and eval env (e : Typed_ir.Expr.t) =
  eval_with_context default_context env e
