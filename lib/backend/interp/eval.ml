open Model
open Syntax

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
end

open Exceptions

let nth_nonempty (xs : 'a Std.Nonempty_list.t) i =
  List.nth_opt (Std.Nonempty_list.to_list xs) i

let rec resolve_occurrence (root : Value.t) (o : Match.Occurence.t) :
    Value.t option =
  match o.path with
  | Match.Occurence.Base -> Some root
  | Project { base; _ } | Unwrap base | Field { base; _ } -> (
      let%bind.Core.Option base = resolve_occurrence root base in
      match (base, o.path) with
      | Prod elements, Project { index; _ } -> nth_nonempty elements index
      | Tagged { inner = Some inner; _ }, Unwrap _ -> Some inner
      | Record fields, Field { name; _ } -> List.assoc_opt name fields
      | _ -> None)

let extend_env_with_bindings env root bindings =
  List.fold_left
    (fun env (name, occ) ->
      let resolved =
        match resolve_occurrence root occ with
        | Some resolved -> resolved
        | None -> raise (Std.Exceptions.Unreachable [%here])
      in
      Type.Id.Map.add name resolved env)
    env bindings

let branch_body_exn branches = function
  | Match.Branch.Block i -> (
      match List.nth_opt branches i with
      | Some (_, body) -> body
      | None -> raise (Std.Exceptions.Unreachable [%here]))

let rec eval_tree ~type_defs env (root : Value.t) branches
    (tree : Match.Decision_tree.t) : Value.t =
  match tree.content with
  | Leaf { branch; bindings } ->
      let body = branch_body_exn branches branch in
      let env' = extend_env_with_bindings env root bindings in
      eval ~type_defs env' body
  | Destruct { occurence; cases; default } ->
      let next =
        match resolve_occurrence root occurence with
        | Some (Tagged { tag; _ }) -> (
            match List.assoc_opt tag cases with
            | Some tree -> tree
            | None -> default)
        | _ -> default
      in
      eval_tree ~type_defs env root branches next
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
      eval_tree ~type_defs env root branches next

and eval ~type_defs env (e : Typed_ir.Expr.t) =
  match e.node with
  | Atom a -> Value.Atom a
  | Var x -> (
      match Type.Id.Map.find_opt x env with
      | None -> raise (UndefinedVariable x)
      | Some v -> v)
  | If { cond; then_; else_ } -> (
      match eval ~type_defs env cond with
      | Atom (Bool b) -> eval ~type_defs env (if b then then_ else else_)
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Ap (f, x) -> (
      match eval ~type_defs env f with
      | Closure f ->
          let x_val = eval ~type_defs env x in
          f x_val
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | Lam { param; body; _ } ->
      Closure
        (fun arg ->
          let env = Type.Id.Map.add param arg env in
          eval ~type_defs env body)
  | Let { binding = Value { name; value }; body } ->
      let v = eval ~type_defs env value in
      eval ~type_defs (Type.Id.Map.add name v env) body
  | Let { binding = TypeDecl { rhs; _ }; body } -> (
      match rhs with
      | Adt ctors ->
          let env_new =
            Std.Nonempty_list.to_list ctors
            |> List.fold_left
                 (fun env (tag, type_body_opt) ->
                   match type_body_opt with
                   | None ->
                       Type.Id.Map.add tag
                         (Value.Tagged { tag; inner = None })
                         env
                   | Some _ ->
                       Type.Id.Map.add tag
                         (Value.Closure
                            (fun inner ->
                              Value.Tagged { tag; inner = Some inner }))
                         env)
                 env
          in
          eval ~type_defs env_new body
      | Record _ -> eval ~type_defs env body)
  | Let { binding = Open name; body } ->
      let struct_val = Type.Id.Map.find name env in
      let members = match struct_val with
        | Value.Struct fields -> fields
        | _ -> raise (Std.Exceptions.Unreachable [%here])
      in
      let env' = List.fold_left (fun env (n, v) -> Type.Id.Map.add n v env) env members in
      eval ~type_defs env' body
  | Fix f -> (
      match eval ~type_defs env f with
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
      Value.Prod (Std.Nonempty_list.map (eval ~type_defs env) elements)
  | Record fields ->
      Value.Record
        (Std.Nonempty_list.to_list fields
        |> List.map (fun (name, e) -> (name, eval ~type_defs env e)))
  | FieldAccess (inner, field) -> (
      match eval ~type_defs env inner with
      | (Record fields | Struct fields) -> (
          match List.assoc_opt field fields with
          | Some v -> v
          | None -> raise (Std.Exceptions.Unreachable [%here]))
      | _ -> raise (Std.Exceptions.Unreachable [%here]))
  | StructDef { members; pub_names } ->
      let env' =
        List.fold_left
          (fun env (b : Typed_ir.Binding.t) ->
            match b with
            | Value { name; value } ->
                let v = eval ~type_defs env value in
                Type.Id.Map.add name v env
            | TypeDecl { rhs = Adt ctors; _ } ->
                Std.Nonempty_list.to_list ctors
                |> List.fold_left
                     (fun env (tag, type_body_opt) ->
                       match type_body_opt with
                       | None ->
                           Type.Id.Map.add tag
                             (Value.Tagged { tag; inner = None })
                             env
                       | Some _ ->
                           Type.Id.Map.add tag
                             (Value.Closure
                                (fun inner ->
                                  Value.Tagged { tag; inner = Some inner }))
                             env)
                     env
            | TypeDecl { rhs = Record _; _ } -> env
            | Open name ->
                let struct_val = Type.Id.Map.find name env in
                let members = match struct_val with
                  | Value.Struct fields -> fields
                  | _ -> raise (Std.Exceptions.Unreachable [%here])
                in
                List.fold_left (fun env (n, v) -> Type.Id.Map.add n v env) env members)
          env members
      in
      let pub_values =
        List.filter_map
          (fun name ->
            match Type.Id.Map.find_opt name env' with
            | Some v -> Some (name, v)
            | None -> None)
          pub_names
      in
      Value.Struct pub_values
  | Match { matched; branches } ->
      let matched_value = eval ~type_defs env matched in
      let branch_list = Std.Nonempty_list.to_list branches in
      let pattern_and_branches =
        List.mapi (fun i (pat, _) -> (pat, Match.Branch.Block i)) branch_list
      in
      let tree =
        Match.Match_compile.compile ~type_defs matched
          pattern_and_branches
      in
      eval_tree ~type_defs env matched_value branch_list tree
