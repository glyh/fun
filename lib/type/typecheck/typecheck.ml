open Syntax.Ast

(* REF: https://cs3110.github.io/textbook/chapters/interp/inference.html *)

module TypeEnv = struct
  type t = Type.T.t Type.Id.Map.t

  let default =
    Type.Id.Map.of_list
      Type.Generic.
        [
          ( "==",
            Type.T.of_human
              (Forall
                 ([ "x" ], Arrow (Var "x", Arrow (Var "x", Con ("Bool", [])))))
          );
          (">", Arrow (i64, Arrow (i64, bool)));
          (">=", Arrow (i64, Arrow (i64, bool)));
          ("<", Arrow (i64, Arrow (i64, bool)));
          ("<=", Arrow (i64, Arrow (i64, bool)));
          ("+", Arrow (i64, Arrow (i64, i64)));
          ("-", Arrow (i64, Arrow (i64, i64)));
          ("*", Arrow (i64, Arrow (i64, i64)));
        ]

  let pp env =
    Type.Id.Map.to_list env
    |> List.map (fun (id, ty) ->
        Printf.sprintf "%s: %s" (Type.Id.pp id) (Type.T.pp ty))
    |> String.concat ", " |> Printf.sprintf "{ %s } "
end

module Constraint = struct
  type t = { lhs : Type.T.t; rhs : Type.T.t }

  let pp { lhs; rhs } =
    Printf.sprintf "[%s = %s]" (Type.T.pp lhs) (Type.T.pp rhs)
end

module StrSet = Set.Make (String)

module Exceptions = struct
  exception UndefinedVariable of Type.Id.t
  exception UnificationFailure of Constraint.t

  exception
    EscapedTypeVarInTypeDefinition of {
      name : string;
      tag : string;
      free_variables : StrSet.t;
    }

  let () =
    Printexc.register_printer (function
      | UnificationFailure c ->
          Some (Printf.sprintf "UnificationFailure (%s)" (Constraint.pp c))
      | _ -> None)
end

open Exceptions

module FreeVariables = struct
  let rec of_type : Type.T.t -> Type.Var.Set.t = function
    | Forall (vars, inner) -> Type.Var.Set.diff (of_type inner) vars
    | Var v -> Type.Var.Set.singleton v
    | Arrow (lhs, rhs) | Prod (lhs, rhs) ->
        Type.Var.Set.union (of_type lhs) (of_type rhs)
    | Con (_, inners) ->
        List.map of_type inners
        |> List.fold_left Type.Var.Set.union Type.Var.Set.empty

  let rec of_type_human : Type.Human.t -> StrSet.t = function
    | Forall (vars, inner) ->
        StrSet.diff (of_type_human inner) (StrSet.of_list vars)
    | Var v -> StrSet.singleton v
    | Arrow (lhs, rhs) | Prod (lhs, rhs) ->
        StrSet.union (of_type_human lhs) (of_type_human rhs)
    | Con (_, inners) ->
        List.map of_type_human inners
        |> List.fold_left StrSet.union StrSet.empty

  let of_env (env : TypeEnv.t) : Type.Var.Set.t =
    Type.Id.Map.fold
      (fun _ v acc -> Type.Var.Set.union acc (of_type v))
      env Type.Var.Set.empty
end

module Substitution = struct
  type t = Type.T.t Type.Var.Map.t

  let empty = Type.Var.Map.empty

  let pp sub =
    Type.Var.Map.to_list sub
    |> List.map (fun (var, ty) ->
        Printf.sprintf "%s -> %s" (Type.Var.pp var) (Type.T.pp ty))
    |> String.concat ", " |> Printf.sprintf "{ %s } "

  let rec on_type ~(sub : t) (ty : Type.T.t) =
    let recurse = on_type ~sub in
    match ty with
    | Type.Generic.Forall (vs, inner) -> Type.Generic.Forall (vs, recurse inner)
    | Var v ->
        Type.Var.Map.find_opt v sub
        |> Option.value ~default:(Type.Generic.Var v)
    | Arrow (lhs, rhs) -> Arrow (recurse lhs, recurse rhs)
    | Prod (lhs, rhs) -> Prod (recurse lhs, recurse rhs)
    | Con (tag, inner) -> Con (tag, List.map recurse inner)

  let on_sub ~(src : t) (dest : t) : t =
    Type.Var.Map.map (on_type ~sub:src) dest

  let on_constraints ~(sub : t) (cs : Constraint.t list) =
    List.map
      (fun Constraint.{ lhs; rhs } ->
        Constraint.{ lhs = on_type ~sub lhs; rhs = on_type ~sub rhs })
      cs

  let on_env ~(sub : t) (env : TypeEnv.t) = Type.Id.Map.map (on_type ~sub) env
end

module Unification = struct
  let one (c : Constraint.t) : Substitution.t option * Constraint.t list =
    match c with
    | { lhs; rhs } when Type.T.equal lhs rhs -> (None, [])
    | { lhs = Forall _; rhs = Forall _ } ->
        failwith
          "Should not unify two forall types directly as we don't support \
           rank2 types, yet."
    | { lhs = Var v; rhs = ty } | { lhs = ty; rhs = Var v } ->
        let fvs_ty = FreeVariables.of_type ty in
        if Type.Var.Set.mem v fvs_ty then raise (UnificationFailure c)
        else (Some (Type.Var.Map.singleton v ty), [])
    | { lhs = Arrow (l1, l2); rhs = Arrow (r1, r2) } ->
        (None, [ { lhs = l1; rhs = r1 }; { lhs = l2; rhs = r2 } ])
    | { lhs = Con (name1, args1); rhs = Con (name2, args2) }
      when String.equal name1 name2 -> (
        try
          (None, List.map2 (fun lhs rhs -> Constraint.{ lhs; rhs }) args1 args2)
        with Invalid_argument _ -> raise (UnificationFailure c))
    | _ -> raise (UnificationFailure c)

  let many (cs : Constraint.t list) : Substitution.t =
    let rec many_do cs subs =
      match cs with
      | [] -> subs
      | c :: cs_rest ->
          let sub_opt, new_constraints = one c in
          let cs_next, result_next =
            match sub_opt with
            | None -> (cs_rest @ new_constraints, subs)
            | Some sub ->
                ( Substitution.on_constraints ~sub cs_rest @ new_constraints,
                  sub :: subs )
          in
          many_do cs_next result_next
    in
    let rec collect_subs_do result subs =
      match subs with
      | [] -> result
      | sub :: subs_rest ->
          let subs_rest_fixed =
            List.map (Substitution.on_sub ~src:sub) subs_rest
          in
          let result =
            Type.Var.Map.union
              (fun _ _ _ -> raise (Std.Exceptions.Unreachable [%here]))
              result sub
          in
          collect_subs_do result subs_rest_fixed
    in

    many_do cs [] |> collect_subs_do Type.Var.Map.empty
end

let generalize (cs : Constraint.t list) (env : TypeEnv.t) (var : Type.Id.t)
    (ty : Type.T.t) =
  let sub_solved = Unification.many cs in
  let env_new = Substitution.on_env ~sub:sub_solved env in
  let fv_env_new = FreeVariables.of_env env_new in
  let ty_new = Substitution.on_type ~sub:sub_solved ty in
  let fv_ty_new = FreeVariables.of_type ty_new in
  let vars_to_generalize = Type.Var.Set.diff fv_ty_new fv_env_new in
  let ty_generalized = Type.Generic.Forall (vars_to_generalize, ty_new) in
  Type.Id.Map.add var ty_generalized env_new

let instantiate = function
  | Type.Generic.Forall (vars, inner) ->
      let sub =
        Type.Var.Set.to_seq vars
        |> Seq.map (fun v -> (v, Type.Generic.Var (Type.Var.inherit_ v)))
        |> Type.Var.Map.of_seq
      in
      Substitution.on_type ~sub inner
  | t -> t

module Inference = struct
  let rec generate_constraints (env : TypeEnv.t) (e : Expr.t) :
      Type.T.t * Constraint.t list =
    match e with
    | Atom Unit -> (Type.Generic.unit, [])
    | Atom (I64 _) -> (Type.Generic.i64, [])
    | Atom (Bool _) -> (Type.Generic.bool, [])
    | Var id -> (
        match Type.Id.Map.find_opt id env with
        | None -> raise (UndefinedVariable id)
        | Some scheme -> (instantiate scheme, []))
    | Ap (f, x) ->
        let f_ty, f_cons = generate_constraints env f in
        let x_ty, x_cons = generate_constraints env x in
        let result_ty = Type.Generic.Var (Type.Var.generate ()) in
        let all_cons =
          Constraint.{ lhs = f_ty; rhs = Type.Generic.Arrow (x_ty, result_ty) }
          :: f_cons
          @ x_cons
        in
        (result_ty, all_cons)
    | Let { binding = Value { name; type_ = value_ty_annotated; value }; body }
      ->
        let value_ty_inferred, value_cons = generate_constraints env value in
        let all_cons =
          match value_ty_annotated with
          | None -> value_cons
          | Some value_ty_annotated ->
              {
                lhs = Type.T.of_human value_ty_annotated;
                rhs = value_ty_inferred;
              }
              :: value_cons
        in
        let env_generalized = generalize all_cons env name value_ty_inferred in
        generate_constraints env_generalized body
    | Let { binding = TypeDecl { name; args; rhs }; body } ->
        let tycon_fvs = StrSet.of_list args in
        let adt_type =
          Type.Generic.(Con (name, args |> List.map (fun arg -> Var arg)))
        in
        let env_new =
          Std.Nonempty_list.to_list rhs
          |> List.fold_left
               (fun env (tag, type_body_opt) ->
                 let tag_type =
                   match type_body_opt with
                   | Some type_body ->
                       let referred_fvs =
                         FreeVariables.of_type_human type_body
                       in
                       if not @@ StrSet.subset referred_fvs tycon_fvs then
                         raise
                           (EscapedTypeVarInTypeDefinition
                              {
                                name;
                                tag;
                                free_variables =
                                  StrSet.diff referred_fvs tycon_fvs;
                              });
                       Type.Generic.(Forall (args, Arrow (type_body, adt_type)))
                   | None -> Forall (args, adt_type)
                 in
                 Type.Id.Map.add tag (Type.T.of_human tag_type) env)
               env
        in
        generate_constraints env_new body
    | If { cond; then_; else_ } ->
        let cond_ty, cond_cons = generate_constraints env cond in
        let then_ty, then_cons = generate_constraints env then_ in
        let else_ty, else_cons = generate_constraints env else_ in
        ( then_ty,
          Constraint.{ lhs = cond_ty; rhs = Type.Generic.bool }
          :: { lhs = then_ty; rhs = else_ty }
          :: cond_cons
          @ then_cons @ else_cons )
    | Lam (param, body) ->
        assert (param.type_ = None);
        let var_gen = Type.Generic.Var (Type.Var.generate ()) in
        let env_new = Type.Id.Map.add param.name var_gen env in
        let body_type, constraints = generate_constraints env_new body in
        (Arrow (var_gen, body_type), constraints)
    | Annotated { inner; typ = annotated } ->
        let inferred, cons = generate_constraints env inner in
        ( inferred,
          Constraint.{ lhs = inferred; rhs = Type.T.of_human annotated } :: cons
        )
    | Fix inner ->
        (* fix: (a -> b) -> b *)
        let a = Type.Generic.Var (Type.Var.generate ()) in
        let b = Type.Generic.Var (Type.Var.generate ()) in
        let inner_ty, cons = generate_constraints env inner in
        (b, Constraint.{ lhs = Arrow (a, b); rhs = inner_ty } :: cons)

  let on_expr (env : TypeEnv.t) (exp : Expr.t) : Type.T.t =
    let exp_ty, cons = generate_constraints env exp in
    let sub = Unification.many cons in
    let type_sub = Substitution.on_type ~sub exp_ty in
    let rest_fvs = FreeVariables.of_type type_sub in
    if Type.Var.Set.is_empty rest_fvs then type_sub
    else Forall (rest_fvs, type_sub)
end
