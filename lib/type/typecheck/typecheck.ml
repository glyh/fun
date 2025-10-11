open Syntax.Ast

(* REF: https://cs3110.github.io/textbook/chapters/interp/inference.html *)

exception UndefinedVariable of Type.Id.t

type type_env = Type.T.t Type.Id.Map.t
type type_constraint = Type.T.t * Type.T.t

exception UnificationFailure of type_constraint

module FreeVariables = struct
  let rec of_type : Type.T.t -> Type.Var.Set.t = function
    | Forall (vars, inner) -> Type.Var.Set.diff (of_type inner) vars
    | Var v -> Type.Var.Set.singleton v
    | Arrow (lhs, rhs) -> Type.Var.Set.union (of_type lhs) (of_type rhs)
    | Type.T.Con _ -> Type.Var.Set.empty

  let of_env (env : type_env) : Type.Var.Set.t =
    Type.Id.Map.fold
      (fun _ v acc -> Type.Var.Set.union acc (of_type v))
      env Type.Var.Set.empty
end

module Substitution = struct
  type t = Type.T.t Type.Var.Map.t

  let empty = Type.Var.Map.empty

  let rec on_type ~(sub : t) (ty : Type.T.t) =
    let recurse = on_type ~sub in
    match ty with
    | Type.T.Forall (vs, inner) -> Type.T.Forall (vs, recurse inner)
    | Var v ->
        Type.Var.Map.find_opt v sub |> Option.value ~default:(Type.T.Var v)
    | Arrow (lhs, rhs) -> Arrow (recurse lhs, recurse rhs)
    | Con _ as default -> default

  let on_sub ~(sub : t) ~(target : t) : t =
    target |> Type.Var.Map.map (on_type ~sub)

  let on_constraints ~(sub : t) (cs : type_constraint list) =
    cs |> List.map (fun (tlhs, trhs) -> (on_type ~sub tlhs, on_type ~sub trhs))

  let on_env ~(sub : t) (env : type_env) = Type.Id.Map.map (on_type ~sub) env
end

module Unification = struct
  let rec one (c : type_constraint) :
      (Type.Var.t * Type.T.t) option * type_constraint list =
    match c with
    | lhs, rhs when Type.T.equal lhs rhs -> (None, [])
    | Forall (vs1, inner1), Forall (vs2, inner2) ->
        if Type.Var.Set.equal vs1 vs2 then one (inner1, inner2)
        else raise (UnificationFailure c)
    | Var v, ty | ty, Var v ->
        let fvs_rhs = FreeVariables.of_type ty in
        if Type.Var.Set.mem v fvs_rhs then raise (UnificationFailure c)
        else (Some (v, ty), [])
    | Arrow (l1, l2), Arrow (r1, r2) -> (None, [ (l1, r1); (l2, r2) ])
    | _ -> raise (UnificationFailure c)

  let rec many (cs : type_constraint list) : Substitution.t =
    match cs with
    | [] -> Substitution.empty
    | c :: cs -> (
        match one c with
        | Some (s_v, s_t), cs_new ->
            Type.Var.Map.add s_v s_t
              (many
                 (cs_new
                 @ Substitution.on_constraints
                     ~sub:(Type.Var.Map.singleton s_v s_t)
                     cs))
        | None, cs_new -> many (cs_new @ cs))
end

let generalize (cs : type_constraint list) (env : type_env) (var : Type.Id.t)
    (ty : Type.T.t) =
  let sub_solved = Unification.many cs in
  let env_new = Substitution.on_env ~sub:sub_solved env in
  let fv_env_new = FreeVariables.of_env env_new in
  let ty_new = Substitution.on_type ~sub:sub_solved ty in
  let fv_ty_new = FreeVariables.of_type ty_new in
  let vars_to_generalize = Type.Var.Set.diff fv_ty_new fv_env_new in
  let ty_generalized = Type.T.Forall (vars_to_generalize, ty_new) in
  Type.Id.Map.add var ty_generalized env_new

let instantiate = function
  | Type.T.Forall (vars, inner) ->
      let sub =
        Type.Var.Set.to_seq vars
        |> Seq.map (fun v -> (v, Type.T.Var (Type.Var.inherit_ v)))
        |> Type.Var.Map.of_seq
      in
      Substitution.on_type ~sub inner
  | t -> t

module Inference = struct
  let rec on_constraints (env : type_env) (e : Expr.t) :
      Type.T.t * type_constraint list =
    match e with
    | Atom Unit -> (Type.Builtin.unit, [])
    | Atom (I64 _) -> (Type.Builtin.i64, [])
    | Var id -> (
        match Type.Id.Map.find_opt id env with
        | None -> raise (UndefinedVariable id)
        | Some scheme -> (instantiate scheme, []))
    | Ap (f, x) ->
        let f_ty, f_cons = on_constraints env f in
        let x_ty, x_cons = on_constraints env x in
        let result_ty = Type.T.Var (Type.Var.generate ()) in
        (result_ty, [ (f_ty, Type.T.Arrow (x_ty, result_ty)) ] @ f_cons @ x_cons)
    | Expr.Let { binding = { recursive; name; type_; value }; body } ->
        assert (recursive = false);
        assert (type_ = None);
        let value_ty, value_cons = on_constraints env value in
        let env_generalized = generalize value_cons env name value_ty in
        on_constraints env_generalized body
    | Expr.If { cond; then_; else_ } ->
        let cond_ty, cond_cons = on_constraints env cond in
        let then_ty, then_cons = on_constraints env then_ in
        let else_ty, else_cons = on_constraints env else_ in
        ( then_ty,
          [ (cond_ty, Type.Builtin.bool); (then_ty, else_ty) ]
          @ cond_cons @ then_cons @ else_cons )
    | Expr.Lam (param, body) ->
        assert (param.type_ = None);
        let var_gen = Type.T.Var (Type.Var.generate ()) in
        let env_new = Type.Id.Map.add param.name var_gen env in
        let body_type, constraints = on_constraints env_new body in
        (Arrow (var_gen, body_type), constraints)
    | Expr.Annotated _ -> failwith "TODO: implement type annotation"

  let on_type (env : type_env) (exp : Expr.t) : Type.T.t =
    let exp_ty, cons = on_constraints env exp in
    let sub = Unification.many cons in
    let type_sub = Substitution.on_type ~sub exp_ty in
    let rest_fvs = FreeVariables.of_type type_sub in
    if Type.Var.Set.is_empty rest_fvs then type_sub
    else Forall (rest_fvs, type_sub)
end
