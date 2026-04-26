open Syntax.Ast

(* REF: https://cs3110.github.io/textbook/chapters/interp/inference.html *)

module TypeEnv = struct
  type t = Type.T.t Type.Id.Map.t

  let default =
    Type.Id.Map.of_list
      Type.Generic.
        [
          ("==", Type.T.of_human ([ "x" ] => Var "x" ^-> Var "x" ^-> bool));
          (">", i64 ^-> i64 ^-> bool);
          (">=", i64 ^-> i64 ^-> bool);
          ("<", i64 ^-> i64 ^-> bool);
          ("<=", i64 ^-> i64 ^-> bool);
          ("+", i64 ^-> i64 ^-> i64);
          ("-", i64 ^-> i64 ^-> i64);
          ("*", i64 ^-> i64 ^-> i64);
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

  exception
    DuplicatedBindingInPatternProd of { pat : Pattern.t; binding : string }

  exception NoSuchTypeConstructor of { pat : Pattern.t; tag : string }
  exception BindingMismatchOnUnion of { pat1 : Pattern.t; pat2 : Pattern.t }
  exception UnknownRecordField of string
  exception NoSuchRecordType of string

  exception
    IncompleteRecordPattern of {
      record_type : string;
      missing : string list;
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
    | Arrow (lhs, rhs) -> Type.Var.Set.union (of_type lhs) (of_type rhs)
    | Prod elements ->
        Std.Nonempty_list.to_list elements
        |> List.map of_type
        |> List.fold_left Type.Var.Set.union Type.Var.Set.empty
    | Con (_, inners) ->
        List.map of_type inners
        |> List.fold_left Type.Var.Set.union Type.Var.Set.empty

  let rec of_type_human : Type.Human.t -> StrSet.t = function
    | Forall (vars, inner) ->
        StrSet.diff (of_type_human inner) (StrSet.of_list vars)
    | Var v -> StrSet.singleton v
    | Arrow (lhs, rhs) -> StrSet.union (of_type_human lhs) (of_type_human rhs)
    | Prod elements ->
        Std.Nonempty_list.to_list elements
        |> List.map of_type_human
        |> List.fold_left StrSet.union StrSet.empty
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
    let open Type.Generic in
    let recurse = on_type ~sub in
    match ty with
    | Forall (vs, inner) -> vs => recurse inner
    | Var v -> Type.Var.Map.find_opt v sub |> Option.value ~default:(Var v)
    | Arrow (lhs, rhs) -> recurse lhs ^-> recurse rhs
    | Prod elements -> Prod (Std.Nonempty_list.map recurse elements)
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

module RecordDefs = struct
  type record_def = {
    params : Type.Var.Set.t;
    fields : (string * Type.T.t) Std.Nonempty_list.t;
  }

  type t = {
    by_name : record_def Type.Id.Map.t;
    by_field : Type.Id.t Type.Id.Map.t;
  }

  let empty = { by_name = Type.Id.Map.empty; by_field = Type.Id.Map.empty }

  let register name (args : string list)
      (fields : (string * Type.Human.t) Std.Nonempty_list.t) t =
    let field_types = Std.Nonempty_list.map snd fields in
    let bundle : Type.Human.t =
      match args with
      | [] -> Prod field_types
      | _ -> Forall (args, Prod field_types)
    in
    let converted = Type.T.of_human bundle in
    let params, field_type_list =
      match converted with
      | Forall (var_set, Prod elems) ->
          (var_set, Std.Nonempty_list.to_list elems)
      | Prod elems ->
          (Type.Var.Set.empty, Std.Nonempty_list.to_list elems)
      | _ -> raise (Std.Exceptions.Unreachable [%here])
    in
    let field_names = Std.Nonempty_list.to_list fields |> List.map fst in
    let converted_fields =
      match List.combine field_names field_type_list with
      | hd :: rest -> Std.Nonempty_list.init hd rest
      | [] -> raise (Std.Exceptions.Unreachable [%here])
    in
    let by_name =
      Type.Id.Map.add name { params; fields = converted_fields } t.by_name
    in
    let by_field =
      Std.Nonempty_list.to_list fields
      |> List.fold_left
           (fun m (field_name, _) -> Type.Id.Map.add field_name name m)
           t.by_field
    in
    { by_name; by_field }

  let find_by_field field t = Type.Id.Map.find_opt field t.by_field
  let find_by_name name t = Type.Id.Map.find_opt name t.by_name

  let instantiate_def def =
    let sub =
      Type.Var.Set.fold
        (fun v acc ->
          Type.Var.Map.add v (Type.Generic.Var (Type.Var.generate ())) acc)
        def.params Type.Var.Map.empty
    in
    let fresh_args =
      Type.Var.Set.to_seq def.params
      |> Seq.map (fun v -> Type.Var.Map.find v sub)
      |> List.of_seq
    in
    let lookup_field name =
      match List.assoc_opt name (Std.Nonempty_list.to_list def.fields) with
      | Some ty -> Substitution.on_type ~sub ty
      | None -> raise (UnknownRecordField name)
    in
    (fresh_args, lookup_field)
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
    | { lhs = Prod elems1; rhs = Prod elems2 } -> (
        try
          ( None,
            List.map2
              (fun lhs rhs -> Constraint.{ lhs; rhs })
              (Std.Nonempty_list.to_list elems1)
              (Std.Nonempty_list.to_list elems2) )
        with Invalid_argument _ -> raise (UnificationFailure c))
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
  (sub_solved, Type.Id.Map.add var ty_generalized env_new)

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
  let of_atom = function
    | Atom.Unit -> Type.Generic.unit
    | I64 _ -> Type.Generic.i64
    | Bool _ -> Type.Generic.bool

  let rec constraints_from_pattern (env : TypeEnv.t)
      (record_defs : RecordDefs.t) (pat : Pattern.t) :
      Typed_ir.Pattern.t * TypeEnv.t * Constraint.t list =
    let open Type.Generic in
    let gen_var () = Var (Type.Var.generate ()) in
    let mk node type_ : Typed_ir.Pattern.t = { node; type_ } in
    match pat with
    | Bind id ->
        let id_ty = gen_var () in
        (mk (Bind id) id_ty, Type.Id.Map.of_list [ (id, id_ty) ], [])
    | Just a -> (mk (Just a) (of_atom a), Type.Id.Map.empty, [])
    | Any -> (mk Any (gen_var ()), Type.Id.Map.empty, [])
    | Prod elements ->
        let prod_pat_union =
          Type.Id.Map.union (fun binding _ _ ->
              raise (DuplicatedBindingInPatternProd { pat; binding }))
        in
        let mapped =
          Std.Nonempty_list.map
            (constraints_from_pattern env record_defs)
            elements
        in
        let typed_pats = Std.Nonempty_list.map (fun (tp, _, _) -> tp) mapped in
        let tys =
          Std.Nonempty_list.map
            (fun ((tp : Typed_ir.Pattern.t), _, _) -> tp.type_)
            mapped
        in
        let extras =
          Std.Nonempty_list.to_list mapped
          |> List.map (fun (_, ext, _) -> ext)
          |> List.fold_left prod_pat_union Type.Id.Map.empty
        in
        let cons =
          Std.Nonempty_list.to_list mapped
          |> List.map (fun (_, _, cons) -> cons)
          |> List.concat
        in
        (mk (Prod typed_pats) (Prod tys), extras, cons)
    | Tagged (tag, inner) -> (
        let tag_ty =
          match Type.Id.Map.find_opt tag env with
          | None -> raise (NoSuchTypeConstructor { tag; pat })
          | Some tag_ty -> tag_ty
        in
        match inner with
        | None -> (mk (Tagged (tag, None)) tag_ty, Type.Id.Map.empty, [])
        | Some inner ->
            let typed_inner, inner_extras, inner_cons =
              constraints_from_pattern env record_defs inner
            in
            let outer_ty = gen_var () in
            ( mk (Tagged (tag, Some typed_inner)) outer_ty,
              inner_extras,
              Constraint.{ lhs = tag_ty; rhs = typed_inner.type_ ^-> outer_ty }
              :: inner_cons ))
    | Union (lhs, rhs) -> (
        let typed_lhs, lhs_extras, lhs_cons =
          constraints_from_pattern env record_defs lhs
        in
        let typed_rhs, rhs_extras, rhs_cons =
          constraints_from_pattern env record_defs rhs
        in
        let sort_bindings bindings =
          Type.Id.Map.to_list bindings
          |> List.sort (fun (lhs_key, _) (rhs_key, _) ->
              String.compare lhs_key rhs_key)
        in
        let lhs_bindings = sort_bindings lhs_extras in
        let rhs_bindings = sort_bindings rhs_extras in
        let exn_to_throw = BindingMismatchOnUnion { pat1 = lhs; pat2 = rhs } in
        try
          let both_bindings = List.combine lhs_bindings rhs_bindings in
          let cons_on_bindings =
            List.map
              (fun ((lhs_name, lhs_ty), (rhs_name, rhs_ty)) ->
                if String.equal lhs_name rhs_name then
                  Constraint.{ lhs = lhs_ty; rhs = rhs_ty }
                else raise exn_to_throw)
              both_bindings
          in
          ( mk (Union (typed_lhs, typed_rhs)) typed_lhs.type_,
            lhs_extras,
            Constraint.{ lhs = typed_lhs.type_; rhs = typed_rhs.type_ }
            :: cons_on_bindings
            @ lhs_cons @ rhs_cons )
        with Invalid_argument _ -> raise exn_to_throw)
    | Record { fields; partial } ->
        let record_pat_union =
          Type.Id.Map.union (fun binding _ _ ->
              raise (DuplicatedBindingInPatternProd { pat; binding }))
        in
        let first_field_name = fst (Std.Nonempty_list.first fields) in
        let record_type_name =
          match RecordDefs.find_by_field first_field_name record_defs with
          | Some name -> name
          | None -> raise (UnknownRecordField first_field_name)
        in
        let record_def =
          match RecordDefs.find_by_name record_type_name record_defs with
          | Some def -> def
          | None -> raise (NoSuchRecordType record_type_name)
        in
        let fresh_args, lookup_field =
          RecordDefs.instantiate_def record_def
        in
        if not partial then begin
          let pat_field_names =
            Std.Nonempty_list.to_list fields |> List.map fst |> StrSet.of_list
          in
          let def_field_names =
            Std.Nonempty_list.to_list record_def.fields
            |> List.map fst |> StrSet.of_list
          in
          let missing = StrSet.diff def_field_names pat_field_names in
          if not (StrSet.is_empty missing) then
            raise
              (IncompleteRecordPattern
                 {
                   record_type = record_type_name;
                   missing = StrSet.elements missing;
                 })
        end;
        let record_ty = Type.Generic.Con (record_type_name, fresh_args) in
        let mapped =
          Std.Nonempty_list.map
            (fun (name, inner_opt) ->
              match inner_opt with
              | None ->
                  let bind_ty = gen_var () in
                  let typed_pat : Typed_ir.Pattern.t =
                    { node = Bind name; type_ = bind_ty }
                  in
                  ( (name, Some typed_pat),
                    Type.Id.Map.singleton name bind_ty,
                    [ Constraint.{ lhs = bind_ty; rhs = lookup_field name } ] )
              | Some inner_pat ->
                  let typed_inner, inner_extras, inner_cons =
                    constraints_from_pattern env record_defs inner_pat
                  in
                  ( (name, Some typed_inner),
                    inner_extras,
                    Constraint.
                      { lhs = typed_inner.type_; rhs = lookup_field name }
                    :: inner_cons ))
            fields
        in
        let typed_fields = Std.Nonempty_list.map (fun (f, _, _) -> f) mapped in
        let extras =
          Std.Nonempty_list.to_list mapped
          |> List.map (fun (_, ext, _) -> ext)
          |> List.fold_left record_pat_union Type.Id.Map.empty
        in
        let cons =
          Std.Nonempty_list.to_list mapped
          |> List.concat_map (fun (_, _, cs) -> cs)
        in
        (mk (Record { fields = typed_fields; partial }) record_ty, extras, cons)

  let rec constraints_from_expr (env : TypeEnv.t) (record_defs : RecordDefs.t)
      (e : Expr.t) : Typed_ir.Expr.t * Constraint.t list * RecordDefs.t =
    let open Type.Generic in
    let gen_var () = Var (Type.Var.generate ()) in
    let mk node type_ : Typed_ir.Expr.t = { node; type_ } in
    let recurse env e = constraints_from_expr env record_defs e in
    match e with
    | Atom a -> (mk (Atom a) (of_atom a), [], record_defs)
    | Var id -> (
        match Type.Id.Map.find_opt id env with
        | None -> raise (UndefinedVariable id)
        | Some scheme ->
            let ty = instantiate scheme in
            (mk (Var id) ty, [], record_defs))
    | Ap (f, x) ->
        let typed_f, f_cons, _ = recurse env f in
        let typed_x, x_cons, _ = recurse env x in
        let result_ty = gen_var () in
        let all_cons =
          Constraint.
            { lhs = typed_f.type_; rhs = Arrow (typed_x.type_, result_ty) }
          :: f_cons
          @ x_cons
        in
        (mk (Ap (typed_f, typed_x)) result_ty, all_cons, record_defs)
    | Let { binding = Value { name; type_ = value_ty_annotated; value }; body }
      ->
        let typed_value, value_cons, record_defs = recurse env value in
        let all_cons =
          match value_ty_annotated with
          | None -> value_cons
          | Some value_ty_annotated ->
              {
                lhs = Type.T.of_human value_ty_annotated;
                rhs = typed_value.type_;
              }
              :: value_cons
        in
        let sub_inner, env_generalized =
          generalize all_cons env name typed_value.type_
        in
        let typed_value_resolved =
          Typed_ir.map_types_expr
            ~f:(Substitution.on_type ~sub:sub_inner)
            typed_value
        in
        let typed_body, body_cons, record_defs =
          constraints_from_expr env_generalized record_defs body
        in
        ( mk
            (Let
               {
                 binding = Value { name; value = typed_value_resolved };
                 body = typed_body;
               })
            typed_body.type_,
          body_cons,
          record_defs )
    | Let { binding = TypeDecl { name; args; rhs }; body } ->
        let tycon_fvs = StrSet.of_list args in
        let adt_type = Con (name, args |> List.map (fun arg -> Var arg)) in
        let wrap_forall inner =
          match args with [] -> inner | args -> args => inner
        in
        let env_new, record_defs_new =
          match rhs with
          | Adt ctors ->
              let env' =
                Std.Nonempty_list.to_list ctors
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
                             type_body ^-> adt_type
                         | None -> adt_type
                       in
                       Type.Id.Map.add tag
                         (Type.T.of_human (wrap_forall tag_type))
                         env)
                     env
              in
              (env', record_defs)
          | Record fields ->
              Std.Nonempty_list.iter
                (fun (_, field_ty) ->
                  let referred_fvs = FreeVariables.of_type_human field_ty in
                  if not @@ StrSet.subset referred_fvs tycon_fvs then
                    raise
                      (EscapedTypeVarInTypeDefinition
                         {
                           name;
                           tag = name;
                           free_variables = StrSet.diff referred_fvs tycon_fvs;
                         }))
                fields;
              (env, RecordDefs.register name args fields record_defs)
        in
        let typed_body, body_cons, record_defs_final =
          constraints_from_expr env_new record_defs_new body
        in
        ( mk
            (Let { binding = TypeDecl { name; args; rhs }; body = typed_body })
            typed_body.type_,
          body_cons,
          record_defs_final )
    | If { cond; then_; else_ } ->
        let typed_cond, cond_cons, _ = recurse env cond in
        let typed_then, then_cons, _ = recurse env then_ in
        let typed_else, else_cons, _ = recurse env else_ in
        ( mk
            (If { cond = typed_cond; then_ = typed_then; else_ = typed_else })
            typed_then.type_,
          Constraint.{ lhs = typed_cond.type_; rhs = bool }
          :: { lhs = typed_then.type_; rhs = typed_else.type_ }
          :: cond_cons
          @ then_cons @ else_cons,
          record_defs )
    | Lam (param, body) ->
        assert (param.type_ = None);
        let var_gen = gen_var () in
        let env_new = Type.Id.Map.add param.name var_gen env in
        let typed_body, constraints, _ = recurse env_new body in
        ( mk
            (Lam { param = param.name; param_type = var_gen; body = typed_body })
            (Arrow (var_gen, typed_body.type_)),
          constraints,
          record_defs )
    | Annotated { inner; typ = annotated } ->
        let typed_inner, cons, _ = recurse env inner in
        ( typed_inner,
          Constraint.
            { lhs = typed_inner.type_; rhs = Type.T.of_human annotated }
          :: cons,
          record_defs )
    | Fix inner ->
        (* fix: (a -> b) -> b *)
        let a = gen_var () in
        let b = gen_var () in
        let typed_inner, cons, _ = recurse env inner in
        ( mk (Fix typed_inner) b,
          Constraint.{ lhs = Arrow (a, b); rhs = typed_inner.type_ } :: cons,
          record_defs )
    | Prod elements ->
        let mapped = Std.Nonempty_list.map (fun e -> recurse env e) elements in
        let typed_elems = Std.Nonempty_list.map (fun (e, _, _) -> e) mapped in
        let tys =
          Std.Nonempty_list.map
            (fun ((e : Typed_ir.Expr.t), _, _) -> e.type_)
            mapped
        in
        let cons =
          Std.Nonempty_list.to_list mapped
          |> List.concat_map (fun (_, cs, _) -> cs)
        in
        (mk (Prod typed_elems) (Prod tys), cons, record_defs)
    | Record fields ->
        let first_field_name = fst (Std.Nonempty_list.first fields) in
        let record_type_name =
          match RecordDefs.find_by_field first_field_name record_defs with
          | Some name -> name
          | None -> raise (UnknownRecordField first_field_name)
        in
        let record_def =
          match RecordDefs.find_by_name record_type_name record_defs with
          | Some def -> def
          | None -> raise (NoSuchRecordType record_type_name)
        in
        let fresh_args, lookup_field =
          RecordDefs.instantiate_def record_def
        in
        let record_ty = Con (record_type_name, fresh_args) in
        let mapped =
          Std.Nonempty_list.map
            (fun (name, value) ->
              let typed_value, cons, _ = recurse env value in
              let field_ty = lookup_field name in
              ( (name, typed_value),
                Constraint.{ lhs = typed_value.type_; rhs = field_ty } :: cons
              ))
            fields
        in
        let typed_fields = Std.Nonempty_list.map fst mapped in
        let cons = Std.Nonempty_list.to_list mapped |> List.concat_map snd in
        (mk (Record typed_fields) record_ty, cons, record_defs)
    | FieldAccess (inner, field) ->
        let typed_inner, inner_cons, _ = recurse env inner in
        let record_type_name =
          match RecordDefs.find_by_field field record_defs with
          | Some name -> name
          | None -> raise (UnknownRecordField field)
        in
        let record_def =
          match RecordDefs.find_by_name record_type_name record_defs with
          | Some def -> def
          | None -> raise (NoSuchRecordType record_type_name)
        in
        let fresh_args, lookup_field =
          RecordDefs.instantiate_def record_def
        in
        let record_ty = Con (record_type_name, fresh_args) in
        let field_ty = lookup_field field in
        ( mk (FieldAccess (typed_inner, field)) field_ty,
          Constraint.{ lhs = typed_inner.type_; rhs = record_ty } :: inner_cons,
          record_defs )
    | Match { matched; branches } ->
        let env_with_extra ~here extras =
          Type.Id.Map.union
            (fun _ _ _ -> raise (Std.Exceptions.Unreachable here))
            extras env
        in
        let typed_matched, matched_cons, _ = recurse env matched in
        let (pat_1, body_1), branch_rest = Std.Nonempty_list.uncons branches in
        let typed_pat_1, body_1_env_extra, pat_1_cons =
          constraints_from_pattern env record_defs pat_1
        in
        let body_1_env = env_with_extra ~here:[%here] body_1_env_extra in
        let typed_body_1, body_1_cons, _ = recurse body_1_env body_1 in
        let typed_rest_branches, rest_cons =
          List.map
            (fun (pat_i, body_i) ->
              let typed_pat_i, body_i_env_extra, pat_i_cons =
                constraints_from_pattern env record_defs pat_i
              in
              let body_i_env = env_with_extra ~here:[%here] body_i_env_extra in
              let typed_body_i, body_i_cons, _ = recurse body_i_env body_i in
              let cons =
                Constraint.
                  { lhs = typed_pat_i.type_; rhs = typed_matched.type_ }
                :: Constraint.
                     { lhs = typed_body_i.type_; rhs = typed_body_1.type_ }
                :: pat_i_cons
                @ body_i_cons
              in
              ((typed_pat_i, typed_body_i), cons))
            branch_rest
          |> List.split
        in
        let all_rest_cons = List.flatten rest_cons in
        let typed_branches =
          Std.Nonempty_list.init (typed_pat_1, typed_body_1) typed_rest_branches
        in
        ( mk
            (Match { matched = typed_matched; branches = typed_branches })
            typed_body_1.type_,
          Constraint.{ lhs = typed_pat_1.type_; rhs = typed_matched.type_ }
          :: matched_cons
          @ pat_1_cons @ body_1_cons @ all_rest_cons,
          record_defs )

  let on_expr (env : TypeEnv.t) (exp : Expr.t) : Typed_ir.Expr.t =
    let typed_skeleton, cons, _ =
      constraints_from_expr env RecordDefs.empty exp
    in
    let sub = Unification.many cons in
    let typed_resolved =
      Typed_ir.map_types_expr ~f:(Substitution.on_type ~sub) typed_skeleton
    in
    let top_type = typed_resolved.type_ in
    let rest_fvs = FreeVariables.of_type top_type in
    if Type.Var.Set.is_empty rest_fvs then typed_resolved
    else { typed_resolved with type_ = Forall (rest_fvs, top_type) }
end
