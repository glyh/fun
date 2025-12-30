open Ppx_hash_lib.Std.Hash.Builtin

module Id = struct
  module T = struct
    type t = string [@@deriving eq, ord]

    let all_symbols = String.for_all (String.contains "+-*/=<>!@#$%^&|~")
    let pp s = "'" ^ if all_symbols s then "`" ^ s ^ "`" else s
  end

  module Map = Map.Make (T)
  include T
end

module Var = struct
  module T = struct
    type t = { id : int; tag : string option } [@@deriving eq, ord, hash]

    let pp { id; tag } =
      match tag with
      | Some tag -> Printf.sprintf "%s_%d" tag id
      | None -> Printf.sprintf "_%d" id

    let var_uuid = ref 0

    let generate ?tag () =
      incr var_uuid;
      { id = !var_uuid; tag }

    let inherit_ var =
      incr var_uuid;
      { id = !var_uuid; tag = Some (pp var) }
  end

  module Set = Set.Make (T)
  module Map = Map.Make (T)
  module Hashtbl = Hashtbl.Make (T)
  include T
end

module Exceptions = struct
  exception Rank2TypeUnsupported
  exception UnusedVars of Var.Set.t
  exception MismatchForallVars of (Var.t Seq.t * Var.t Seq.t)
end

open Exceptions

module Precedence = struct
  (* Assumption: in the same precedence level, associativity is always same *)
  let of_tag = function
    | `Forall -> (30, `Left)
    | `Prod -> (20, `Left)
    | `Arrow -> (10, `Right)
    | `Var | `Con -> (0, `Left)

  let base = (-1, `Left)

  let should_add_paren ~ctx:(ctx_prec, ctx_direction) ~my:(my_prec, my_assoc) =
    if my_prec < ctx_prec then true
    else if my_prec > ctx_prec then false
    else
      match (ctx_direction, my_assoc) with
      | `Left, `Right | `Right, `Left -> true
      | _, _ -> false
end

module Generic = struct
  type ('var, 'var_set) t =
    | Forall of 'var_set * ('var, 'var_set) t
    | Var of 'var
    | Con of Id.t * ('var, 'var_set) t list
    | Prod of ('var, 'var_set) t * ('var, 'var_set) t
    | Arrow of ('var, 'var_set) t * ('var, 'var_set) t
  [@@deriving eq]

  let to_tag = function
    | Forall _ -> `Forall
    | Var _ -> `Var
    | Con _ -> `Con
    | Prod _ -> `Prod
    | Arrow _ -> `Arrow

  let pp ~pp_var ~pp_var_set =
    let rec pp_at ctx ty =
      let ((my_prec, _) as my) = Precedence.of_tag @@ to_tag ty in
      let my_left = (my_prec, `Left) in
      let my_right = (my_prec, `Right) in
      let unparened =
        match ty with
        | Forall (vars, inner) ->
            Printf.sprintf "'%s . %s" (pp_var_set vars) (pp_at my inner)
        | Var v -> "'" ^ pp_var v
        | Con (ty, []) -> ty
        | Con (ty, args) ->
            ty ^ "["
            ^ (List.map (pp_at Precedence.base) args |> String.concat ",")
            ^ "]"
        | Arrow (a, b) -> pp_at my_left a ^ " -> " ^ pp_at my_right b
        | Prod (a, b) -> pp_at my_left a ^ " * " ^ pp_at my_right b
      in
      if Precedence.should_add_paren ~ctx ~my then "(" ^ unparened ^ ")"
      else unparened
    in
    pp_at Precedence.base

  let con_0 name = Con (name, [])
  let unit = con_0 "Unit"
  let i64 = con_0 "I64"
  let bool = con_0 "Bool"
  let char = con_0 "Char"
end

module Human = struct
  type t = (string, string list) Generic.t [@@deriving eq]

  let pp = Generic.pp ~pp_var:(fun v -> v) ~pp_var_set:(String.concat " ")
end

module T = struct
  type t = (Var.t, Var.Set.t) Generic.t [@@deriving eq]

  let pp =
    Generic.pp ~pp_var:Var.pp ~pp_var_set:(fun s ->
        Var.Set.to_seq s |> Seq.map Var.pp |> List.of_seq |> String.concat " ")

  let of_human (input : Human.t) : t =
    let rec of_human_do ctx = function
      | Generic.Forall (vars, inner) ->
          let new_binds =
            List.map (fun tag -> (tag, Var.generate ~tag ())) vars
            |> List.to_seq
          in
          let vars_converted =
            Seq.map (fun (_, v) -> v) new_binds |> Var.Set.of_seq
          in
          let new_ctx = Id.Map.add_seq new_binds ctx in
          Generic.Forall (vars_converted, of_human_do new_ctx inner)
      | Var var -> (
          match Id.Map.find_opt var ctx with
          | Some var -> Var var
          | None -> Var (Var.generate ~tag:var ()))
      | Con (s, args) -> Con (s, List.map (of_human_do ctx) args)
      | Arrow (a, b) -> Arrow (of_human_do ctx a, of_human_do ctx b)
      | Prod (a, b) -> Prod (of_human_do ctx a, of_human_do ctx b)
    in
    of_human_do Id.Map.empty input

  let order_vars varset type_ =
    let vars_to_slash =
      Var.Set.to_seq varset
      |> Seq.map (fun var -> (var, ()))
      |> Var.Hashtbl.of_seq
    in
    let q = Queue.create () in
    let rec order_vars_do = function
      | Generic.Forall _ -> raise Rank2TypeUnsupported
      | Var var ->
          if Var.Hashtbl.find_opt vars_to_slash var |> Option.is_some then
            Var.Hashtbl.remove vars_to_slash var;
          Queue.add var q
      | Con (_, vars) -> List.iter order_vars_do vars
      | Arrow (a, b) | Prod (a, b) ->
          order_vars_do a;
          order_vars_do b
    in
    order_vars_do type_;
    let ordered = Queue.to_seq q in
    let unused_vars = Queue.to_seq q |> Var.Set.of_seq |> Var.Set.diff varset in
    if Var.Set.cardinal unused_vars != 0 then raise (UnusedVars unused_vars);
    ordered

  let rec map_equal ~mapping ~from to_ =
    let recur from to_ = map_equal ~mapping ~from to_ in
    match (from, to_) with
    | Generic.Forall _, _ | _, Generic.Forall _ -> raise Rank2TypeUnsupported
    | Var from, Var to_ ->
        Var.Map.find_opt from mapping |> Option.equal Var.equal (Some to_)
    | Con (tycon1, ty_args1), Con (tycon2, ty_args2) ->
        String.equal tycon1 tycon2 && List.equal recur ty_args1 ty_args2
    | Arrow (a1, b1), Arrow (a2, b2) | Prod (a1, b1), Prod (a2, b2) ->
        recur a1 a2 && recur b1 b2
    | _ -> false

  let equal t1 t2 =
    match (t1, t2) with
    | Generic.Forall (binds1, inner1), Generic.Forall (binds2, inner2) ->
        let ordered1 = order_vars binds1 inner1 in
        let ordered2 = order_vars binds2 inner2 in
        if Seq.length ordered1 != Seq.length ordered2 then
          raise (MismatchForallVars (ordered1, ordered2));
        let mapping = Seq.zip ordered1 ordered2 |> Var.Map.of_seq in
        map_equal ~mapping ~from:inner1 inner2
    | Var _, Var _ ->
        (* Dangling type variables can't be considered equal *)
        false
    | _ -> equal t1 t2
end
