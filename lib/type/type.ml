module Id = struct
  module T = struct
    type t = string [@@deriving eq, ord]
  end

  module Map = Map.Make (T)
  include T
end

module Var = struct
  module T = struct
    type t = { id : int; tag : string option } [@@deriving eq, ord, hash]

    let show { id; tag } =
      match tag with
      | Some tag -> Printf.sprintf "%s.%d" tag id
      | None -> Printf.sprintf ".%d" id

    let var_uuid = ref 0

    let generate ?tag () =
      incr var_uuid;
      { id = !var_uuid; tag }

    let inherit_ var =
      incr var_uuid;
      { id = !var_uuid; tag = Some (show var) }
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

module Human = struct
  type t =
    | Forall of string list * t
    | Var of string
    | Con of string
    | Arrow of t * t
end

module T = struct
  type t =
    | Forall of Var.Set.t * t
    | Var of Var.t
    | Con of string
    | Arrow of t * t
  [@@deriving eq]

  let rec pp = function
    | Forall (vars, inner) ->
        let vars_str =
          Var.Set.to_seq vars |> Seq.map Var.show |> List.of_seq
          |> String.concat " "
        in
        vars_str ^ " . " ^ pp inner
    | Var v -> Var.show v
    | Con ty -> ty
    | Arrow (a, b) -> (
        match a with
        | Arrow _ -> "(" ^ pp a ^ ") -> " ^ pp b
        | _ -> pp a ^ " -> " ^ pp b)

  let of_human input =
    let rec of_human_do ctx = function
      | Human.Forall (vars, inner) ->
          let new_binds =
            List.map (fun tag -> (tag, Var.generate ~tag ())) vars
            |> List.to_seq
          in
          let vars_converted =
            Seq.map (fun (_, v) -> v) new_binds |> Var.Set.of_seq
          in
          let new_ctx = Id.Map.add_seq new_binds ctx in
          Forall (vars_converted, of_human_do new_ctx inner)
      | Var var -> (
          match Id.Map.find_opt var ctx with
          | Some var -> Var var
          | None -> Var (Var.generate ~tag:var ()))
      | Con s -> Con s
      | Arrow (a, b) -> Arrow (of_human_do ctx a, of_human_do ctx b)
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
      | Forall _ -> raise Rank2TypeUnsupported
      | Var var ->
          if Var.Hashtbl.find_opt vars_to_slash var |> Option.is_some then
            Var.Hashtbl.remove vars_to_slash var;
          Queue.add var q
      | Con _ -> ()
      | Arrow (a, b) ->
          order_vars_do a;
          order_vars_do b
    in
    order_vars_do type_;
    let ordered = Queue.to_seq q in
    let unused_vars = Queue.to_seq q |> Var.Set.of_seq |> Var.Set.diff varset in
    if Var.Set.cardinal unused_vars != 0 then raise (UnusedVars unused_vars);
    ordered

  let rec map_equal ~mapping ~from to_ =
    match (from, to_) with
    | Forall _, _ | _, Forall _ -> raise Rank2TypeUnsupported
    | Var from, Var to_ ->
        Var.Map.find_opt from mapping |> Option.equal Var.equal (Some to_)
    | Con lhs, Con rhs -> String.equal lhs rhs
    | Arrow (a1, b1), Arrow (a2, b2) ->
        map_equal ~mapping ~from:a1 a2 && map_equal ~mapping ~from:b1 b2
    | _ -> false

  let equal t1 t2 =
    match (t1, t2) with
    | Forall (binds1, inner1), Forall (binds2, inner2) ->
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

module Builtin = struct
  open T

  let unit = Con "Unit"
  let i64 = Con "I64"
  let bool = Con "Bool"
  let char = Con "Char"
end
