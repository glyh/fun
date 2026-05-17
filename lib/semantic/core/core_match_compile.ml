open Core
module DT = Core_decision_tree

type missing_pat = MWild | MCon of string * missing_pat option

exception Non_exhaustive of missing_pat

let () =
  Printexc.register_printer (function
    | Non_exhaustive MWild -> Some "Non_exhaustive: _"
    | Non_exhaustive (MCon (name, None)) -> Some ("Non_exhaustive: " ^ name)
    | Non_exhaustive (MCon (name, Some _)) ->
        Some ("Non_exhaustive: " ^ name ^ " _")
    | _ -> None)

type entry = core_pat

type row = {
  pats : entry array;
  branch : int;
  bindings : DT.occurrence list;
}

type matrix = {
  header : DT.occurrence array;
  rows : row list;
}

let is_empty m = m.rows = []

let get_column m i = List.map (fun r -> r.pats.(i)) m.rows

let swap_columns m i j =
  if i = j then m
  else
    let swap_arr a =
      let a = Array.copy a in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
      a
    in
    {
      header = swap_arr m.header;
      rows = List.map (fun r -> { r with pats = swap_arr r.pats }) m.rows;
    }

type refutability = Irrefutable | Destruct

let classify = function
  | CPatCon _ -> Destruct
  | CPatWild | CPatBind -> Irrefutable

let find_refutable_column m =
  let ncols = Array.length m.header in
  let rec go i =
    if i >= ncols then None
    else
      let col = get_column m i in
      if List.exists (fun p -> classify p = Destruct) col then Some i
      else go (i + 1)
  in
  go 0

let collect_tags col =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun p ->
      match p with
      | CPatCon (name, _, _) ->
          if not (Hashtbl.mem seen name) then Hashtbl.replace seen name ()
      | _ -> ())
    col;
  Hashtbl.fold (fun k () acc -> k :: acc) seen []

let specialize_destruct m tag ~num_type_params ~arity =
  let occ0 = m.header.(0) in
  let new_occs =
    Array.init arity (fun i ->
      DT.OChild { parent = occ0; index = num_type_params + i })
  in
  let rest_header =
    Array.sub m.header 1 (Array.length m.header - 1)
  in
  let header = Array.append new_occs rest_header in
  let rows =
    List.filter_map
      (fun r ->
        match r.pats.(0) with
        | CPatCon (name, _, sub_pats) when String.equal name tag ->
            let sub_arr = Array.of_list sub_pats in
            let rest = Array.sub r.pats 1 (Array.length r.pats - 1) in
            let pats = Array.append sub_arr rest in
            Some { r with pats }
        | CPatWild ->
            let wilds = Array.make arity CPatWild in
            let rest = Array.sub r.pats 1 (Array.length r.pats - 1) in
            let pats = Array.append wilds rest in
            Some { r with pats }
        | CPatBind ->
            let wilds = Array.make arity CPatWild in
            let rest = Array.sub r.pats 1 (Array.length r.pats - 1) in
            let pats = Array.append wilds rest in
            let bindings = r.bindings @ [ occ0 ] in
            Some { pats; branch = r.branch; bindings }
        | _ -> None)
      m.rows
  in
  { header; rows }

let default_matrix m =
  let occ0 = m.header.(0) in
  let header = Array.sub m.header 1 (Array.length m.header - 1) in
  let rows =
    List.filter_map
      (fun r ->
        match r.pats.(0) with
        | CPatWild ->
            let pats = Array.sub r.pats 1 (Array.length r.pats - 1) in
            Some { r with pats }
        | CPatBind ->
            let pats = Array.sub r.pats 1 (Array.length r.pats - 1) in
            let bindings = r.bindings @ [ occ0 ] in
            Some { pats; branch = r.branch; bindings }
        | _ -> None)
      m.rows
  in
  { header; rows }

let collect_leaf_bindings m =
  let r = List.hd m.rows in
  let extra =
    Array.to_list m.header
    |> List.mapi (fun i occ -> (i, occ))
    |> List.filter_map (fun (i, occ) ->
         match r.pats.(i) with CPatBind -> Some occ | _ -> None)
  in
  r.bindings @ extra

let compile ~(constructors : (string * int * bool) list) (pats : core_pat list)
    : DT.t =
  let module DTB = DT.Make () in
  let initial : matrix =
    {
      header = [| DT.OBase |];
      rows =
        List.mapi
          (fun i p -> { pats = [| p |]; branch = i; bindings = [] })
          pats;
    }
  in
  let rec go (m : matrix) : DT.t =
    if is_empty m then raise (Non_exhaustive MWild);
    match find_refutable_column m with
    | None ->
        DTB.get (Leaf { branch = (List.hd m.rows).branch;
                        bindings = collect_leaf_bindings m })
    | Some i ->
        let m = swap_columns m 0 i in
        let col = get_column m 0 in
        let tags = collect_tags col in
        let cases =
          List.filter_map
            (fun tag ->
              let num_type_params, arity =
                match
                  List.find_opt (fun (n, _, _) -> String.equal n tag) constructors
                with
                | Some (_, ntp, has_payload) ->
                    (ntp, if has_payload then 1 else 0)
                | None -> (0, 0)
              in
              let sub = specialize_destruct m tag ~num_type_params ~arity in
              match go sub with
              | dt -> Some (tag, arity, dt)
              | exception Non_exhaustive inner ->
                  raise
                    (Non_exhaustive
                       (MCon (tag, match inner with MWild -> Some MWild | _ -> Some inner))))
            tags
        in
        let all_ctor_names = List.map (fun (n, _, _) -> n) constructors in
        let missing_ctors =
          List.filter (fun c -> not (List.mem c tags)) all_ctor_names
        in
        let default =
          if missing_ctors = [] then None
          else
            let dm = default_matrix m in
            if is_empty dm then
              raise
                (Non_exhaustive
                   (MCon
                      ( List.hd missing_ctors,
                        let _, _, has_payload =
                          List.find
                            (fun (n, _, _) -> String.equal n (List.hd missing_ctors))
                            constructors
                        in
                        if has_payload then Some MWild else None )))
            else Some (go dm)
        in
        let cases_with_arity = List.map (fun (tag, arity, dt) -> (tag, arity, dt)) cases in
        DTB.get (Destruct { occurrence = m.header.(0); cases = cases_with_arity; default })
  in
  go initial
