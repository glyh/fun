open Core
module DT = Core_decision_tree

type domain = Nominal of (string * int * bool) list | Atom of atom_ty | Product of int | Unknown

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

type refutability = Irrefutable | Destruct | Switch | ProductPat

let classify = function
  | CPatCon _ -> Destruct
  | CPatAtom _ -> Switch
  | CPatProd _ -> ProductPat
  | CPatWild | CPatBind -> Irrefutable

let find_refutable_column m =
  let ncols = Array.length m.header in
  let rec go i =
    if i >= ncols then None
    else
      let col = get_column m i in
      if List.exists (fun p -> classify p <> Irrefutable) col then Some i
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

let collect_atoms col =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun p ->
      match p with
      | CPatAtom atom ->
          if not (Hashtbl.mem seen atom) then Hashtbl.replace seen atom ()
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

let specialize_literal m atom =
  let occ0 = m.header.(0) in
  let header = Array.sub m.header 1 (Array.length m.header - 1) in
  let rows =
    List.filter_map
      (fun r ->
        match r.pats.(0) with
        | CPatAtom atom' when Syntax.Ast.Atom.equal atom atom' ->
            let pats = Array.sub r.pats 1 (Array.length r.pats - 1) in
            Some { r with pats }
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

let specialize_product m arity =
  let occ0 = m.header.(0) in
  let new_occs =
    Array.init arity (fun i -> DT.OChild { parent = occ0; index = i })
  in
  let rest_header = Array.sub m.header 1 (Array.length m.header - 1) in
  let header = Array.append new_occs rest_header in
  let rows =
    List.filter_map
      (fun r ->
        match r.pats.(0) with
        | CPatProd sub_pats ->
            if List.length sub_pats <> arity then None
            else
              let rest = Array.sub r.pats 1 (Array.length r.pats - 1) in
              Some { r with pats = Array.append (Array.of_list sub_pats) rest }
        | CPatWild ->
            let wilds = Array.make arity CPatWild in
            let rest = Array.sub r.pats 1 (Array.length r.pats - 1) in
            Some { r with pats = Array.append wilds rest }
        | CPatBind ->
            let wilds = Array.make arity CPatWild in
            let rest = Array.sub r.pats 1 (Array.length r.pats - 1) in
            let bindings = r.bindings @ [ occ0 ] in
            Some { pats = Array.append wilds rest; branch = r.branch; bindings }
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

let all_atoms = function
  | TBool -> Some [ Syntax.Ast.Atom.Bool true; Bool false ]
  | TUnit -> Some [ Unit ]
  | TI64 | TChar -> None

let compile_with_domains ~domain_of_occurrence (pats : core_pat list) : DT.t =
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
        (match classify (List.find (fun p -> classify p <> Irrefutable) col) with
        | ProductPat ->
            let arity =
              match List.find_map (function CPatProd ps -> Some (List.length ps) | _ -> None) col with
              | Some arity -> arity
              | None -> (
                  match domain_of_occurrence m.header.(0) with
                  | Product arity -> arity
                  | _ -> 0)
            in
            go (specialize_product m arity)
        | Destruct ->
            let domain = domain_of_occurrence m.header.(0) in
            let constructors = match domain with Nominal ctors -> ctors | _ -> [] in
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
        | Switch ->
            let atoms = collect_atoms col in
            let cases =
              List.map
                (fun atom ->
                  let sub = specialize_literal m atom in
                  (atom, go sub))
                atoms
            in
            let dm = default_matrix m in
            let domain = domain_of_occurrence m.header.(0) in
            let missing_finite_atom =
              match domain with
              | Atom atom_ty ->
                  (match all_atoms atom_ty with
                  | Some all ->
                      List.exists
                        (fun atom ->
                          not (List.exists (Syntax.Ast.Atom.equal atom) atoms))
                        all
                  | None -> true)
              | _ -> true
            in
            let default =
              if is_empty dm && missing_finite_atom then raise (Non_exhaustive MWild)
              else if is_empty dm then DTB.get (Leaf { branch = (List.hd m.rows).branch; bindings = [] })
              else go dm
            in
            DTB.get (Switch { occurrence = m.header.(0); cases; default })
        | Irrefutable -> assert false)
  in
  go initial

let compile ~domain pats =
  compile_with_domains
    ~domain_of_occurrence:(function DT.OBase -> domain | _ -> Unknown)
    pats
