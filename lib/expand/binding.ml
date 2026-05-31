type binding_info = {
  scope : Scope_set.t;
  resolved_name : string;
}

type t = (string, binding_info list) Hashtbl.t

let create () : t = Hashtbl.create 32

let copy (tbl : t) : t =
  let new_tbl = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add new_tbl k v) tbl;
  new_tbl

let extend (tbl : t) ~name ~scope ~resolved_name =
  let info = { scope; resolved_name } in
  let existing = try Hashtbl.find tbl name with Not_found -> [] in
  Hashtbl.replace tbl name (info :: existing)

let has_name (tbl : t) name = Hashtbl.mem tbl name

(** Add a binding. Bindings for the same written name are stacked;
    during resolution the one with the largest subset-scope wins. *)

(** Resolve an identifier occurrence: find the binding with the same
    written name whose scope set is a subset of the occurrence scope
    set, choosing the one with the largest binding scope. *)
let incompatible_best name a b =
  failwith (Printf.sprintf "ambiguous binding for %s: scopes %s and %s"
              name
              (Format.asprintf "%a" Scope_set.pp a.scope)
              (Format.asprintf "%a" Scope_set.pp b.scope))

let more_specific name a b =
  if Scope_set.equal a.scope b.scope then b
  else if Scope_set.subset a.scope b.scope then b
  else if Scope_set.subset b.scope a.scope then a
  else incompatible_best name a b

let resolve (tbl : t) (id : Syntax.id) : binding_info option =
  let candidates = try Hashtbl.find tbl id.name with Not_found -> [] in
  match candidates with
  | [] -> None
  | _ ->
    let matches = List.filter (fun info -> Scope_set.subset info.scope id.scope) candidates in
    match matches with
    | [] -> None
    | _ ->
      Some (List.fold_left (more_specific id.name) (List.hd matches) (List.tl matches))
