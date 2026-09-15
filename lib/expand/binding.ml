(** Whether a binding names a value, a (procedural) macro, or a syntactic role
    the enforester reads ([Role]). Name resolution returns values and macros; a
    [Role] binder is what the enforester consults, and what a value binder of
    its name conflicts with (M7). *)
type binding_kind = Value | Macro | Role

type binding_info = {
  scope : Scope_set.t;
  resolved_name : string;
  kind : binding_kind;
  (* A [Role] binder's role. *)
  role : Syntax.role option;
}

type t = (string, binding_info list) Hashtbl.t

let create () : t = Hashtbl.create 32

let copy (tbl : t) : t =
  let new_tbl = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add new_tbl k v) tbl;
  new_tbl

let extend ?role (tbl : t) ~name ~scope ~kind ~resolved_name =
  let info = { scope; resolved_name; kind; role } in
  let existing = try Hashtbl.find tbl name with Not_found -> [] in
  Hashtbl.replace tbl name (info :: existing)

let incompatible_best name a b =
  failwith (Printf.sprintf "ambiguous binding for %s: scopes %s and %s"
              name
              (Format.asprintf "%a" Scope_set.pp a.scope)
              (Format.asprintf "%a" Scope_set.pp b.scope))

(** Resolve an identifier occurrence: among the binders of its written name
    whose scope set is a subset of the occurrence's, the one with the largest
    wins. Binders with equal scope sets resolve to the one added last ([extend]
    prepends, so that is the first candidate). *)
let best name candidates =
  let better a b =
    if Scope_set.subset b.scope a.scope then a
    else if Scope_set.subset a.scope b.scope then b
    else incompatible_best name a b
  in
  match candidates with
  | [] -> None
  | first :: rest -> Some (List.fold_left better first rest)

(* An order group's name is a role binder of its own sort: it never mixes with
   another binder of its name. *)
let is_group (info : binding_info) =
  match info.role with Some { Syntax.meaning = Syntax.OrderGroup; _ } -> true | _ -> false

let resolve (tbl : t) (id : Syntax.id) : binding_info option =
  Option.value ~default:[] (Hashtbl.find_opt tbl id.name)
  |> List.filter (fun info -> info.kind <> Role && Scope_set.subset info.scope id.scope)
  |> best id.name

(* A syntactic role is resolved like any binder (M7), among the roles of that
   name and fixity. *)
let find_role (tbl : t) ~(fixity : Syntax.operator_fixity) ~(scope : Scope_set.t) name : Syntax.role option =
  Option.value ~default:[] (Hashtbl.find_opt tbl name)
  |> List.filter (fun info ->
         Scope_set.subset info.scope scope
         && (not (is_group info))
         && match info.role with Some r -> r.Syntax.fixity = fixity | None -> false)
  |> best name
  |> Fun.flip Option.bind (fun info -> info.role)

(* An order group, resolved by scope set like any binder. *)
let find_order (tbl : t) ~(scope : Scope_set.t) name : Syntax.order option =
  Option.value ~default:[] (Hashtbl.find_opt tbl name)
  |> List.filter (fun info -> Scope_set.subset info.scope scope && is_group info)
  |> best name
  |> Fun.flip Option.bind (fun info -> Option.bind info.role (fun (r : Syntax.role) -> r.order))

let role ?(declared_at = Source_span.synthetic) ~fixity ?order meaning : Syntax.role =
  { Syntax.fixity; order; meaning; declared_at; from_unit = None }

(* A unit's exported roles, stamped with the unit at the import site - the only
   place that knows the written path. An imported rule's replacement was parsed
   in that unit, so the scopes on its ids mean nothing here: they are dropped,
   and the ids it introduces resolve through the unit's open (see
   [Expand_ctx.open_candidates]). *)
let from_unit path (roles : (string * Syntax.role) list) =
  List.map (fun (name, (r : Syntax.role)) -> (name, { r with Syntax.from_unit = Some path })) roles

let fixity_name = function Syntax.PrefixOp -> "prefix" | Syntax.InfixOp -> "infix"

let duplicate_exports_message (exports : (string * Syntax.role) list) =
  let rec go seen = function
    | [] -> None
    | (name, (r : Syntax.role)) :: rest -> (
        match
          List.find_opt
            (fun (n, (p : Syntax.role)) ->
              String.equal n name && p.fixity = r.fixity && (p.meaning = Syntax.OrderGroup) = (r.meaning = Syntax.OrderGroup))
            seen
        with
        | Some (_, previous) ->
            Some
              (Printf.sprintf
                 "ambiguous syntax extension candidates for %s operator %S: declarations at %s and %s"
                 (fixity_name r.fixity) name
                 (Format.asprintf "%a" Source_span.pp previous.Syntax.declared_at)
                 (Format.asprintf "%a" Source_span.pp r.declared_at))
        | None -> go ((name, r) :: seen) rest)
  in
  go [] exports
