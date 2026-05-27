type occurrence =
  | OBase
  | OChild of { parent : occurrence; index : int }
  | OField of { parent : occurrence; name : string }

type switch_key = KAtom of Atom.t | KType of Core.atom_ty | KNominal of Core.nominal_id

let rec occurrence_equal a b =
  match (a, b) with
  | OBase, OBase -> true
  | OChild p1, OChild p2 ->
      p1.index = p2.index
      && occurrence_equal p1.parent p2.parent
  | OField p1, OField p2 ->
      String.equal p1.name p2.name
      && occurrence_equal p1.parent p2.parent
  | _ -> false

let rec occurrence_hash a =
  match a with
  | OBase -> 0
  | OChild { parent; index } ->
      Hashtbl.hash (1, occurrence_hash parent, index)
  | OField { parent; name } ->
      Hashtbl.hash (2, occurrence_hash parent, name)

let switch_key_equal lhs rhs =
  match (lhs, rhs) with
  | KAtom lhs, KAtom rhs -> Atom.equal lhs rhs
  | KType lhs, KType rhs -> Core.equal_atom_ty lhs rhs
  | KNominal lhs, KNominal rhs -> lhs = rhs
  | _ -> false

type branch = int

type t = { id : int; content : content }

and content =
  | Leaf of { branch : branch; bindings : occurrence list }
  | Destruct of {
      occurrence : occurrence;
      cases : (string * int * t) list;
      default : t option;
    }
  | Switch of {
      occurrence : occurrence;
      cases : (switch_key * t) list;
      default : t;
    }

let rec equal a b =
  if a.id = b.id then true
  else
    match (a.content, b.content) with
    | Leaf l1, Leaf l2 ->
        l1.branch = l2.branch
        && List.equal occurrence_equal l1.bindings l2.bindings
    | Destruct d1, Destruct d2 ->
        occurrence_equal d1.occurrence d2.occurrence
        && Option.equal equal d1.default d2.default
        && List.equal
             (fun (n1, a1, t1) (n2, a2, t2) ->
               String.equal n1 n2 && a1 = a2 && equal t1 t2)
             d1.cases d2.cases
    | Switch s1, Switch s2 ->
        occurrence_equal s1.occurrence s2.occurrence
        && equal s1.default s2.default
        && List.equal
             (fun (a1, t1) (a2, t2) -> switch_key_equal a1 a2 && equal t1 t2)
             s1.cases s2.cases
    | _ -> false

let hash t = Hashtbl.hash t.id

module Make () = struct
  module TT = Hashtbl.Make (struct
    type nonrec t = t

    let hash = hash
    let equal = equal
  end)

  let cache : t TT.t = TT.create 16
  let counter = ref (-1)

  let get content =
    let c' = !counter + 1 in
    let candidate = { content; id = c' } in
    match TT.find_opt cache candidate with
    | Some existing -> existing
    | None ->
        TT.add cache candidate candidate;
        incr counter;
        candidate
end
