(* Holes in quoted syntax, found and filled on its reflection value.

   A hole [$x] is an id spelled ["$x"] ([$] cannot begin a source identifier).
   Where it sits decides its kind (M10): as the id of an expression variable it
   stands for an [Expr], as the id of a pattern variable for a [Pattern], and
   anywhere else - a binder, a field's id - for an [Id]. *)

open Core

type kind = Expr | Pattern | Id

let hole_name (v : value) =
  match v with
  | VRecord { fields; _ } when List.mem_assoc "scope" fields -> (
      match List.assoc_opt "name" fields with
      | Some (VAtom (String n)) when String.length n > 1 && n.[0] = '$' -> Some n
      | _ -> None)
  | _ -> None

let rec map_holes (on_hole : kind -> string -> value -> value) (v : value) : value =
  match v with
  | VCon { name = ("RawVar" | "RawPatBind") as c; spine = [ _; id ]; _ } when Option.is_some (hole_name id) ->
      on_hole (if String.equal c "RawVar" then Expr else Pattern) (Option.get (hole_name id)) v
  | VCon c -> VCon { c with spine = List.map (map_holes on_hole) c.spine }
  | VRecord r -> (
      match hole_name v with
      | Some n -> on_hole Id n v
      | None -> VRecord { r with fields = List.map (fun (k, x) -> (k, map_holes on_hole x)) r.fields })
  | _ -> v

(* Every hole occurrence, in order, with the kind its position gives it. *)
let occurrences (template : value) : (string * kind) list =
  let found = ref [] in
  ignore (map_holes (fun kind name v -> found := (name, kind) :: !found; v) template);
  List.rev !found

let fill (template : value) (values : (string * value) list) : value =
  map_holes (fun _ name v -> Option.value (List.assoc_opt name values) ~default:v) template
