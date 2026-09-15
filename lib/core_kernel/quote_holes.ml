(* Holes in quoted syntax, found and filled on its reflection value.

   A hole [$x] is an id spelled ["$x"] ([$] cannot begin a source identifier).
   Where it sits decides its kind (M10): as the id of an expression variable it
   stands for an [Expr], as the id of a pattern variable for a [Pattern], as a
   quoted item for a [Decl], and anywhere else - a binder, a field's id - for
   an [Id]. An identifier token spelled [$x] - a generated rule's head - is an
   [Id] too, filled as the token spelling that id. *)

open Core

type kind = Expr | Pattern | Decl | Id

let hole_spelling = function
  | VAtom (String n) when String.length n > 1 && n.[0] = '$' -> Some n
  | _ -> None

let hole_name (v : value) =
  match v with
  | VRecord { fields; _ } when List.mem_assoc "scope" fields ->
      Option.bind (List.assoc_opt "name" fields) hole_spelling
  | _ -> None

let token_hole_name (v : value) =
  match v with
  | VCon { name = "Tok"; spine = [ _; VCon { name = "IdentTok"; spine = [ n ]; _ }; _ ]; _ } -> hole_spelling n
  | _ -> None

(* The identifier token [tok] was written as, spelling the [Id] [id] instead. *)
let token_of_id (tok : value) (id : value) =
  match tok, id with
  | VCon ({ spine = [ span; VCon kind; _ ]; _ } as t), VRecord { fields; _ } ->
      VCon { t with spine = [ span; VCon { kind with spine = [ List.assoc "name" fields ] }; List.assoc "scope" fields ] }
  | _ -> failwith "Quote_holes.token_of_id: a token hole filled with a non-Id"

let rec map_holes (on_hole : kind -> string -> value -> value) (v : value) : value =
  match v with
  | VCon { name = ("RawVar" | "RawPatBind") as c; spine = [ _; id ]; _ } when Option.is_some (hole_name id) ->
      on_hole (if String.equal c "RawVar" then Expr else Pattern) (Option.get (hole_name id)) v
  | VCon { name = "DeclHole"; spine = [ id ]; _ } when Option.is_some (hole_name id) ->
      on_hole Decl (Option.get (hole_name id)) v
  | VCon _ when Option.is_some (token_hole_name v) -> on_hole Id (Option.get (token_hole_name v)) v
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
  map_holes
    (fun _ name v ->
      match List.assoc_opt name values with
      | Some filled when Option.is_some (token_hole_name v) -> token_of_id v filled
      | Some filled -> filled
      | None -> v)
    template
