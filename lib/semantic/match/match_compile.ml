module OT = Hashtbl.Make (Occurence)
module Row = Matrix.Row
module Col = Matrix.Col

(* preprocess list of patterns at a given occurrence into a pattern matrix.
   [branches] provides the branch index for each row. *)
let preprocess (base : Occurence.t) (branches : Branch.t list)
    (ps : Matrix.Entry.t list) : Matrix.t =
  let seen = OT.create 10 in
  let header = Row.create () in
  let occurrences_of (p : Matrix.Entry.t) =
    let map = OT.create 10 in
    let add_occurence o p =
      OT.add map o p;
      if not (OT.mem seen o) then Row.enqueue header o;
      OT.add seen o ()
    in
    begin match p.node with
    | Prod sub_pats ->
        Std.Nonempty_list.iteri
          (fun index (sub_pat : Typed_ir.Pattern.t) ->
            add_occurence
              { path = Project { base; index }; type_ = sub_pat.type_ }
              sub_pat)
          sub_pats
    | _ -> add_occurence base p
    end;
    map
  in
  let occs : Typed_ir.Pattern.t OT.t list = List.map occurrences_of ps in
  let matrix = Matrix.create header in
  List.iter2
    (fun map branch ->
      let row =
        Row.map
          ~f:(fun o ->
            match OT.find_opt map o with
            | Some p -> p
            | _ -> Matrix.Entry.default)
          header
      in
      Matrix.add_row matrix row branch)
    occs branches;
  matrix

type refutability =
  | Irrefutable
  | Destruct (* Tagged constructor — specialize by unwrapping payload *)
  | Switch (* Literal value — specialize by equality, no payload *)

let classify (p : Typed_ir.Pattern.t) : refutability =
  match p.node with
  | Tagged _ -> Destruct
  | Just _ -> Switch
  | Prod _ ->
      (* tuples can't appear at toplevel in matrix because preprocess
         unpacks them *)
      raise (Std.Exceptions.Unreachable [%here])
  | Union _ ->
      Destruct (* not irrefutable; expanded lazily before specialization *)
  | Bind _ | Any -> Irrefutable

let find_refutable ps =
  Dynarray.fold_left
    (fun acc p ->
      match acc with
      | (Destruct | Switch) as r -> r
      | Irrefutable -> classify p)
    Irrefutable ps

(* Unwraps the payload of a constructor pattern. If it's not a constructor
   with a payload, returns a default entry (Any). The inner pattern already
   carries its resolved type from the typed IR. *)
let unwrap (entry : Typed_ir.Pattern.t) : Matrix.Entry.t =
  match entry.node with
  | Tagged (_, Some inner) -> inner
  | _ -> Matrix.Entry.default

(* Filter rows where the first column satisfies [pred], drop that column.
   [unwrap_with]: if provided, unwraps the matched entry into sub-pattern
   columns that are prepended before the remaining columns.
   Accumulates bindings when a Bind pattern is consumed from column 0. *)
let specialize_column (m : Matrix.t) pred ~unwrap_with =
  let col0_occ = Row.peek_exn m.header in
  let kept = Dynarray.create () in
  Col.iter
    (fun (rd : Matrix.row_data) ->
      let entries = Row.copy rd.entries in
      match Row.dequeue entries with
      | Some entry when pred entry ->
          let bindings = Dynarray.copy rd.bindings in
          (match entry.node with
          | Bind name -> Dynarray.add_last bindings (name, col0_occ)
          | _ -> ());
          Dynarray.add_last kept (entry, rd.index, entries, bindings)
      | _ -> ())
    m.rows;
  let kept_rows = Dynarray.to_list kept in
  let branches = List.map (fun (_, b, _, _) -> b) kept_rows in
  let remainders = List.map (fun (_, _, r, _) -> r) kept_rows in
  let bindings_list = List.map (fun (_, _, _, bs) -> bs) kept_rows in
  let remaining_header =
    let h = Row.copy m.header in
    ignore (Row.dequeue h);
    h
  in
  match unwrap_with with
  | None ->
      let m' = Matrix.create (Row.copy remaining_header) in
      List.iter2
        (fun (branch, bs) rem ->
          Matrix.add_row m' rem branch;
          (* add_row creates empty bindings; seed with accumulated ones *)
          let rd = Col.get m'.rows (Col.length m'.rows - 1) in
          Dynarray.append_seq rd.bindings (Dynarray.to_seq bs))
        (List.combine branches bindings_list)
        remainders;
      m'
  | Some unwrap_fn ->
      let pats = List.map (fun (e, _, _, _) -> unwrap_fn e) kept_rows in
      let occ : Occurence.t =
        {
          path = Unwrap col0_occ;
          type_ = (List.hd pats).Typed_ir.Pattern.type_;
        }
      in
      let m' = preprocess occ branches pats in
      Row.iter ~f:(Row.enqueue m'.header) remaining_header;
      let m'_rows = Col.to_list m'.rows in
      List.iter2
        (fun (rd : Matrix.row_data) (rem, bs) ->
          Row.iter ~f:(Row.enqueue rd.entries) rem;
          Dynarray.append_seq rd.bindings (Dynarray.to_seq bs))
        m'_rows
        (List.combine remainders bindings_list);
      m'

(* Specializes the matrix for a specific constructor (Destruct case).
   Filters rows matching [pred], unwraps constructor payloads into
   sub-patterns, and appends the remaining columns.
   For nullary constructors (no payload), skips unwrapping. *)
let specialize_destruct (m : Matrix.t) pred =
  let has_payload =
    let found = ref false in
    Col.iter
      (fun (rd : Matrix.row_data) ->
        let entry = Row.peek_exn rd.entries in
        if pred entry then
          match (entry : Typed_ir.Pattern.t).node with
          | Tagged (_, Some _) -> found := true
          | _ -> ())
      m.rows;
    !found
  in
  specialize_column m pred
    ~unwrap_with:(if has_payload then Some unwrap else None)

(* Specializes the matrix for a specific literal value (Switch case).
   Keeps rows where the first column matches [atom] or is irrefutable,
   then drops the first column (literals have no payload). *)
let specialize_literal (m : Matrix.t) atom =
  specialize_column m
    (fun (entry : Typed_ir.Pattern.t) ->
      match entry.node with
      | Just a -> Syntax.Ast.Atom.equal a atom
      | Bind _ | Any -> true
      | _ -> false)
    ~unwrap_with:None

let is_any (p : Typed_ir.Pattern.t) =
  match p.node with Bind _ | Any -> true | _ -> false

let admits tag (p : Typed_ir.Pattern.t) =
  match p.node with
  | Tagged (tag', _) -> String.equal tag tag'
  | Bind _ | Any -> true
  | _ -> false

let type_name (t : Type.T.t) =
  match t with Con (name, _) -> name | _ -> failwith "Not a valid type!"

let find_refutable_column (m : Matrix.t) =
  match
    Matrix.find_column m ~f:(fun col ->
        match find_refutable col with Irrefutable -> false | _ -> true)
  with
  | Some i -> i
  | None -> failwith "No refutable column!"

let collect_tags col =
  let seen = Hashtbl.create 10 in
  let tags = ref [] in
  Dynarray.iter
    (fun (p : Typed_ir.Pattern.t) ->
      match p.node with
      | Tagged (tag, _) ->
          if not (Hashtbl.mem seen tag) then begin
            Hashtbl.add seen tag ();
            tags := tag :: !tags
          end
      | _ -> ())
    col;
  !tags

let collect_atoms col =
  let atoms = ref [] in
  Dynarray.iter
    (fun (p : Typed_ir.Pattern.t) ->
      match p.node with
      | Just a ->
          if not (List.exists (Syntax.Ast.Atom.equal a) !atoms) then
            atoms := a :: !atoms
      | _ -> ())
    col;
  !atoms

(* Flatten top-level Union into a list of alternatives *)
let rec union_alternatives (p : Typed_ir.Pattern.t) : Typed_ir.Pattern.t list =
  match p.node with
  | Union (lhs, rhs) -> union_alternatives lhs @ union_alternatives rhs
  | _ -> [ p ]

(* If column 0 has any Union entries, split those rows into one row per
   alternative (same branch, same remaining columns). Returns None if no
   unions found, Some matrix' otherwise. *)
let expand_first_column_unions (m : Matrix.t) : Matrix.t option =
  let has_union = ref false in
  Col.iter
    (fun (rd : Matrix.row_data) ->
      match (Row.peek_exn rd.entries : Typed_ir.Pattern.t).node with
      | Union _ -> has_union := true
      | _ -> ())
    m.rows;
  if not !has_union then None
  else
    let m' = Matrix.create (Row.copy m.header) in
    Col.iter
      (fun (rd : Matrix.row_data) ->
        let alts = union_alternatives (Row.peek_exn rd.entries) in
        List.iter
          (fun alt ->
            let entries = Row.copy rd.entries in
            Row.set entries 0 alt;
            Matrix.add_row m' entries rd.index;
            let new_rd = Col.get m'.rows (Col.length m'.rows - 1) in
            Dynarray.append_seq new_rd.bindings (Dynarray.to_seq rd.bindings))
          alts)
      m.rows;
    Some m'

(* Collect bindings from the first row: accumulated bindings plus any
   remaining Bind patterns paired with their column occurrences. *)
let collect_leaf_bindings (matrix : Matrix.t) =
  let rd = Col.get matrix.rows 0 in
  let bindings = Dynarray.copy rd.bindings in
  Row.iteri rd.entries ~f:(fun i entry ->
      match (entry : Typed_ir.Pattern.t).node with
      | Bind name -> Dynarray.add_last bindings (name, Row.get matrix.header i)
      | _ -> ());
  Dynarray.to_list bindings

(* Compile a list of typed patterns into a decision tree (Maranget's algorithm).
   [arities]: type name → number of constructors (for signature completeness).
   [base]: occurrence representing the scrutinee.
   [branches]: branch index for each pattern arm.
   [ps]: one typed pattern per arm. *)
let compile ~arities base (branches : Branch.t list)
    (ps : Typed_ir.Pattern.t list) =
  let module DT = Decision_tree in
  let module DTB = DT.Make () in
  let initial = preprocess base branches ps in
  let rec go (matrix : Matrix.t) : DT.t =
    assert (
      (not (Matrix.is_empty matrix))
      || failwith "empty matrix: non-exhaustive patterns");
    if
      Row.for_all (Col.get matrix.rows 0).entries ~f:(fun p ->
          match classify p with Irrefutable -> true | _ -> false)
    then
      DTB.get
        (Leaf
           {
             branch = (Col.get matrix.rows 0).index;
             bindings = collect_leaf_bindings matrix;
           })
    else begin
      let i = find_refutable_column matrix in
      Matrix.swap_columns matrix 0 i;
      (* Expand any Union entries in column 0 before specializing *)
      match expand_first_column_unions matrix with
      | Some matrix' -> go matrix'
      | None -> (
          let occ = Row.peek_exn matrix.header in
          let first = Matrix.get_column matrix 0 in
          match find_refutable first with
          | Irrefutable -> raise (Std.Exceptions.Unreachable [%here])
          | Destruct ->
              let tags = collect_tags first in
              let cases =
                List.map
                  (fun tag ->
                    (tag, go (specialize_destruct matrix (admits tag))))
                  tags
              in
              let default =
                let tn = type_name occ.type_ in
                if List.length tags < arities tn then
                  Some (go (specialize_column matrix is_any ~unwrap_with:None))
                else None
              in
              DTB.get (DT.Destruct { occurence = occ; cases; default })
          | Switch ->
              let atoms = collect_atoms first in
              let cases =
                List.map
                  (fun atom -> (atom, go (specialize_literal matrix atom)))
                  atoms
              in
              let default =
                go (specialize_column matrix is_any ~unwrap_with:None)
              in
              DTB.get (DT.Switch { occurence = occ; cases; default }))
    end
  in
  go initial
