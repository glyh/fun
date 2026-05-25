open Core

let pp_value_short (mc : MetaContext.t) (v : value) : string =
  let rec go depth v =
    if depth > 4 then "..." else
    match v with
    | VU -> "U"
    | VAtom (I64 n) -> Int64.to_string n
    | VAtom (Bool b) -> string_of_bool b
    | VAtom Unit -> "()"
    | VAtom (Char c) -> Atom.pp (Char c)
    | VAtomTy TI64 -> "I64"
    | VAtomTy TBool -> "Bool"
    | VAtomTy TUnit -> "Unit"
    | VAtomTy TChar -> "Char"
    | VFlex { id; spine } ->
        let base = Printf.sprintf "?%d" id in
        let solved = match MetaContext.lookup mc id with
          | Solved sv -> Printf.sprintf "[=%s]" (go (depth+1) sv)
          | Unsolved -> ""
        in
        if List.length spine > 0 then
          Printf.sprintf "%s%s(%s)" base solved
            (String.concat ", " (List.map (go (depth+1)) spine))
        else base ^ solved
    | VRigid { lvl; spine } ->
        let base = Printf.sprintf "#%d" lvl in
        if List.length spine > 0 then
          Printf.sprintf "%s(%s)" base
            (String.concat ", " (List.map (go (depth+1)) spine))
        else base
    | VLam _ -> "<lam>"
    | VPi { explicitness = Implicit; domain; _ } ->
        Printf.sprintf "{%s} -> ..." (go (depth+1) domain)
    | VPi { explicitness = Explicit; domain; _ } ->
        Printf.sprintf "%s -> ..." (go (depth+1) domain)
    | VNominal n ->
        if List.length n.params = 0 then n.name
        else Printf.sprintf "%s(%s)" n.name
          (String.concat ", " (List.map (go (depth+1)) n.params))
    | VCon { name; spine; _ } ->
        if List.length spine = 0 then name
        else Printf.sprintf "%s(%s)" name
          (String.concat ", " (List.map (go (depth+1)) spine))
    | VProd elems ->
        Printf.sprintf "(%s)" (String.concat ", " (List.map (go (depth+1)) elems))
    | VProdTy elems ->
        Printf.sprintf "(%s)" (String.concat " * " (List.map (go (depth+1)) elems))
    | VStruct _ -> "<struct>"
    | VRecord _ -> "<record>"
    | VFix _ -> "<fix>"
    | VNeutral _ -> "<neutral>"
  in
  go 0 v

let pp_meta_state (mc : MetaContext.t) (id : meta_id) : string =
  match MetaContext.lookup mc id with
  | Solved v -> Printf.sprintf "?%d = %s" id (pp_value_short mc v)
  | Unsolved -> Printf.sprintf "?%d unsolved" id

let dump_metas (mc : MetaContext.t) : unit =
  let n = Dynarray.length mc in
  Printf.eprintf "--- Meta state (%d metas) ---\n%!" n;
  for i = 0 to n - 1 do
    Printf.eprintf "  %s\n%!" (pp_meta_state mc i)
  done;
  Printf.eprintf "---\n%!"

let rec pp_term (t : term) : string =
  match t with
  | Var ix -> Printf.sprintf "Var(%d)" ix
  | Lam body -> Printf.sprintf "Lam(%s)" (pp_term body)
  | Ap (f, _, a) -> Printf.sprintf "Ap(%s, %s)" (pp_term f) (pp_term a)
  | Let (_, def, body) -> Printf.sprintf "Let(%s, %s)" (pp_term def) (pp_term body)
  | Pi (Implicit, a, b) -> Printf.sprintf "Pi{%s, %s}" (pp_term a) (pp_term b)
  | Pi (Explicit, a, b) -> Printf.sprintf "Pi(%s, %s)" (pp_term a) (pp_term b)
  | U -> "U"
  | Atom _ -> "Atom"
  | AtomTy _ -> "AtomTy"
  | Meta id -> Printf.sprintf "Meta(%d)" id
  | InsertedMeta (id, _) -> Printf.sprintf "IMeta(%d)" id
  | NomRef (name, args) -> Printf.sprintf "NomRef(%s, [%s])" name (String.concat "," (List.map pp_term args))
  | Con name -> Printf.sprintf "Con(%s)" name
  | Ctor { name; nominal_name; _ } -> Printf.sprintf "Ctor(%s/%s)" name nominal_name
  | _ -> "<term>"
