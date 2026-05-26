open Core

let rec pp_term (t : term) : string =
  match t with
  | Var ix -> Printf.sprintf "Var(%d)" ix
  | Lam body -> Printf.sprintf "Lam(%s)" (pp_term body)
  | Ap (f, _, a) -> Printf.sprintf "Ap(%s, %s)" (pp_term f) (pp_term a)
  | Let (_, def, body) -> Printf.sprintf "Let(%s, %s)" (pp_term def) (pp_term body)
  | Pi { explicitness; domain; effects; codomain } ->
      let row =
        if is_empty_effect_row effects then ""
        else
          Printf.sprintf " can %s"
            (String.concat ", " (List.map pp_term effects.effects))
      in
      (match explicitness with
      | Implicit ->
          Printf.sprintf "Pi{%s, %s%s}" (pp_term domain) (pp_term codomain) row
      | Explicit ->
          Printf.sprintf "Pi(%s, %s%s)" (pp_term domain) (pp_term codomain) row)
  | U -> "U"
  | Atom _ -> "Atom"
  | AtomTy _ -> "AtomTy"
  | Meta id -> Printf.sprintf "Meta(%d)" id
  | InsertedMeta (id, _) -> Printf.sprintf "IMeta(%d)" id
  | NomRef (name, args) -> Printf.sprintf "NomRef(%s, [%s])" name (String.concat "," (List.map pp_term args))
  | EffectRef (name, args) -> Printf.sprintf "EffectRef(%s, [%s])" name (String.concat "," (List.map pp_term args))
  | Con name -> Printf.sprintf "Con(%s)" name
  | Ctor { name; nominal_name; _ } -> Printf.sprintf "Ctor(%s/%s)" name nominal_name
  | EffectDef { name; body; _ } -> Printf.sprintf "EffectDef(%s, %s)" name (pp_term body)
  | Perform { eff; op; arg } ->
      Printf.sprintf "Perform(%s.%s, %s)" (pp_term eff) op (pp_term arg)
  | _ -> "<term>"

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
    | VPi { explicitness; domain; effects; _ } ->
        let arrow =
          match explicitness with
          | Implicit -> Printf.sprintf "{%s} -> ..." (go (depth + 1) domain)
          | Explicit -> Printf.sprintf "%s -> ..." (go (depth + 1) domain)
        in
        if List.is_empty effects.effects && Option.is_none effects.tail then arrow
        else
          Printf.sprintf "%s can %s" arrow
            (String.concat ", " (List.map pp_term effects.effects))
    | VNominal n ->
        if List.length n.params = 0 then n.name
        else Printf.sprintf "%s(%s)" n.name
          (String.concat ", " (List.map (go (depth+1)) n.params))
    | VEffect e ->
        if List.length e.params = 0 then Printf.sprintf "effect %s" e.name
        else Printf.sprintf "effect %s(%s)" e.name
          (String.concat ", " (List.map (go (depth+1)) e.params))
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
