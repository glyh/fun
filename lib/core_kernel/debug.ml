open Core

let rec pp_effect_row (row : effect_row) =
  let effects = List.map pp_term row.effects in
  match row.tail with
  | None -> String.concat ", " effects
  | Some tail -> String.concat ", " effects ^ " | " ^ pp_term tail

and pp_term (t : term) : string =
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
  | EffectRowTy -> "EffectRowTy"
  | EffectRowLit row -> Printf.sprintf "EffectRowLit(%s)" (pp_effect_row row)
  | Atom _ -> "Atom"
  | AtomTy _ -> "AtomTy"
  | Meta id -> Printf.sprintf "Meta(%d)" id
  | InsertedMeta (id, _) -> Printf.sprintf "IMeta(%d)" id
  | NomRef (name, args) -> Printf.sprintf "NomRef(%s, [%s])" name (String.concat "," (List.map pp_term args))
  | EffectRef (name, args) -> Printf.sprintf "EffectRef(%s, [%s])" name (String.concat "," (List.map pp_term args))
  | TraitRef { trait_name; _ } -> Printf.sprintf "TraitRef(%s)" trait_name
  | TraitDictTy { trait_name; args; _ } ->
      Printf.sprintf "TraitDictTy(%s, [%s])" trait_name (String.concat "," (List.map pp_term args))
  | SelfTypeRef args -> Printf.sprintf "SelfTypeRef([%s])" (String.concat "," (List.map pp_term args))
  | Con name -> Printf.sprintf "Con(%s)" name
  | Ctor { name; nominal_name; _ } -> Printf.sprintf "Ctor(%s/%s)" name nominal_name
  | EffectDef { name; body; _ } -> Printf.sprintf "EffectDef(%s, %s)" name (pp_term body)
  | Perform { eff; op; arg } ->
      Printf.sprintf "Perform(%s.%s, %s)" (pp_term eff) op (pp_term arg)
  | RefTy a -> Printf.sprintf "RefTy(%s)" (pp_term a)
  | RefNew e -> Printf.sprintf "RefNew(%s)" (pp_term e)
  | RefGet e -> Printf.sprintf "RefGet(%s)" (pp_term e)
  | RefSet (r, e) -> Printf.sprintf "RefSet(%s, %s)" (pp_term r) (pp_term e)
  | _ -> "<term>"

let pp_value_short (mc : MetaContext.t) (v : value) : string =
  let rec go depth v =
    if depth > 4 then "..." else
    match v with
    | VU -> "U"
    | VEffectRowTy -> "EffectRow"
    | VEffectRow row ->
        let effects = List.map (go (depth + 1)) row.effect_values in
        let tail = Option.map (go (depth + 1)) row.tail_value in
        Printf.sprintf "{%s%s}" (String.concat ", " effects)
          (match tail with None -> "" | Some tail -> " | " ^ tail)
    | VAtom (I64 n) -> Int64.to_string n
    | VAtom (Bool b) -> string_of_bool b
    | VAtom Unit -> "()"
    | VAtom (Char c) -> Atom.pp (Char c)
    | VAtom (String s) -> Atom.pp (String s)
    | VAtomTy TI64 -> "I64"
    | VAtomTy TBool -> "Bool"
    | VAtomTy TUnit -> "Unit"
    | VAtomTy TChar -> "Char"
    | VAtomTy TString -> "String"
    | VAtomTy TAbsurd -> "Absurd"
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
    | VTrait t -> Printf.sprintf "trait %s" t.trait_name
    | VTraitDict d ->
        Printf.sprintf "<impl %s%s>" d.trait_name
          (if List.is_empty d.args then "" else " " ^ String.concat " " (List.map (go (depth + 1)) d.args))
    | VSelfType args ->
        if List.is_empty args then "Self" else Printf.sprintf "Self(%s)" (String.concat ", " (List.map (go (depth + 1)) args))
    | VRefTy a -> Printf.sprintf "Ref %s" (go (depth + 1) a)
    | VRef _ -> "<ref>"
    | VCon { name; spine; _ } ->
        if List.length spine = 0 then name
        else Printf.sprintf "%s(%s)" name
          (String.concat ", " (List.map (go (depth+1)) spine))
    | VProd elems ->
        Printf.sprintf "(%s)" (String.concat ", " (List.map (go (depth+1)) elems))
    | VProdTy elems ->
        Printf.sprintf "(%s)" (String.concat " * " (List.map (go (depth+1)) elems))
    | VModule _ -> "<module>"
    | VStruct _ -> "<struct>"
    | VRecord _ -> "<record>"
    | VCont _ -> "<continuation>"
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
