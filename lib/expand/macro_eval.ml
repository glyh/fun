open Core

let value_tag (v : value) =
  match v with
  | VStx _ -> "VStx"
  | VLam _ -> "VLam"
  | VPi _ -> "VPi"
  | VU -> "VU"
  | VAtom _ -> "VAtom"
  | VAtomTy _ -> "VAtomTy"
  | VProd _ -> "VProd"
  | VProdTy _ -> "VProdTy"
  | VModule _ -> "VModule"
  | VStruct _ -> "VStruct"
  | VRecord _ -> "VRecord"
  | VNominal n -> "VNominal(" ^ n.name ^ ")"
  | VEffect _ -> "VEffect"
  | VTrait _ -> "VTrait"
  | VTraitDict _ -> "VTraitDict"
  | VCon { name; _ } -> "VCon(" ^ name ^ ")"
  | VRefTy _ -> "VRefTy"
  | VRef _ -> "VRef"
  | VCont _ -> "VCont"
  | VNeutral _ -> "VNeutral"
  | VRigid _ -> "VRigid"
  | VFlex _ -> "VFlex"
  | VFix _ -> "VFix"
  | VSelfType _ -> "VSelfType"
  | VEffectRowTy -> "VEffectRowTy"
  | VEffectRow _ -> "VEffectRow"
  | VPatternSyn _ -> "VPatternSyn"

type syntax_nominals = {
  expr : value;
  explicitness : value;
  atom_val : value;
  option_ : value;
  decl : value;
  list : value;
}

let vcon_none nominals =
  VCon { name = "None"; spine = []; nominal = nominals.option_ }

let vcon_some nominals v =
  VCon { name = "Some"; spine = [ v ]; nominal = nominals.option_ }

let vcon_explicit nominals =
  VCon { name = "Explicit"; spine = []; nominal = nominals.explicitness }

let opt_string nominals v = match v with
  | Some s -> vcon_some nominals (VAtom (String s))
  | None -> vcon_none nominals

let opt_i64 nominals v = match v with
  | Some n -> vcon_some nominals (VAtom (I64 (Int64.of_int n)))
  | None -> vcon_none nominals

let span_to_option nominals (span : Source_span.t) : value =
  if span.synthetic then vcon_none nominals
  else
    vcon_some nominals
      (VRecord
         { typ = VU;
           fields =
             [ ("file", opt_string nominals span.file);
               ("start_byte", VAtom (I64 (Int64.of_int span.start_byte)));
               ("end_byte", VAtom (I64 (Int64.of_int span.end_byte)));
               ("start_line", opt_i64 nominals span.start_line);
               ("start_col", opt_i64 nominals span.start_col);
               ("end_line", opt_i64 nominals span.end_line);
               ("end_col", opt_i64 nominals span.end_col) ] })

let id_to_value nominals (id : Syntax.id) : value =
  VRecord { typ = VU;
            fields =
              [ ("name", VAtom (String id.name));
                ("span", span_to_option nominals id.span);
                ("scope", VAtom (I64 0L)) ] }

let explicitness_to_value nominals (e : Explicitness.t) : value =
  match e with
  | Explicitness.Explicit -> vcon_explicit nominals
  | Explicitness.Implicit ->
      VCon { name = "Implicit"; spine = []; nominal = nominals.explicitness }

let atom_to_atomval nominals (a : Atom.t) : value =
  match a with
  | Atom.I64 n -> VCon { name = "I64Atom"; spine = [ VAtom (I64 n) ]; nominal = nominals.atom_val }
  | Atom.Bool b -> VCon { name = "BoolAtom"; spine = [ VAtom (Bool b) ]; nominal = nominals.atom_val }
  | Atom.Char c -> VCon { name = "CharAtom"; spine = [ VAtom (Char c) ]; nominal = nominals.atom_val }
  | Atom.String s -> VCon { name = "StringAtom"; spine = [ VAtom (String s) ]; nominal = nominals.atom_val }
  | Atom.Unit -> VCon { name = "UnitAtom"; spine = []; nominal = nominals.atom_val }

let atomval_to_atom (v : value) : Atom.t option =
  match v with
  | VCon { name = "I64Atom"; spine = [ VAtom (I64 n) ]; _ } -> Some (Atom.I64 n)
  | VCon { name = "BoolAtom"; spine = [ VAtom (Bool b) ]; _ } -> Some (Atom.Bool b)
  | VCon { name = "CharAtom"; spine = [ VAtom (Char c) ]; _ } -> Some (Atom.Char c)
  | VCon { name = "StringAtom"; spine = [ VAtom (String s) ]; _ } -> Some (Atom.String s)
  | VCon { name = "UnitAtom"; spine = []; _ } -> Some Atom.Unit
  | _ -> None

let rec param_to_value nominals (p : Syntax.param) : value =
  VRecord
    { typ = VU;
      fields =
        [ ("name", id_to_value nominals p.name);
          ("type_", (match p.type_ with
                     | Some t -> vcon_some nominals (wrap_stx_sub nominals t)
                     | None -> vcon_none nominals));
          ("explicitness", explicitness_to_value nominals p.explicitness) ] }

and wrap_stx_sub nominals (stx : Syntax.t) : value =
  let span_opt = span_to_option nominals stx.span in
  match stx.kind with
  | Var id ->
      VCon { name = "RawVar"; spine = [ span_opt; id_to_value nominals id ];
             nominal = nominals.expr }
  | Atom a ->
      VCon { name = "RawAtom"; spine = [ span_opt; atom_to_atomval nominals a ];
             nominal = nominals.expr }
  | Ap (f, e, a) ->
      VCon { name = "RawAp";
             spine = [ span_opt; wrap_stx_sub nominals f;
                       explicitness_to_value nominals e; wrap_stx_sub nominals a ];
             nominal = nominals.expr }
  | Lam (p, body) ->
      VCon { name = "RawLam";
             spine = [ span_opt; param_to_value nominals p;
                       wrap_stx_sub nominals body ];
             nominal = nominals.expr }
  | Let { name; type_; value; body; recursive } ->
      let type_val = match type_ with
        | Some t -> vcon_some nominals (wrap_stx_sub nominals t)
        | None -> vcon_none nominals
      in
      VCon { name = "RawLet";
             spine =
               [ span_opt; id_to_value nominals name; type_val;
                 wrap_stx_sub nominals value;
                 wrap_stx_sub nominals body;
                 VAtom (if recursive then Bool true else Bool false) ];
             nominal = nominals.expr }
  | _ ->
      VStx (StxExpr stx)

let wrap_stx ~nominals (stx : Syntax.t) : value =
  match nominals with
  | None -> VStx (StxExpr stx)
  | Some nominals ->
  let span_opt = span_to_option nominals stx.span in
  match stx.kind with
  | Var id ->
      VCon { name = "RawVar"; spine = [ span_opt; id_to_value nominals id ];
             nominal = nominals.expr }
  | Atom a ->
      VCon { name = "RawAtom"; spine = [ span_opt; atom_to_atomval nominals a ];
             nominal = nominals.expr }
  | Ap (f, e, a) ->
      VCon { name = "RawAp";
             spine = [ span_opt; wrap_stx_sub nominals f;
                       explicitness_to_value nominals e; wrap_stx_sub nominals a ];
             nominal = nominals.expr }
  | Lam (p, body) ->
      VCon { name = "RawLam";
             spine = [ span_opt; param_to_value nominals p;
                       wrap_stx_sub nominals body ];
             nominal = nominals.expr }
  | Let { name; type_; value; body; recursive } ->
      let type_val = match type_ with
        | Some t -> vcon_some nominals (wrap_stx_sub nominals t)
        | None -> vcon_none nominals
      in
      VCon { name = "RawLet";
             spine =
               [ span_opt; id_to_value nominals name; type_val;
                 wrap_stx_sub nominals value;
                 wrap_stx_sub nominals body;
                  VAtom (if recursive then Bool true else Bool false) ];
             nominal = nominals.expr }
  | _ ->
      VStx (StxExpr stx)

let rec value_to_id (v : value) : Syntax.id =
  match v with
  | VRecord { fields; _ } ->
      let name = match List.assoc_opt "name" fields with
        | Some (VAtom (String n)) -> n | _ -> "?"
      in
      let span = match List.assoc_opt "span" fields with
        | Some s -> value_to_span s | None -> Source_span.synthetic
      in
      { name; span; scope = Scope_set.empty }
  | _ ->
      { name = "?"; span = Source_span.synthetic; scope = Scope_set.empty }

and value_to_span (v : value) : Source_span.t =
  match v with
  | VCon { name = "None"; _ } -> Source_span.synthetic
  | VCon { name = "Some"; spine = [ VRecord ({ fields; _ } as _rec) ]; _ } ->
      let file = match List.assoc_opt "file" fields with
        | Some (VCon { name = "Some"; spine = [ VAtom (String s) ]; _ }) -> Some s
        | _ -> None
      in
      let start_byte = match List.assoc_opt "start_byte" fields with
        | Some (VAtom (I64 n)) -> Int64.to_int n | _ -> 0
      in
      let end_byte = match List.assoc_opt "end_byte" fields with
        | Some (VAtom (I64 n)) -> Int64.to_int n | _ -> 0
      in
      { file; start_byte; end_byte;
        start_line = None; start_col = None; end_line = None; end_col = None;
        synthetic = false }
  | _ -> Source_span.synthetic

and value_to_param (v : value) : Syntax.param =
  match v with
  | VRecord { fields; _ } ->
      let name = match List.assoc_opt "name" fields with
        | Some n -> value_to_id n
        | None -> { name = "?"; span = Source_span.synthetic; scope = Scope_set.empty }
      in
      let explicitness = match List.assoc_opt "explicitness" fields with
        | Some (VCon { name = "Explicit"; _ }) -> Explicitness.Explicit
        | _ -> Explicitness.Implicit
      in
      { name; type_ = None; trait_bounds = []; explicitness }
  | _ ->
      { name = { name = "?"; span = Source_span.synthetic; scope = Scope_set.empty };
        type_ = None; trait_bounds = []; explicitness = Explicitness.Explicit }

and value_to_bool (v : value) : bool =
  match v with VAtom (Bool b) -> b | _ -> false

let rec unwind_stx (v : value) : Syntax.t option =
  match v with
  | VCon { name = "RawVar"; spine = [ span_val; id_val ]; _ } ->
      Some { kind = Var (value_to_id id_val); span = value_to_span span_val }
  | VCon { name = "RawAtom"; spine = [ span_val; atom_val ]; _ } ->
      (match atomval_to_atom atom_val with
       | Some a -> Some { kind = Atom a; span = value_to_span span_val }
       | None -> None)
  | VCon { name = "RawAp"; spine = [ span_val; fn_val; _; arg_val ]; _ } ->
      (match (unwind_stx fn_val, unwind_stx arg_val) with
       | Some fn, Some arg ->
           Some { kind = Ap (fn, Explicitness.Explicit, arg); span = value_to_span span_val }
       | _ -> None)
  | VCon { name = "RawLam"; spine = [ span_val; param_val; body_val ]; _ } ->
      (match unwind_stx body_val with
       | Some body ->
           Some { kind = Lam (value_to_param param_val, body); span = value_to_span span_val }
       | _ -> None)
  | VCon { name = "RawLet"; spine = [ span_val; name_val; _; value_val; body_val; rec_val ]; _ } ->
      (match (unwind_stx value_val, unwind_stx body_val) with
       | Some value, Some body ->
           Some { kind = Let { name = value_to_id name_val;
                              type_ = None; value; body;
                              recursive = value_to_bool rec_val };
                  span = value_to_span span_val }
       | _ -> None)
  | VStx (StxExpr stx) -> Some stx
  | _ -> None

let unwrap_stx (v : value) : Syntax.t option = unwind_stx v

let unwrap_stx_decl (v : value) : Syntax.struct_binding list option =
  let rec go v =
    match v with
    | VCon { name = "DeclLet"; spine = [ id_val; expr_val; pub_val ]; _ } ->
        (match unwind_stx expr_val with
         | Some expr ->
             let name = value_to_id id_val in
             let public = value_to_bool pub_val in
             Some [ Syntax.LetBinding { name; value = expr; public; recursive = false } ]
         | None -> None)
    | VCon { name = "Nil"; _ } -> Some []
    | VCon { name = "Cons"; spine = [ head; tail ]; _ } ->
        (match go head, go tail with
         | Some hd, Some tl -> Some (hd @ tl)
         | _ -> None)
    | VStx (StxDecl binding) -> Some [ binding ]
    | VStx (StxDecls bindings) -> Some bindings
    | VStx (StxExpr stx) -> (
        match stx.kind with
        | Syntax.Module { bindings } | Syntax.Struct { bindings; _ } -> Some bindings
        | _ -> None)
    | _ -> None
  in
  go v

let wrap_stx_decl ~nominals (bindings : Syntax.struct_binding list) : value =
  match nominals with
  | None ->
      (match bindings with
       | [ b ] -> VStx (StxDecl b)
       | _ -> VStx (StxDecls bindings))
  | Some n ->
      let rec go = function
        | [] -> VCon { name = "Nil"; spine = []; nominal = n.list }
        | Syntax.LetBinding { name; value; public; _ } :: rest ->
            let id_val = id_to_value n name in
            let expr_val = wrap_stx_sub n value in
            let pub_val = VAtom (if public then Bool true else Bool false) in
            let head = VCon { name = "DeclLet"; spine = [ id_val; expr_val; pub_val ]; nominal = n.decl } in
            VCon { name = "Cons"; spine = [ head; go rest ]; nominal = n.list }
        | _ :: rest -> go rest  (* skip non-LetBindings for now *)
      in
      go bindings
