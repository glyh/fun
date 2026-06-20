open Core

let dummy_opt_nominal =
  VNominal { id = NominalId.fresh (); name = "Option";
             num_params = 0; params = []; constructors = [] }

let dummy_expl_nominal =
  VNominal { id = NominalId.fresh (); name = "Explicitness";
             num_params = 0; params = []; constructors = [] }

let dummy_expr_nominal =
  VNominal { id = NominalId.fresh (); name = "Expr";
             num_params = 0; params = []; constructors = [] }

let vcon_none =
  VCon { name = "None"; spine = []; nominal = dummy_opt_nominal }

let vcon_some v =
  VCon { name = "Some"; spine = [ v ]; nominal = dummy_opt_nominal }

let vcon_explicit =
  VCon { name = "Explicit"; spine = []; nominal = dummy_expl_nominal }

let vcon_implicit =
  VCon { name = "Implicit"; spine = []; nominal = dummy_expl_nominal }

let opt_string v = match v with
  | Some s -> vcon_some (VAtom (String s))
  | None -> vcon_none

let opt_i64 v = match v with
  | Some n -> vcon_some (VAtom (I64 (Int64.of_int n)))
  | None -> vcon_none

let span_to_option (span : Source_span.t) : value =
  if span.synthetic then vcon_none
  else
    vcon_some
      (VRecord
         { typ = VU;
           fields =
             [ ("file", opt_string span.file);
               ("start_byte", VAtom (I64 (Int64.of_int span.start_byte)));
               ("end_byte", VAtom (I64 (Int64.of_int span.end_byte)));
               ("start_line", opt_i64 span.start_line);
               ("start_col", opt_i64 span.start_col);
               ("end_line", opt_i64 span.end_line);
               ("end_col", opt_i64 span.end_col) ] })

let id_to_value (id : Syntax.id) : value =
  VRecord { typ = VU;
            fields =
              [ ("name", VAtom (String id.name));
                ("span", span_to_option id.span);
                ("scope", VAtom (I64 0L)) ] }

let explicitness_to_value (e : Explicitness.t) : value =
  match e with
  | Explicitness.Explicit -> vcon_explicit
  | Explicitness.Implicit -> vcon_implicit

let atom_to_atomval (a : Atom.t) : value =
  match a with
  | Atom.I64 n -> VCon { name = "I64Atom"; spine = [ VAtom (I64 n) ]; nominal = dummy_expr_nominal }
  | Atom.Bool b -> VCon { name = "BoolAtom"; spine = [ VAtom (Bool b) ]; nominal = dummy_expr_nominal }
  | Atom.Char c -> VCon { name = "CharAtom"; spine = [ VAtom (Char c) ]; nominal = dummy_expr_nominal }
  | Atom.String s -> VCon { name = "StringAtom"; spine = [ VAtom (String s) ]; nominal = dummy_expr_nominal }
  | Atom.Unit -> VCon { name = "UnitAtom"; spine = []; nominal = dummy_expr_nominal }

let atomval_to_atom (v : value) : Atom.t option =
  match v with
  | VCon { name = "I64Atom"; spine = [ VAtom (I64 n) ]; _ } -> Some (Atom.I64 n)
  | VCon { name = "BoolAtom"; spine = [ VAtom (Bool b) ]; _ } -> Some (Atom.Bool b)
  | VCon { name = "CharAtom"; spine = [ VAtom (Char c) ]; _ } -> Some (Atom.Char c)
  | VCon { name = "StringAtom"; spine = [ VAtom (String s) ]; _ } -> Some (Atom.String s)
  | VCon { name = "UnitAtom"; spine = []; _ } -> Some Atom.Unit
  | _ -> None

let rec param_to_value (p : Syntax.param) : value =
  VRecord
    { typ = VU;
      fields =
        [ ("name", id_to_value p.name);
          ("type_", (match p.type_ with
                     | Some t -> vcon_some (wrap_stx_sub t)
                     | None -> vcon_none));
          ("explicitness", explicitness_to_value p.explicitness) ] }

and wrap_stx_sub (stx : Syntax.t) : value =
  let span_opt = span_to_option stx.span in
  match stx.kind with
  | Var id ->
      VCon { name = "RawVar"; spine = [ span_opt; id_to_value id ];
             nominal = dummy_expr_nominal }
  | Atom a ->
      VCon { name = "RawAtom"; spine = [ span_opt; atom_to_atomval a ];
             nominal = dummy_expr_nominal }
  | Ap (f, e, a) ->
      VCon { name = "RawAp";
             spine = [ span_opt; wrap_stx_sub f; explicitness_to_value e; wrap_stx_sub a ];
             nominal = dummy_expr_nominal }
  | Lam (p, body) ->
      VCon { name = "RawLam";
             spine = [ span_opt; param_to_value p; wrap_stx_sub body ];
             nominal = dummy_expr_nominal }
  | Let { name; type_; value; body; recursive } ->
      let type_val = match type_ with
        | Some t -> vcon_some (wrap_stx_sub t)
        | None -> vcon_none
      in
      VCon { name = "RawLet";
             spine =
               [ span_opt; id_to_value name; type_val;
                 wrap_stx_sub value; wrap_stx_sub body;
                 VAtom (if recursive then Bool true else Bool false) ];
             nominal = dummy_expr_nominal }
  | _ ->
      VStx (StxExpr stx)

let wrap_stx ~syntax_nominal (stx : Syntax.t) : value =
  match syntax_nominal with
  | Some nominal ->
      let span_opt = span_to_option stx.span in
      (match stx.kind with
       | Var id ->
           VCon { name = "RawVar"; spine = [ span_opt; id_to_value id ]; nominal }
       | Atom a ->
           VCon { name = "RawAtom"; spine = [ span_opt; atom_to_atomval a ]; nominal }
       | Ap (f, e, a) ->
           VCon { name = "RawAp";
                  spine = [ span_opt; wrap_stx_sub f; explicitness_to_value e; wrap_stx_sub a ];
                  nominal }
       | Lam (p, body) ->
           VCon { name = "RawLam";
                  spine = [ span_opt; param_to_value p; wrap_stx_sub body ];
                  nominal }
       | Let { name; type_; value; body; recursive } ->
           let type_val = match type_ with
             | Some t -> vcon_some (wrap_stx_sub t)
             | None -> vcon_none
           in
           VCon { name = "RawLet";
                  spine =
                    [ span_opt; id_to_value name; type_val;
                      wrap_stx_sub value; wrap_stx_sub body;
                      VAtom (if recursive then Bool true else Bool false) ];
                  nominal }
       | _ ->
           VStx (StxExpr stx))
  | None ->
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
