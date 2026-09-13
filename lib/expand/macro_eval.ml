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
  pat : value;
  r_ : value;
  bool : value;
}

(* [Bool] is a library ADT; reflected boolean flags (a [let]'s recursive/public
   markers) are its nullary constructors [True]/[False], not atoms. *)
let vcon_bool nominals b =
  VCon { name = (if b then "True" else "False"); spine = []; nominal = nominals.bool }

let vcon_none nominals =
  VCon { name = Compiler_names.Constructor_name.none; spine = []; nominal = nominals.option_ }

let vcon_some nominals v =
  VCon { name = Compiler_names.Constructor_name.some; spine = [ v ]; nominal = nominals.option_ }

let same_nominal expected actual =
  match expected, actual with
  | VNominal e, VNominal a -> e.id = a.id
  | _ -> false

let con_matches ?nominals nominal_of_expected expected_name actual_name actual_nominal =
  String.equal actual_name expected_name
  &&
  match nominals with
  | None -> false
  | Some n -> same_nominal (nominal_of_expected n) actual_nominal

let list_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.list) name nominal

let option_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.option_) name nominal

let expr_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.expr) name nominal

let pat_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.pat) name nominal

let decl_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.decl) name nominal

let atom_val_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.atom_val) name nominal

let explicitness_con_matches ?nominals name nominal =
  con_matches ?nominals (fun n -> n.explicitness) name nominal

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
                ("scope", VAtom (Scopes id.scope)) ] }

let explicitness_to_value nominals (e : Explicitness.t) : value =
  match e with
  | Explicitness.Explicit -> vcon_explicit nominals
  | Explicitness.Implicit ->
      VCon { name = "Implicit"; spine = []; nominal = nominals.explicitness }

let atom_to_atomval nominals (a : Atom.t) : value =
  match a with
  | Atom.I64 n -> VCon { name = "I64Atom"; spine = [ VAtom (I64 n) ]; nominal = nominals.atom_val }
  | Atom.Char c -> VCon { name = "CharAtom"; spine = [ VAtom (Char c) ]; nominal = nominals.atom_val }
  | Atom.String s -> VCon { name = "StringAtom"; spine = [ VAtom (String s) ]; nominal = nominals.atom_val }
  | Atom.Unit -> VCon { name = "UnitAtom"; spine = []; nominal = nominals.atom_val }
  | Atom.Scopes s -> VCon { name = "ScopesAtom"; spine = [ VAtom (Scopes s) ]; nominal = nominals.atom_val }

let atomval_to_atom ?nominals (v : value) : Atom.t option =
  match v with
  | VCon { name; spine = [ VAtom (I64 n) ]; nominal }
    when atom_val_con_matches ?nominals "I64Atom" name nominal -> Some (Atom.I64 n)
  | VCon { name; spine = [ VAtom (Char c) ]; nominal }
    when atom_val_con_matches ?nominals "CharAtom" name nominal -> Some (Atom.Char c)
  | VCon { name; spine = [ VAtom (String s) ]; nominal }
    when atom_val_con_matches ?nominals "StringAtom" name nominal -> Some (Atom.String s)
  | VCon { name; spine = []; nominal }
    when atom_val_con_matches ?nominals "UnitAtom" name nominal -> Some Atom.Unit
  | VCon { name; spine = [ VAtom (Scopes s) ]; nominal }
    when atom_val_con_matches ?nominals "ScopesAtom" name nominal -> Some (Atom.Scopes s)
  | _ -> None

let list_to_value nominals items =
  List.fold_right
    (fun h t -> VCon { name = "Cons"; spine = [ h; t ]; nominal = nominals.list })
    items
    (VCon { name = "Nil"; spine = []; nominal = nominals.list })

let option_to_value nominals f = function
  | Some x -> vcon_some nominals (f x)
  | None -> vcon_none nominals

(* Syntax.t -> reflection value. Every field is carried; a form the reflection
   ADTs do not yet decompose rides as an undecomposed [VStx] (scaffolding, M1). *)
let rec param_to_value nominals (p : Syntax.param) : value =
  VRecord
    { typ = VU;
      fields =
        [ ("name", id_to_value nominals p.name);
          ("type_", option_to_value nominals (wrap_stx_sub nominals) p.type_);
          ("explicitness", explicitness_to_value nominals p.explicitness) ] }

and wrap_stx_sub nominals (stx : Syntax.t) : value =
  let span_opt = span_to_option nominals stx.span in
  let expr name spine = VCon { name; spine = span_opt :: spine; nominal = nominals.expr } in
  match stx.kind with
  | Var id -> expr "RawVar" [ id_to_value nominals id ]
  | Atom a -> expr "RawAtom" [ atom_to_atomval nominals a ]
  | Ap (f, e, a) ->
      expr "RawAp" [ wrap_stx_sub nominals f; explicitness_to_value nominals e; wrap_stx_sub nominals a ]
  | Lam (p, body) -> expr "RawLam" [ param_to_value nominals p; wrap_stx_sub nominals body ]
  | Let { name; type_; value; body; recursive } ->
      expr "RawLet"
        [ id_to_value nominals name;
          option_to_value nominals (wrap_stx_sub nominals) type_;
          wrap_stx_sub nominals value;
          wrap_stx_sub nominals body;
          vcon_bool nominals recursive ]
  | _ -> VStx (StxExpr stx)

let wrap_stx ~nominals (stx : Syntax.t) : value =
  match nominals with
  | None -> VStx (StxExpr stx)
  | Some nominals -> wrap_stx_sub nominals stx

(* Reflection value -> Syntax.t. The inverse of the above on every field; a
   value that is not well-formed reflection is [None], never a guess. *)
let ( let* ) = Option.bind

let rec option_all = function
  | [] -> Some []
  | x :: xs -> let* x = x in let* xs = option_all xs in Some (x :: xs)

let field name fields = List.assoc_opt name fields

let value_to_i64 = function VAtom (I64 n) -> Some (Int64.to_int n) | _ -> None

(* The payload of a constructor of a parameterised type: a constructor built by
   macro code carries its type arguments first in the spine, one built by
   reflection does not, so the payload is the trailing [n] elements. *)
let payload n spine =
  let len = List.length spine in
  if len < n then None else Some (List.filteri (fun i _ -> i >= len - n) spine)

let value_to_option ?nominals f (v : value) =
  match v with
  | VCon { name; nominal; _ }
    when option_con_matches ?nominals Compiler_names.Constructor_name.none name nominal -> Some None
  | VCon { name; spine; nominal }
    when option_con_matches ?nominals Compiler_names.Constructor_name.some name nominal ->
      (match payload 1 spine with
       | Some [ x ] -> let* x = f x in Some (Some x)
       | _ -> None)
  | _ -> None

let rec value_to_list ?nominals (v : value) : value list option =
  match v with
  | VCon { name; nominal; _ } when list_con_matches ?nominals "Nil" name nominal -> Some []
  | VCon { name; spine; nominal } when list_con_matches ?nominals "Cons" name nominal ->
      (match payload 2 spine with
       | Some [ head; tail ] -> let* tail = value_to_list ?nominals tail in Some (head :: tail)
       | _ -> None)
  | _ -> None

let value_to_bool ?nominals (v : value) : bool option =
  match v, nominals with
  | VCon { name = "True"; spine = []; nominal }, Some n when same_nominal n.bool nominal -> Some true
  | VCon { name = "False"; spine = []; nominal }, Some n when same_nominal n.bool nominal -> Some false
  | _ -> None

let value_to_span ?nominals (v : value) : Source_span.t option =
  let* span = value_to_option ?nominals (function VRecord { fields; _ } -> Some fields | _ -> None) v in
  match span with
  | None -> Some Source_span.synthetic
  | Some fields ->
      let opt_i64 name =
        let* v = field name fields in
        value_to_option ?nominals value_to_i64 v
      in
      let* file = let* v = field "file" fields in
        value_to_option ?nominals (function VAtom (String s) -> Some s | _ -> None) v in
      let* start_byte = let* v = field "start_byte" fields in value_to_i64 v in
      let* end_byte = let* v = field "end_byte" fields in value_to_i64 v in
      let* start_line = opt_i64 "start_line" in
      let* start_col = opt_i64 "start_col" in
      let* end_line = opt_i64 "end_line" in
      let* end_col = opt_i64 "end_col" in
      Some { Source_span.file; start_byte; end_byte; start_line; start_col; end_line; end_col;
             synthetic = false }

let value_to_id ?nominals (v : value) : Syntax.id option =
  match v with
  | VRecord { fields; _ } ->
      let* name = match field "name" fields with Some (VAtom (String n)) -> Some n | _ -> None in
      let* span = let* s = field "span" fields in value_to_span ?nominals s in
      let* scope = match field "scope" fields with Some (VAtom (Scopes s)) -> Some s | _ -> None in
      Some { Syntax.name; span; scope }
  | _ -> None

let value_to_explicitness ?nominals (v : value) : Explicitness.t option =
  match v with
  | VCon { name; spine = []; nominal } when explicitness_con_matches ?nominals "Explicit" name nominal ->
      Some Explicitness.Explicit
  | VCon { name; spine = []; nominal } when explicitness_con_matches ?nominals "Implicit" name nominal ->
      Some Explicitness.Implicit
  | _ -> None

let rec value_to_param ?nominals (v : value) : Syntax.param option =
  match v with
  | VRecord { fields; _ } ->
      let* name = let* n = field "name" fields in value_to_id ?nominals n in
      let* type_ = let* t = field "type_" fields in value_to_option ?nominals (unwind_stx ?nominals) t in
      let* explicitness = let* e = field "explicitness" fields in value_to_explicitness ?nominals e in
      Some { Syntax.name; type_; trait_bounds = []; explicitness }
  | _ -> None

and unwind_stx ?nominals (v : value) : Syntax.t option =
  let mk span_val kind = let* span = value_to_span ?nominals span_val in Some { Syntax.kind; span } in
  match v with
  | VCon { name; spine = [ span_val; id_val ]; nominal }
    when expr_con_matches ?nominals "RawVar" name nominal ->
      let* id = value_to_id ?nominals id_val in
      mk span_val (Var id)
  | VCon { name; spine = [ span_val; atom_val ]; nominal }
    when expr_con_matches ?nominals "RawAtom" name nominal ->
      let* a = atomval_to_atom ?nominals atom_val in
      mk span_val (Atom a)
  | VCon { name; spine = [ span_val; fn_val; expl_val; arg_val ]; nominal }
    when expr_con_matches ?nominals "RawAp" name nominal ->
      let* fn = unwind_stx ?nominals fn_val in
      let* expl = value_to_explicitness ?nominals expl_val in
      let* arg = unwind_stx ?nominals arg_val in
      mk span_val (Ap (fn, expl, arg))
  | VCon { name; spine = [ span_val; param_val; body_val ]; nominal }
    when expr_con_matches ?nominals "RawLam" name nominal ->
      let* param = value_to_param ?nominals param_val in
      let* body = unwind_stx ?nominals body_val in
      mk span_val (Lam (param, body))
  | VCon { name; spine = [ span_val; name_val; type_val; value_val; body_val; rec_val ]; nominal }
    when expr_con_matches ?nominals "RawLet" name nominal ->
      let* name = value_to_id ?nominals name_val in
      let* type_ = value_to_option ?nominals (unwind_stx ?nominals) type_val in
      let* value = unwind_stx ?nominals value_val in
      let* body = unwind_stx ?nominals body_val in
      let* recursive = value_to_bool ?nominals rec_val in
      mk span_val (Let { name; type_; value; body; recursive })
  | VStx (StxExpr stx) -> Some stx
  | _ -> None

let unwrap_stx ?nominals (v : value) : Syntax.t option = unwind_stx ?nominals v

let unwrap_stx_decl ?nominals (v : value) : Syntax.struct_binding list option =
  let decl v =
    match v with
    | VCon { name; spine = [ id_val; expr_val; pub_val ]; nominal }
      when decl_con_matches ?nominals "DeclLet" name nominal ->
        let* name = value_to_id ?nominals id_val in
        let* value = unwind_stx ?nominals expr_val in
        let* public = value_to_bool ?nominals pub_val in
        Some [ Syntax.LetBinding { name; value; public; recursive = false } ]
    | VStx (StxDecl binding) -> Some [ binding ]
    | _ -> None
  in
  match v with
  | VStx (StxDecl binding) -> Some [ binding ]
  | VStx (StxDecls bindings) -> Some bindings
  | VStx (StxExpr { kind = Syntax.Module { bindings } | Syntax.Struct { bindings; _ }; _ }) -> Some bindings
  | _ -> (
      (* A [Decl] macro returns one declaration or a list of them. *)
      match value_to_list ?nominals v with
      | Some items ->
          let* decls = option_all (List.map decl items) in
          Some (List.concat decls)
      | None -> decl v)

let wrap_stx_decl ~nominals (bindings : Syntax.struct_binding list) : value =
  match nominals with
  | None ->
      (match bindings with
       | [ b ] -> VStx (StxDecl b)
       | _ -> VStx (StxDecls bindings))
  | Some n ->
      list_to_value n
        (List.map
           (function
             | Syntax.LetBinding { name; value; public; _ } ->
                 VCon { name = "DeclLet";
                        spine = [ id_to_value n name; wrap_stx_sub n value; vcon_bool n public ];
                        nominal = n.decl }
             | binding -> VStx (StxDecl binding))
           bindings)

let unwrap_stx_pat ?nominals (v : value) : Syntax.pat option =
  let rec go v =
    let pats v = let* items = value_to_list ?nominals v in option_all (List.map go items) in
    match v with
    | VCon { name; spine = [ _span_val ]; nominal }
      when pat_con_matches ?nominals "RawPatWild" name nominal ->
        Some Syntax.PatWild
    | VCon { name; spine = [ _span_val; id_val ]; nominal }
      when pat_con_matches ?nominals "RawPatBind" name nominal ->
        let* id = value_to_id ?nominals id_val in
        Some (Syntax.PatBind id)
    | VCon { name; spine = [ _span_val; path_val; ctor_val; args_val ]; nominal }
      when pat_con_matches ?nominals "RawPatCon" name nominal ->
        let* path_items = value_to_list ?nominals path_val in
        let* path = option_all (List.map (function VAtom (String s) -> Some s | _ -> None) path_items) in
        let* ctor = value_to_id ?nominals ctor_val in
        let* args = pats args_val in
        Some (Syntax.PatCon (path, ctor.name, args))
    | VCon { name; spine = [ _span_val; atom_val ]; nominal }
      when pat_con_matches ?nominals "RawPatAtom" name nominal ->
        let* a = atomval_to_atom ?nominals atom_val in
        Some (Syntax.PatAtom a)
    | VCon { name; spine = [ _span_val; pats_val ]; nominal }
      when pat_con_matches ?nominals "RawPatProd" name nominal ->
        let* ps = pats pats_val in
        Some (Syntax.PatProd ps)
    | VCon { name; spine = [ _span_val; l_val; r_val ]; nominal }
      when pat_con_matches ?nominals "RawPatOr" name nominal ->
        let* l = go l_val in
        let* r = go r_val in
        Some (Syntax.PatOr (l, r))
    | VStx (StxPattern p) -> Some p
    | _ -> None
  in
  go v

let wrap_stx_pat ~nominals (p : Syntax.pat) : value =
  match nominals with
  | None -> VStx (StxPattern p)
  | Some n ->
    let span_opt = vcon_none n in
    let pat name spine = VCon { name; spine = span_opt :: spine; nominal = n.pat } in
    let rec go p =
      match p with
      | Syntax.PatWild -> pat "RawPatWild" []
      | Syntax.PatBind id -> pat "RawPatBind" [ id_to_value n id ]
      | Syntax.PatCon (path, ctor, args) ->
          (* A constructor head is a plain string in [Syntax.pat]: it has no
             span or scope set to carry, so both are empty. *)
          let ctor_val = id_to_value n { Syntax.name = ctor; span = Source_span.synthetic; scope = Scope_set.empty } in
          pat "RawPatCon"
            [ list_to_value n (List.map (fun s -> VAtom (String s)) path); ctor_val;
              list_to_value n (List.map go args) ]
      | Syntax.PatAtom a -> pat "RawPatAtom" [ atom_to_atomval n a ]
      | Syntax.PatProd pats -> pat "RawPatProd" [ list_to_value n (List.map go pats) ]
      | Syntax.PatOr (l, r) -> pat "RawPatOr" [ go l; go r ]
      | Syntax.PatRecord _ | Syntax.PatStructType _ | Syntax.PatType _ -> VStx (StxPattern p)
    in
    go p
