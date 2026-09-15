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
  | VSig _ -> "VSig"
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
  | VGlued _ -> "VGlued"
  | VRecOcc _ -> "VRecOcc"
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
  field : value;
  param : value;
  effect_row : value;
  effect_op : value;
  type_decl : value;
  ctor : value;
  branch : value;
  pat_field : value;
  atom_ty : value;
  fixity : value;
  macro_ann : value;
  quote_hole : value;
  token_tree : value;
  token_kind : value;
  delim : value;
  assoc : value;
  role : value;
  role_meaning : value;
  order : value;
  rule : value;
  rule_part : value;
  hole_kind : value;
  replacement : value;
  capture : value;
  captured : value;
}

(* Reflection: [Syntax.t] and the reflection ADTs of the prelude's [Syntax]
   module are one grammar seen twice. [wrap_*] builds the value of a form,
   [unwrap_*] reads a form back; the pair is the identity on every field (M1).
   A value that is not well-formed reflection unwraps to [None], never to a
   guess. *)

let ( let* ) = Option.bind

let rec option_all = function
  | [] -> Some []
  | x :: xs -> let* x = x in let* xs = option_all xs in Some (x :: xs)

let same_nominal expected actual =
  match expected, actual with
  | VNominal e, VNominal a -> e.id = a.id
  | _ -> false

(* ---- building values ---- *)

let con nominal name spine = VCon { name; spine; nominal }

(* [Bool] is a library ADT; reflected flags are its nullary constructors. *)
let w_bool ns b = con ns.bool (if b then "True" else "False") []

let w_option ns f = function
  | Some x -> con ns.option_ Compiler_names.Constructor_name.some [ f x ]
  | None -> con ns.option_ Compiler_names.Constructor_name.none []

let w_list ns f items =
  List.fold_right (fun h t -> con ns.list "Cons" [ f h; t ]) items (con ns.list "Nil" [])

let w_string s = VAtom (String s)
let w_i64 n = VAtom (I64 (Int64.of_int n))
let record fields = VRecord { typ = VU; fields }

let w_span ns (span : Source_span.t) : value =
  if span.synthetic then w_option ns Fun.id None
  else
    w_option ns Fun.id
      (Some
         (record
            [ ("file", w_option ns w_string span.file);
              ("start_byte", w_i64 span.start_byte);
              ("end_byte", w_i64 span.end_byte);
              ("start_line", w_option ns w_i64 span.start_line);
              ("start_col", w_option ns w_i64 span.start_col);
              ("end_line", w_option ns w_i64 span.end_line);
              ("end_col", w_option ns w_i64 span.end_col) ]))

let w_id ns (id : Syntax.id) =
  record [ ("name", w_string id.name); ("span", w_span ns id.span); ("scope", VAtom (Scopes (id.scope, Syntax.certificate id.name))) ]

let w_explicitness ns = function
  | Explicitness.Explicit -> con ns.explicitness "Explicit" []
  | Explicitness.Implicit -> con ns.explicitness "Implicit" []

let w_atom ns (a : Atom.t) =
  match a with
  | I64 n -> con ns.atom_val "I64Atom" [ VAtom (I64 n) ]
  | Char c -> con ns.atom_val "CharAtom" [ VAtom (Char c) ]
  | String s -> con ns.atom_val "StringAtom" [ VAtom (String s) ]
  | Unit -> con ns.atom_val "UnitAtom" []
  | Scopes _ as s -> con ns.atom_val "ScopesAtom" [ VAtom s ]

let atom_ty_names =
  [ (Atom_ty.TI64, "TyI64"); (TUnit, "TyUnit"); (TChar, "TyChar"); (TString, "TyString");
    (TScopes, "TyScopes"); (TAbsurd, "TyAbsurd") ]

let w_atom_ty ns t = con ns.atom_ty (List.assoc t atom_ty_names) []

let w_path ns (p : Syntax.path) =
  let w_choice (c : Syntax.open_choice) =
    record [ ("opens", w_list ns w_string c.opens); ("fallback", w_option ns w_string c.fallback) ]
  in
  record [ ("head", w_id ns p.head); ("members", w_list ns w_string p.members);
           ("head_choice", w_option ns w_choice p.head_choice) ]

let w_macro_ann ns = function
  | Syntax.MacroAnnotation.Expr -> con ns.macro_ann "AnnExpr" []
  | Decl -> con ns.macro_ann "AnnDecl" []

let w_fixity ns = function
  | Syntax.PrefixOp -> con ns.fixity "PrefixFixity" []
  | InfixOp -> con ns.fixity "InfixFixity" []

let w_delim ns (d : Token_tree.delimiter) =
  con ns.delim (match d with Paren -> "ParenDelim" | Bracket -> "BracketDelim" | Brace -> "BraceDelim") []

let w_token_kind ns (k : Token_tree.token_kind) =
  let tk name spine = con ns.token_kind name spine in
  match k with
  | Ident s -> tk "IdentTok" [ w_string s ]
  | Operator s -> tk "OperatorTok" [ w_string s ]
  | Int n -> tk "IntTok" [ VAtom (I64 n) ]
  | Char c -> tk "CharTok" [ VAtom (Char c) ]
  | String s -> tk "StringTok" [ w_string s ]
  | Unit -> tk "UnitTok" []
  | k -> (
      match Token_tree.spelling_of Token_tree.keyword_spellings k with
      | Some s -> tk "KeywordTok" [ w_string s ]
      | None -> tk "PunctTok" [ w_string (Option.get (Token_tree.spelling_of Token_tree.punct_spellings k)) ])

(* A token tree, each token with its scope set (M9). *)
let rec w_token_tree ns (t : Token_tree.t) =
  match t.datum with
  | Token tok ->
      let cert = match tok.kind with Ident name -> Syntax.certificate name | _ -> None in
      con ns.token_tree "Tok" [ w_span ns t.span; w_token_kind ns tok.kind; VAtom (Scopes (tok.scope, cert)) ]
  | Group (d, items, span) -> con ns.token_tree "TokGroup" [ w_span ns span; w_delim ns d; w_list ns (w_token_tree ns) items ]

let w_tokens ns ts = w_list ns (w_token_tree ns) ts

let w_assoc ns = function
  | Syntax.LeftAssoc -> con ns.assoc "Left" []
  | RightAssoc -> con ns.assoc "Right" []
  | NonAssoc -> con ns.assoc "NonAssoc" []

let w_hole_kind ns (k : Syntax.hole_kind) =
  con ns.hole_kind
    (match k with HoleExpr -> "HoleExpr" | HoleBlock -> "HoleBlock" | HoleId -> "HoleId" | HoleDecl -> "HoleDecl" | HoleOneDecl -> "HoleOneDecl" | HolePattern -> "HolePattern" | HoleTokens -> "HoleTokens")
    []

let rec w_expr ns (stx : Syntax.t) : value =
  let e name spine = con ns.expr name (w_span ns stx.span :: spine) in
  let x = w_expr ns and ids = w_list ns (w_id ns) in
  match stx.kind with
  | Var id -> e "RawVar" [ w_id ns id ]
  | Atom a -> e "RawAtom" [ w_atom ns a ]
  | Self -> e "RawSelf" []
  | SelfType -> e "RawSelfType" []
  | Ap (f, ex, a) -> e "RawAp" [ x f; w_explicitness ns ex; x a ]
  | Lam (p, body) -> e "RawLam" [ w_param ns p; x body ]
  | Let { name; type_; value; body; recursive } ->
      e "RawLet" [ w_id ns name; w_option ns x type_; x value; x body; w_bool ns recursive ]
  | LetRecGroup { members; body } ->
      e "RawLetRecGroup" [ ids (List.map fst members); w_list ns x (List.map snd members); x body ]
  | Annotated { inner; typ } -> e "RawAnnotated" [ x inner; x typ ]
  | Prod xs -> e "RawProd" [ w_list ns x xs ]
  | ProdTy xs -> e "RawProdTy" [ w_list ns x xs ]
  | TraitBoundSet xs -> e "RawTraitBoundSet" [ w_list ns x xs ]
  | Arrow (ex, name, dom, row, cod) ->
      e "RawArrow" [ w_explicitness ns ex; w_option ns (w_id ns) name; x dom; w_option ns (w_effect_row ns) row; x cod ]
  | FieldAccess (r, f) -> e "RawFieldAccess" [ x r; w_string f ]
  | Proj (r, i) -> e "RawProj" [ x r; w_i64 i ]
  | RecordConstruct { typ; fields } -> e "RawRecordConstruct" [ x typ; w_fields ns fields ]
  | Struct { bindings } -> e "RawStruct" [ w_list ns (w_decl ns) bindings ]
  | Module { bindings } -> e "RawModule" [ w_list ns (w_decl ns) bindings ]
  | Sig { bindings } -> e "RawSig" [ w_list ns (w_decl ns) bindings ]
  | Enum { name; ctors } ->
      e "RawEnum" [ w_option ns w_string name; w_ctors ns (List.map (fun (c, ps) -> (Syntax.fresh_id c, ps)) ctors) ]
  | Import { path; scope } -> e "RawImport" [ w_string path; VAtom (Scopes (scope, None)) ]
  | Open (m, body, label) -> e "RawOpen" [ x m; x body; w_string label ]
  | OpenChoice { name; opens; fallback } ->
      e "RawOpenChoice" [ w_id ns name; w_list ns w_string opens; w_option ns w_string fallback ]
  | TypeDef { name; params; ctors; body } ->
      e "RawTypeDef" [ w_type_decl ns { name; params; ctors }; x body ]
  | EffectDef { name; params; ops; body } ->
      e "RawEffectDef" [ w_id ns name; ids params; w_list ns (w_effect_op ns) ops; x body ]
  | TraitDef { name; params; fields; body } ->
      e "RawTraitDef" [ w_id ns name; ids params; w_fields ns fields; x body ]
  | ImplDef { name; trait; args; fields; body } ->
      e "RawImplDef" [ w_option ns (w_id ns) name; w_path ns trait; w_list ns x args; w_fields ns fields; x body ]
  | Perform { op; arg } -> e "RawPerform" [ w_path ns op; x arg ]
  | Resume a -> e "RawResume" [ x a ]
  | RefNew a -> e "RawRefNew" [ x a ]
  | RefGet a -> e "RawRefGet" [ x a ]
  | RefSet (l, r) -> e "RawRefSet" [ x l; x r ]
  | Match (scrut, branches) -> e "RawMatch" [ x scrut; w_list ns (w_branch ns) branches ]
  | Stx inner -> e "RawStx" [ x inner ]
  (* Internal: a macro sees the argument, not the elaborator's note that it is done. *)
  | Elaborated { form; _ } -> x form
  | Quote { template; holes } -> e "RawQuote" [ x template; w_quote_holes ns holes ]
  | QuoteDecls { items; holes } -> e "RawQuoteDecls" [ w_list ns (w_decl ns) items; w_quote_holes ns holes ]
  | MacroDef { name; value; body; kind; output } ->
      e "RawMacroDef" [ w_id ns name; x value; x body; w_option ns (w_macro_ann ns) kind; w_option ns x output ]
  | SyntaxDef { name; role; body } -> e "RawSyntaxDef" [ w_id ns name; w_role ns role; x body ]
  | Block ts -> e "RawBlock" [ w_tokens ns ts ]
  | Instantiate { form; rule; captures; from_unit } ->
      e "RawInstantiate" [ w_id ns form; w_rule ns rule; w_captures ns captures; w_option ns w_string from_unit ]
  | MacroCall (f, args) -> e "RawMacroCall" [ x f; w_list ns (w_captured ns) args ]
  | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } ->
      e "RawOperatorUse"
        [ w_id ns operator; w_fixity ns fixity; w_list ns x operands; w_span ns declaration_span;
          w_span ns use_span; w_option ns w_string unit ]

and w_role ns (r : Syntax.role) =
  let meaning =
    match r.meaning with
    | ApplyValue -> con ns.role_meaning "ApplyValue" []
    | AssignRef -> con ns.role_meaning "AssignRef" []
    | CallMacro -> con ns.role_meaning "CallMacro" []
    | Rules { rules_kind; rules } -> con ns.role_meaning "Rules" [ w_macro_ann ns rules_kind; w_list ns (w_rule ns) rules ]
    | OrderGroup -> con ns.role_meaning "OrderGroup" []
    | PolyArrow -> con ns.role_meaning "PolyArrow" []
  in
  con ns.role "MkRole"
    [ w_fixity ns r.fixity; w_option ns (w_order ns) r.order; meaning; w_span ns r.declared_at;
      w_option ns w_string r.from_unit ]

and w_order ns (o : Syntax.order) =
  con ns.order "MkOrder"
    [ w_string o.group; w_string o.group_name; w_assoc ns o.group_assoc; w_bool ns o.weakest;
      w_list ns (w_order ns) o.stronger_than; w_list ns (w_order ns) o.weaker_than ]

and w_rule ns (r : Syntax.rule) =
  let replacement =
    match r.replacement with
    | ReplaceExpr e -> con ns.replacement "ReplaceExpr" [ w_expr ns e ]
    | ReplaceDecls ds -> con ns.replacement "ReplaceDecls" [ w_list ns (w_decl ns) ds ]
  in
  con ns.rule "MkRule" [ w_list ns (w_rule_part ns) r.pattern; replacement; w_span ns r.rule_span ]

and w_rule_part ns = function
  | Syntax.PartToken t -> con ns.rule_part "PartToken" [ w_token_tree ns t ]
  | PartGroup (d, parts, span) -> con ns.rule_part "PartGroup" [ w_delim ns d; w_list ns (w_rule_part ns) parts; w_span ns span ]
  | PartHole { hole; hole_kind; hole_span } ->
      con ns.rule_part "PartHole" [ w_string hole; w_hole_kind ns hole_kind; w_span ns hole_span ]

and w_captured ns = function
  | Syntax.CapExpr e -> con ns.captured "CapExpr" [ w_expr ns e ]
  | CapBlock ts -> con ns.captured "CapBlock" [ w_tokens ns ts ]
  | CapId tok -> con ns.captured "CapId" [ w_token_tree ns { datum = Token tok; span = tok.span } ]
  | CapPattern p -> con ns.captured "CapPattern" [ w_pat ns p ]
  | CapDecls ds -> con ns.captured "CapDecls" [ w_list ns (w_decl ns) ds ]
  | CapDecl d -> con ns.captured "CapDecl" [ w_decl ns d ]
  | CapTokens ts -> con ns.captured "CapTokens" [ w_tokens ns ts ]

and w_captures ns captures =
  w_list ns (fun (n, c) -> con ns.capture "MkCapture" [ w_string n; w_captured ns c ]) captures

and w_quote_holes ns holes = w_list ns (fun (n, h) -> con ns.quote_hole "MkQuoteHole" [ w_string n; w_expr ns h ]) holes

and w_fields ns fields = w_list ns (fun (n, v) -> con ns.field "MkField" [ w_string n; w_expr ns v ]) fields

and w_param ns (p : Syntax.param) =
  con ns.param "MkParam"
    [ w_id ns p.name; w_option ns (w_expr ns) p.type_; w_list ns (w_path ns) p.trait_bounds;
      w_explicitness ns p.explicitness ]

and w_effect_row ns (row : Syntax.effect_row) =
  con ns.effect_row "MkEffectRow" [ w_list ns (w_expr ns) row.effects; w_option ns (w_expr ns) row.tail; w_bool ns row.inferred; w_bool ns row.polymorphic ]

and w_effect_op ns (op : Syntax.effect_op) =
  con ns.effect_op "MkEffectOp" [ w_string op.name; w_expr ns op.input; w_expr ns op.output ]

and w_type_decl ns (d : Syntax.type_decl) =
  con ns.type_decl "MkTypeDecl"
    [ w_id ns d.name; w_list ns (w_id ns) d.params;
      w_ctors ns d.ctors ]

and w_ctors ns ctors = w_list ns (fun (c, payloads) -> con ns.ctor "MkCtor" [ w_id ns c; w_list ns (w_expr ns) payloads ]) ctors

and w_branch ns = function
  | Syntax.ValueBranch (p, body) -> con ns.branch "ValueBranch" [ w_pat ns p; w_expr ns body ]
  | EffectBranch { op; arg_pat; body } ->
      con ns.branch "EffectBranch" [ w_path ns op; w_pat ns arg_pat; w_expr ns body ]

and w_pat ns (p : Syntax.pat) =
  (* Patterns carry no span in [Syntax.pat]; the reflected span is always [None]. *)
  let pc name spine = con ns.pat name (w_option ns Fun.id None :: spine) in
  let pat_field (n, p) = con ns.pat_field "MkPatField" [ w_string n; w_option ns (w_pat ns) p ] in
  match p with
  | PatWild -> pc "RawPatWild" []
  | PatBind id -> pc "RawPatBind" [ w_id ns id ]
  | PatCon (path, args) -> pc "RawPatCon" [ w_path ns path; w_list ns (w_pat ns) args ]
  | PatAtom a -> pc "RawPatAtom" [ w_atom ns a ]
  | PatProd ps -> pc "RawPatProd" [ w_list ns (w_pat ns) ps ]
  | PatOr (l, r) -> pc "RawPatOr" [ w_pat ns l; w_pat ns r ]
  | PatRecord { typ; fields; partial } ->
      pc "RawPatRecord" [ w_path ns typ; w_list ns pat_field fields; w_bool ns partial ]
  | PatStructType { fields; partial } ->
      pc "RawPatStructType" [ w_list ns pat_field (List.map (fun (n, p) -> (n, Some p)) fields); w_bool ns partial ]
  | PatType t -> pc "RawPatType" [ w_atom_ty ns t ]

and w_decl ns (b : Syntax.struct_binding) =
  let d name spine = con ns.decl name spine in
  let ids = w_list ns (w_id ns) in
  match b with
  | LetBinding { name; value; public; recursive } ->
      d "DeclLet" [ w_id ns name; w_expr ns value; w_bool ns public; w_bool ns recursive ]
  | RecGroupBinding { members; public } ->
      d "DeclRecGroup" [ ids (List.map fst members); w_list ns (w_expr ns) (List.map snd members); w_bool ns public ]
  | MethodBinding { name; params; effects; body; public } ->
      d "DeclMethod" [ w_id ns name; w_list ns (w_param ns) params; w_option ns (w_effect_row ns) effects; w_expr ns body; w_bool ns public ]
  | EffectBinding { name; params; ops; public } ->
      d "DeclEffect" [ w_id ns name; ids params; w_list ns (w_effect_op ns) ops; w_bool ns public ]
  | TraitBinding { name; params; fields; public } ->
      d "DeclTrait" [ w_id ns name; ids params; w_fields ns fields; w_bool ns public ]
  | ImplBinding { name; trait; args; fields; public } ->
      d "DeclImpl" [ w_option ns (w_id ns) name; w_path ns trait; w_list ns (w_expr ns) args; w_fields ns fields; w_bool ns public ]
  | MacroBinding { name; value; public; kind; output } ->
      d "DeclMacro" [ w_id ns name; w_expr ns value; w_bool ns public; w_option ns (w_macro_ann ns) kind; w_option ns (w_expr ns) output ]
  | MacroCallBinding { f; args; public } -> d "DeclMacroCall" [ w_expr ns f; w_list ns (w_captured ns) args; w_bool ns public ]
  | PatternSynBinding { name; params; rhs; public } ->
      d "DeclPatternSyn" [ w_id ns name; ids params; w_pat ns rhs; w_bool ns public ]
  | FieldBinding { name; type_ } -> d "DeclField" [ w_string name; w_expr ns type_ ]
  | OpenBinding (m, label) -> d "DeclOpen" [ w_expr ns m; w_string label ]
  | ExportBinding { m; names; public } -> d "DeclExport" [ w_expr ns m; w_option ns (w_list ns w_string) names; w_bool ns public ]
  | HoleBinding id -> d "DeclHole" [ w_id ns id ]
  | SyntaxBinding { name; role; public } -> d "DeclSyntax" [ w_id ns name; w_role ns role; w_bool ns public ]
  | Items ts -> d "DeclItems" [ w_tokens ns ts ]
  | InstantiateBinding { inst = { form; rule; captures; from_unit }; public } ->
      d "DeclInstantiate" [ w_id ns form; w_rule ns rule; w_captures ns captures; w_option ns w_string from_unit; w_bool ns public ]

(* ---- reading values back ---- *)

(* The payload of a constructor of type [nominal]: a constructor built by macro
   code carries a parameterised type's arguments first in its spine, one built
   by reflection does not, so the payload is the trailing [arity] elements. *)
let payload nominal (v : value) : (string * value list) option =
  match v with
  | VCon { name; spine; nominal = actual } when same_nominal nominal actual -> Some (name, spine)
  | _ -> None

let trailing n spine =
  let len = List.length spine in
  if len < n then None else Some (List.filteri (fun i _ -> i >= len - n) spine)

let u_string = function VAtom (String s) -> Some s | _ -> None
let u_int = function VAtom (I64 n) -> Some (Int64.to_int n) | _ -> None
let u_field name = function VRecord { fields; _ } -> List.assoc_opt name fields | _ -> None

let u_bool ns v =
  match payload ns.bool v with
  | Some ("True", []) -> Some true
  | Some ("False", []) -> Some false
  | _ -> None

let u_option ns f v =
  match payload ns.option_ v with
  | Some (name, _) when String.equal name Compiler_names.Constructor_name.none -> Some None
  | Some (name, spine) when String.equal name Compiler_names.Constructor_name.some -> (
      match trailing 1 spine with Some [ x ] -> let* x = f x in Some (Some x) | _ -> None)
  | _ -> None

let rec u_list ns f v =
  match payload ns.list v with
  | Some ("Nil", _) -> Some []
  | Some ("Cons", spine) -> (
      match trailing 2 spine with
      | Some [ h; t ] -> let* h = f h in let* t = u_list ns f t in Some (h :: t)
      | _ -> None)
  | _ -> None

let u_span ns v : Source_span.t option =
  let* span = u_option ns Option.some v in
  match span with
  | None -> Some Source_span.synthetic
  | Some r ->
      let get name f = let* v = u_field name r in f v in
      let* file = get "file" (u_option ns u_string) in
      let* start_byte = get "start_byte" u_int in
      let* end_byte = get "end_byte" u_int in
      let* start_line = get "start_line" (u_option ns u_int) in
      let* start_col = get "start_col" (u_option ns u_int) in
      let* end_line = get "end_line" (u_option ns u_int) in
      let* end_col = get "end_col" (u_option ns u_int) in
      Some { Source_span.file; start_byte; end_byte; start_line; start_col; end_line; end_col; synthetic = false }

let u_id ns v : Syntax.id option =
  let* name = let* n = u_field "name" v in u_string n in
  let* span = let* s = u_field "span" v in u_span ns s in
  let* scope, cert = match u_field "scope" v with Some (VAtom (Scopes (s, c))) -> Some (s, c) | _ -> None in
  if Syntax.certified name cert then Some { Syntax.name; span; scope } else None

let u_explicitness ns v =
  match payload ns.explicitness v with
  | Some ("Explicit", []) -> Some Explicitness.Explicit
  | Some ("Implicit", []) -> Some Explicitness.Implicit
  | _ -> None

let u_atom ns v : Atom.t option =
  match payload ns.atom_val v with
  | Some ("I64Atom", [ VAtom (I64 n) ]) -> Some (I64 n)
  | Some ("CharAtom", [ VAtom (Char c) ]) -> Some (Char c)
  | Some ("StringAtom", [ VAtom (String s) ]) -> Some (String s)
  | Some ("UnitAtom", []) -> Some Unit
  | Some ("ScopesAtom", [ VAtom (Scopes _ as s) ]) -> Some s
  | _ -> None

let u_atom_ty ns v =
  match payload ns.atom_ty v with
  | Some (name, []) -> List.find_map (fun (t, n) -> if String.equal n name then Some t else None) atom_ty_names
  | _ -> None

let u_path ns v : Syntax.path option =
  let* head = let* h = u_field "head" v in u_id ns h in
  let* members = let* m = u_field "members" v in u_list ns u_string m in
  let u_choice c : Syntax.open_choice option =
    let* opens = let* o = u_field "opens" c in u_list ns u_string o in
    let* fallback = let* f = u_field "fallback" c in u_option ns u_string f in
    Some { Syntax.opens; fallback }
  in
  let* head_choice = let* c = u_field "head_choice" v in u_option ns u_choice c in
  Some { Syntax.head; members; head_choice }

let u_macro_ann ns v : Syntax.MacroAnnotation.t option =
  match payload ns.macro_ann v with
  | Some ("AnnExpr", []) -> Some Syntax.MacroAnnotation.Expr
  | Some ("AnnDecl", []) -> Some Syntax.MacroAnnotation.Decl
  | _ -> None

let u_fixity ns v =
  match payload ns.fixity v with
  | Some ("PrefixFixity", []) -> Some Syntax.PrefixOp
  | Some ("InfixFixity", []) -> Some Syntax.InfixOp
  | _ -> None

let u_delim ns v : Token_tree.delimiter option =
  match payload ns.delim v with
  | Some ("ParenDelim", []) -> Some Paren
  | Some ("BracketDelim", []) -> Some Bracket
  | Some ("BraceDelim", []) -> Some Brace
  | _ -> None

let u_token_kind ns v : Token_tree.token_kind option =
  match payload ns.token_kind v with
  | Some ("IdentTok", [ s ]) -> let* s = u_string s in Some (Token_tree.Ident s)
  | Some ("OperatorTok", [ s ]) -> let* s = u_string s in Some (Token_tree.Operator s)
  | Some ("IntTok", [ VAtom (I64 n) ]) -> Some (Token_tree.Int n)
  | Some ("CharTok", [ VAtom (Char c) ]) -> Some (Token_tree.Char c)
  | Some ("StringTok", [ s ]) -> let* s = u_string s in Some (Token_tree.String s)
  | Some ("UnitTok", []) -> Some Token_tree.Unit
  | Some ("KeywordTok", [ s ]) -> let* s = u_string s in Token_tree.of_spelling Token_tree.keyword_spellings s
  | Some ("PunctTok", [ s ]) -> let* s = u_string s in Token_tree.of_spelling Token_tree.punct_spellings s
  | _ -> None

let rec u_token_tree ns v : Token_tree.t option =
  match payload ns.token_tree v with
  | Some ("Tok", [ span; kind; VAtom (Scopes (scope, cert)) ]) ->
      let* span = u_span ns span in
      let* kind = u_token_kind ns kind in
      let* () = match kind with Ident name when not (Syntax.certified name cert) -> None | _ -> Some () in
      Some { Token_tree.datum = Token { kind; span; scope }; span }
  | Some ("TokGroup", [ span; d; items ]) ->
      let* span = u_span ns span in
      let* d = u_delim ns d in
      let* items = u_list ns (u_token_tree ns) items in
      Some { Token_tree.datum = Group (d, items, span); span }
  | _ -> None

let u_tokens ns v = u_list ns (u_token_tree ns) v

let u_assoc ns v =
  match payload ns.assoc v with
  | Some ("Left", []) -> Some Syntax.LeftAssoc
  | Some ("Right", []) -> Some Syntax.RightAssoc
  | Some ("NonAssoc", []) -> Some Syntax.NonAssoc
  | _ -> None

let u_hole_kind ns v : Syntax.hole_kind option =
  match payload ns.hole_kind v with
  | Some ("HoleExpr", []) -> Some HoleExpr
  | Some ("HoleBlock", []) -> Some HoleBlock
  | Some ("HoleId", []) -> Some HoleId
  | Some ("HoleDecl", []) -> Some HoleDecl
  | Some ("HoleOneDecl", []) -> Some HoleOneDecl
  | Some ("HoleTokens", []) -> Some HoleTokens
  | Some ("HolePattern", []) -> Some HolePattern
  | _ -> None

let rec u_expr ns (v : value) : Syntax.t option =
  let x = u_expr ns and ids = u_list ns (u_id ns) in
  match v with
  | VStx (StxExpr stx) -> Some stx
  | _ -> (
      let* name, spine = payload ns.expr v in
      let* span_v, args = match spine with s :: args -> Some (s, args) | [] -> None in
      let* span = u_span ns span_v in
      let mk kind = Some { Syntax.kind; span } in
      match name, args with
      | "RawVar", [ id ] -> let* id = u_id ns id in mk (Var id)
      | "RawAtom", [ a ] -> let* a = u_atom ns a in mk (Atom a)
      | "RawSelf", [] -> mk Self
      | "RawSelfType", [] -> mk SelfType
      | "RawAp", [ f; ex; a ] ->
          let* f = x f in let* ex = u_explicitness ns ex in let* a = x a in mk (Ap (f, ex, a))
      | "RawLam", [ p; body ] -> let* p = u_param ns p in let* body = x body in mk (Lam (p, body))
      | "RawLetRecGroup", [ names; values; body ] ->
          let* names = ids names in
          let* values = u_list ns x values in
          let* body = x body in
          if List.length names <> List.length values then None
          else mk (LetRecGroup { members = List.combine names values; body })
      | "RawLet", [ name; ty; value; body; recursive ] ->
          let* name = u_id ns name in
          let* type_ = u_option ns x ty in
          let* value = x value in
          let* body = x body in
          let* recursive = u_bool ns recursive in
          mk (Let { name; type_; value; body; recursive })
      | "RawAnnotated", [ inner; typ ] -> let* inner = x inner in let* typ = x typ in mk (Annotated { inner; typ })
      | "RawProd", [ xs ] -> let* xs = u_list ns x xs in mk (Prod xs)
      | "RawProdTy", [ xs ] -> let* xs = u_list ns x xs in mk (ProdTy xs)
      | "RawTraitBoundSet", [ xs ] -> let* xs = u_list ns x xs in mk (TraitBoundSet xs)
      | "RawArrow", [ ex; name; dom; row; cod ] ->
          let* ex = u_explicitness ns ex in
          let* name = u_option ns (u_id ns) name in
          let* dom = x dom in
          let* row = u_option ns (u_effect_row ns) row in
          let* cod = x cod in
          mk (Arrow (ex, name, dom, row, cod))
      | "RawFieldAccess", [ r; f ] -> let* r = x r in let* f = u_string f in mk (FieldAccess (r, f))
      | "RawProj", [ r; i ] -> let* r = x r in let* i = u_int i in mk (Proj (r, i))
      | "RawRecordConstruct", [ typ; fields ] ->
          let* typ = x typ in let* fields = u_fields ns fields in mk (RecordConstruct { typ; fields })
      | "RawStruct", [ bindings ] -> let* bindings = u_list ns (u_decl ns) bindings in mk (Struct { bindings })
      | "RawModule", [ bindings ] -> let* bindings = u_list ns (u_decl ns) bindings in mk (Module { bindings })
      | "RawSig", [ bindings ] -> let* bindings = u_list ns (u_decl ns) bindings in mk (Sig { bindings })
      | "RawEnum", [ name; ctors ] ->
          let* name = u_option ns u_string name in
          let* ctors = u_ctors ns ctors in
          mk (Enum { name; ctors = List.map (fun ((c : Syntax.id), ps) -> (c.name, ps)) ctors })
      | "RawImport", [ path; VAtom (Scopes (scope, _)) ] -> let* path = u_string path in mk (Import { path; scope })
      | "RawOpen", [ m; body; label ] ->
          let* m = x m in let* body = x body in let* label = u_string label in mk (Open (m, body, label))
      | "RawOpenChoice", [ name; opens; fallback ] ->
          let* name = u_id ns name in
          let* opens = u_list ns u_string opens in
          let* fallback = u_option ns u_string fallback in
          mk (OpenChoice { name; opens; fallback })
      | "RawTypeDef", [ decl; body ] ->
          let* { name; params; ctors } = u_type_decl ns decl in
          let* body = x body in
          mk (TypeDef { name; params; ctors; body })
      | "RawEffectDef", [ name; params; ops; body ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* ops = u_list ns (u_effect_op ns) ops in
          let* body = x body in
          mk (EffectDef { name; params; ops; body })
      | "RawTraitDef", [ name; params; fields; body ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* fields = u_fields ns fields in
          let* body = x body in
          mk (TraitDef { name; params; fields; body })
      | "RawImplDef", [ name; trait; args; fields; body ] ->
          let* name = u_option ns (u_id ns) name in
          let* trait = u_path ns trait in
          let* args = u_list ns x args in
          let* fields = u_fields ns fields in
          let* body = x body in
          mk (ImplDef { name; trait; args; fields; body })
      | "RawPerform", [ op; arg ] ->
          let* op = u_path ns op in
          let* arg = x arg in
          mk (Perform { op; arg })
      | "RawResume", [ a ] -> let* a = x a in mk (Resume a)
      | "RawRefNew", [ a ] -> let* a = x a in mk (RefNew a)
      | "RawRefGet", [ a ] -> let* a = x a in mk (RefGet a)
      | "RawRefSet", [ l; r ] -> let* l = x l in let* r = x r in mk (RefSet (l, r))
      | "RawMatch", [ scrut; branches ] ->
          let* scrut = x scrut in
          let* branches = u_list ns (u_branch ns) branches in
          mk (Match (scrut, branches))
      | "RawStx", [ inner ] -> let* inner = x inner in mk (Stx inner)
      | "RawQuote", [ template; holes ] ->
          let* template = x template in
          let* holes = u_quote_holes ns holes in
          mk (Quote { template; holes })
      | "RawQuoteDecls", [ items; holes ] ->
          let* items = u_list ns (u_decl ns) items in
          let* holes = u_quote_holes ns holes in
          mk (QuoteDecls { items; holes })
      | "RawMacroDef", [ name; value; body; kind; output ] ->
          let* name = u_id ns name in
          let* value = x value in
          let* body = x body in
          let* kind = u_option ns (u_macro_ann ns) kind in
          let* output = u_option ns x output in
          mk (MacroDef { name; value; body; kind; output })
      | "RawSyntaxDef", [ name; role; body ] ->
          let* name = u_id ns name in
          let* role = u_role ns role in
          let* body = x body in
          mk (SyntaxDef { name; role; body })
      | "RawBlock", [ ts ] -> let* ts = u_tokens ns ts in mk (Block ts)
      | "RawInstantiate", [ form; rule; captures; from_unit ] ->
          let* form = u_id ns form in
          let* rule = u_rule ns rule in
          let* captures = u_captures ns captures in
          let* from_unit = u_option ns u_string from_unit in
          mk (Instantiate { form; rule; captures; from_unit })
      | "RawMacroCall", [ f; args ] -> let* f = x f in let* args = u_list ns (u_captured ns) args in mk (MacroCall (f, args))
      | "RawOperatorUse", [ operator; fixity; operands; declaration_span; use_span; unit ] ->
          let* operator = u_id ns operator in
          let* fixity = u_fixity ns fixity in
          let* operands = u_list ns x operands in
          let* declaration_span = u_span ns declaration_span in
          let* use_span = u_span ns use_span in
          let* unit = u_option ns u_string unit in
          mk (SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit })
      | _ -> None)

and u_role ns v : Syntax.role option =
  match payload ns.role v with
  | Some ("MkRole", [ fixity; order; meaning; declared_at; from_unit ]) ->
      let* fixity = u_fixity ns fixity in
      let* order = u_option ns (u_order ns) order in
      let* meaning =
        match payload ns.role_meaning meaning with
        | Some ("ApplyValue", []) -> Some Syntax.ApplyValue
        | Some ("AssignRef", []) -> Some Syntax.AssignRef
        | Some ("CallMacro", []) -> Some Syntax.CallMacro
        | Some ("Rules", [ kind; rules ]) ->
            let* rules_kind = u_macro_ann ns kind in
            let* rules = u_list ns (u_rule ns) rules in
            Some (Syntax.Rules { rules_kind; rules })
        | Some ("OrderGroup", []) -> Some Syntax.OrderGroup
        | Some ("PolyArrow", []) -> Some Syntax.PolyArrow
        | _ -> None
      in
      let* declared_at = u_span ns declared_at in
      let* from_unit = u_option ns u_string from_unit in
      Some { Syntax.fixity; order; meaning; declared_at; from_unit }
  | _ -> None

and u_order ns v : Syntax.order option =
  match payload ns.order v with
  | Some ("MkOrder", [ group; group_name; assoc; weakest; stronger; weaker ]) ->
      let* group = u_string group in
      let* group_name = u_string group_name in
      let* group_assoc = u_assoc ns assoc in
      let* weakest = u_bool ns weakest in
      let* stronger_than = u_list ns (u_order ns) stronger in
      let* weaker_than = u_list ns (u_order ns) weaker in
      Some { Syntax.group; group_name; group_assoc; weakest; stronger_than; weaker_than }
  | _ -> None

and u_rule ns v : Syntax.rule option =
  match payload ns.rule v with
  | Some ("MkRule", [ pattern; replacement; rule_span ]) ->
      let* pattern = u_list ns (u_rule_part ns) pattern in
      let* replacement =
        match payload ns.replacement replacement with
        | Some ("ReplaceExpr", [ e ]) -> let* e = u_expr ns e in Some (Syntax.ReplaceExpr e)
        | Some ("ReplaceDecls", [ ds ]) -> let* ds = u_list ns (u_decl ns) ds in Some (Syntax.ReplaceDecls ds)
        | _ -> None
      in
      let* rule_span = u_span ns rule_span in
      Some { Syntax.pattern; replacement; rule_span }
  | _ -> None

and u_rule_part ns v : Syntax.rule_part option =
  match payload ns.rule_part v with
  | Some ("PartToken", [ t ]) -> let* t = u_token_tree ns t in Some (Syntax.PartToken t)
  | Some ("PartGroup", [ d; parts; span ]) ->
      let* d = u_delim ns d in
      let* parts = u_list ns (u_rule_part ns) parts in
      let* span = u_span ns span in
      Some (Syntax.PartGroup (d, parts, span))
  | Some ("PartHole", [ hole; kind; span ]) ->
      let* hole = u_string hole in
      let* hole_kind = u_hole_kind ns kind in
      let* hole_span = u_span ns span in
      Some (Syntax.PartHole { hole; hole_kind; hole_span })
  | _ -> None

and u_captured ns c : Syntax.capture option =
  match payload ns.captured c with
  | Some ("CapExpr", [ e ]) -> let* e = u_expr ns e in Some (Syntax.CapExpr e)
  | Some ("CapBlock", [ ts ]) -> let* ts = u_tokens ns ts in Some (Syntax.CapBlock ts)
  | Some ("CapId", [ t ]) -> (
      match u_token_tree ns t with Some { datum = Token tok; _ } -> Some (Syntax.CapId tok) | _ -> None)
  | Some ("CapPattern", [ p ]) -> let* p = u_pat ns p in Some (Syntax.CapPattern p)
  | Some ("CapDecls", [ ds ]) -> let* ds = u_list ns (u_decl ns) ds in Some (Syntax.CapDecls ds)
  | Some ("CapDecl", [ d ]) -> let* d = u_decl ns d in Some (Syntax.CapDecl d)
  | Some ("CapTokens", [ ts ]) -> let* ts = u_tokens ns ts in Some (Syntax.CapTokens ts)
  | _ -> None

and u_captures ns v =
  u_list ns
    (fun e ->
      match payload ns.capture e with
      | Some ("MkCapture", [ n; c ]) -> let* n = u_string n in let* c = u_captured ns c in Some (n, c)
      | _ -> None)
    v

and u_quote_holes ns v =
  u_list ns
    (fun h ->
      match payload ns.quote_hole h with
      | Some ("MkQuoteHole", [ n; e ]) -> let* n = u_string n in let* e = u_expr ns e in Some (n, e)
      | _ -> None)
    v

and u_fields ns v =
  u_list ns
    (fun f ->
      match payload ns.field f with
      | Some ("MkField", [ n; e ]) -> let* n = u_string n in let* e = u_expr ns e in Some (n, e)
      | _ -> None)
    v

and u_param ns v : Syntax.param option =
  match payload ns.param v with
  | Some ("MkParam", [ name; ty; bounds; ex ]) ->
      let* name = u_id ns name in
      let* type_ = u_option ns (u_expr ns) ty in
      let* trait_bounds = u_list ns (u_path ns) bounds in
      let* explicitness = u_explicitness ns ex in
      Some { Syntax.name; type_; trait_bounds; explicitness }
  | _ -> None

and u_effect_row ns v : Syntax.effect_row option =
  match payload ns.effect_row v with
  | Some ("MkEffectRow", [ effects; tail; inferred; polymorphic ]) ->
      let* effects = u_list ns (u_expr ns) effects in
      let* tail = u_option ns (u_expr ns) tail in
      let* inferred = u_bool ns inferred in
      let* polymorphic = u_bool ns polymorphic in
      Some { Syntax.effects; tail; inferred; polymorphic }
  | _ -> None

and u_effect_op ns v : Syntax.effect_op option =
  match payload ns.effect_op v with
  | Some ("MkEffectOp", [ name; input; output ]) ->
      let* name = u_string name in
      let* input = u_expr ns input in
      let* output = u_expr ns output in
      Some { Syntax.name; input; output }
  | _ -> None

and u_type_decl ns v : Syntax.type_decl option =
  match payload ns.type_decl v with
  | Some ("MkTypeDecl", [ name; params; ctors ]) ->
      let* name = u_id ns name in
      let* params = u_list ns (u_id ns) params in
      let* ctors = u_ctors ns ctors in
      Some { Syntax.name; params; ctors }
  | _ -> None

and u_ctors ns ctors =
  u_list ns
    (fun c ->
      match payload ns.ctor c with
      | Some ("MkCtor", [ cname; payloads ]) ->
          let* cname = u_id ns cname in
          let* payloads = u_list ns (u_expr ns) payloads in
          Some (cname, payloads)
      | _ -> None)
    ctors

and u_branch ns v : Syntax.match_branch option =
  match payload ns.branch v with
  | Some ("ValueBranch", [ p; body ]) -> let* p = u_pat ns p in let* body = u_expr ns body in Some (Syntax.ValueBranch (p, body))
  | Some ("EffectBranch", [ op; p; body ]) ->
      let* op = u_path ns op in
      let* arg_pat = u_pat ns p in
      let* body = u_expr ns body in
      Some (Syntax.EffectBranch { op; arg_pat; body })
  | _ -> None

and u_pat ns v : Syntax.pat option =
  let pat_fields =
    u_list ns (fun f ->
        match payload ns.pat_field f with
        | Some ("MkPatField", [ n; p ]) -> let* n = u_string n in let* p = u_option ns (u_pat ns) p in Some (n, p)
        | _ -> None)
  in
  match v with
  | VStx (StxPattern p) -> Some p
  | _ -> (
      let* name, spine = payload ns.pat v in
      let* args = match spine with _span :: args -> Some args | [] -> None in
      match name, args with
      | "RawPatWild", [] -> Some Syntax.PatWild
      | "RawPatBind", [ id ] -> let* id = u_id ns id in Some (Syntax.PatBind id)
      | "RawPatCon", [ path; ps ] ->
          let* path = u_path ns path in
          let* ps = u_list ns (u_pat ns) ps in
          Some (Syntax.PatCon (path, ps))
      | "RawPatAtom", [ a ] -> let* a = u_atom ns a in Some (Syntax.PatAtom a)
      | "RawPatProd", [ ps ] -> let* ps = u_list ns (u_pat ns) ps in Some (Syntax.PatProd ps)
      | "RawPatOr", [ l; r ] -> let* l = u_pat ns l in let* r = u_pat ns r in Some (Syntax.PatOr (l, r))
      | "RawPatRecord", [ typ; fields; partial ] ->
          let* typ = u_path ns typ in
          let* fields = pat_fields fields in
          let* partial = u_bool ns partial in
          Some (Syntax.PatRecord { typ; fields; partial })
      | "RawPatStructType", [ fields; partial ] ->
          let* fields = pat_fields fields in
          let* fields = option_all (List.map (fun (n, p) -> Option.map (fun p -> (n, p)) p) fields) in
          let* partial = u_bool ns partial in
          Some (Syntax.PatStructType { fields; partial })
      | "RawPatType", [ t ] -> let* t = u_atom_ty ns t in Some (Syntax.PatType t)
      | _ -> None)

and u_decl ns v : Syntax.struct_binding option =
  let ids = u_list ns (u_id ns) in
  match v with
  | VStx (StxDecl b) -> Some b
  | _ -> (
      let* name, args = payload ns.decl v in
      match name, args with
      | "DeclRecGroup", [ names; values; public ] ->
          let* names = u_list ns (u_id ns) names in
          let* values = u_list ns (u_expr ns) values in
          let* public = u_bool ns public in
          if List.length names <> List.length values then None
          else Some (Syntax.RecGroupBinding { members = List.combine names values; public })
      | "DeclLet", [ name; value; public; recursive ] ->
          let* name = u_id ns name in
          let* value = u_expr ns value in
          let* public = u_bool ns public in
          let* recursive = u_bool ns recursive in
          Some (Syntax.LetBinding { name; value; public; recursive })
      | "DeclMethod", [ name; params; effects; body; public ] ->
          let* name = u_id ns name in
          let* params = u_list ns (u_param ns) params in
          let* effects = u_option ns (u_effect_row ns) effects in
          let* body = u_expr ns body in
          let* public = u_bool ns public in
          Some (Syntax.MethodBinding { name; params; effects; body; public })
      | "DeclEffect", [ name; params; ops; public ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* ops = u_list ns (u_effect_op ns) ops in
          let* public = u_bool ns public in
          Some (Syntax.EffectBinding { name; params; ops; public })
      | "DeclTrait", [ name; params; fields; public ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* fields = u_fields ns fields in
          let* public = u_bool ns public in
          Some (Syntax.TraitBinding { name; params; fields; public })
      | "DeclImpl", [ name; trait; args; fields; public ] ->
          let* name = u_option ns (u_id ns) name in
          let* trait = u_path ns trait in
          let* args = u_list ns (u_expr ns) args in
          let* fields = u_fields ns fields in
          let* public = u_bool ns public in
          Some (Syntax.ImplBinding { name; trait; args; fields; public })
      | "DeclMacro", [ name; value; public; kind; output ] ->
          let* name = u_id ns name in
          let* value = u_expr ns value in
          let* public = u_bool ns public in
          let* kind = u_option ns (u_macro_ann ns) kind in
          let* output = u_option ns (u_expr ns) output in
          Some (Syntax.MacroBinding { name; value; public; kind; output })
      | "DeclMacroCall", [ f; args; public ] ->
          let* f = u_expr ns f in
          let* args = u_list ns (u_captured ns) args in
          let* public = u_bool ns public in
          Some (Syntax.MacroCallBinding { f; args; public })
      | "DeclPatternSyn", [ name; params; rhs; public ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* rhs = u_pat ns rhs in
          let* public = u_bool ns public in
          Some (Syntax.PatternSynBinding { name; params; rhs; public })
      | "DeclField", [ name; type_ ] -> let* name = u_string name in let* type_ = u_expr ns type_ in Some (Syntax.FieldBinding { name; type_ })
      | "DeclExport", [ m; names; public ] ->
          let* m = u_expr ns m in
          let* names = u_option ns (u_list ns u_string) names in
          let* public = u_bool ns public in
          Some (Syntax.ExportBinding { m; names; public })
      | "DeclOpen", [ m; label ] -> let* m = u_expr ns m in let* label = u_string label in Some (Syntax.OpenBinding (m, label))
      | "DeclHole", [ id ] -> let* id = u_id ns id in Some (Syntax.HoleBinding id)
      | "DeclSyntax", [ name; role; public ] ->
          let* name = u_id ns name in
          let* role = u_role ns role in
          let* public = u_bool ns public in
          Some (Syntax.SyntaxBinding { name; role; public })
      | "DeclItems", [ ts ] -> let* ts = u_tokens ns ts in Some (Syntax.Items ts)
      | "DeclInstantiate", [ form; rule; captures; from_unit; public ] ->
          let* form = u_id ns form in
          let* rule = u_rule ns rule in
          let* captures = u_captures ns captures in
          let* from_unit = u_option ns u_string from_unit in
          let* public = u_bool ns public in
          Some (Syntax.InstantiateBinding { inst = { form; rule; captures; from_unit }; public })
      | _ -> None)

(* ---- the interface the expander and elaborator use ---- *)

let wrap_stx ~nominals (stx : Syntax.t) : value =
  match nominals with None -> VStx (StxExpr stx) | Some ns -> w_expr ns stx

(* A macro argument, as the value of its parameter's kind (M9): a [Block] is the
   [Expr] it stands as, an [Id] the id its token names. *)
let wrap_capture ~nominals (c : Syntax.capture) : value =
  match nominals, c with
  | _, CapExpr e -> wrap_stx ~nominals e
  | _, CapBlock ts -> wrap_stx ~nominals (Syntax.synth (Block ts))
  | Some ns, CapId tok -> w_id ns (Syntax.token_id tok)
  | None, CapId tok -> VStx (StxExpr (Syntax.synth (Var (Syntax.token_id tok))))
  | Some ns, CapPattern p -> w_pat ns p
  | None, CapPattern p -> VStx (StxPattern p)
  | Some ns, CapDecls ds -> w_list ns (w_decl ns) ds
  | None, CapDecls ds -> VStx (StxDecls ds)
  | Some ns, CapDecl d -> w_decl ns d
  | None, CapDecl d -> VStx (StxDecls [ d ])
  | Some ns, CapTokens ts -> w_tokens ns ts
  | None, CapTokens ts -> wrap_stx ~nominals (Syntax.synth (Block ts))

let unwrap_stx ?nominals (v : value) : Syntax.t option =
  match nominals, v with
  | _, VStx (StxExpr stx) -> Some stx
  | Some ns, _ -> u_expr ns v
  | None, _ -> None

let wrap_stx_pat ~nominals (p : Syntax.pat) : value =
  match nominals with None -> VStx (StxPattern p) | Some ns -> w_pat ns p

let unwrap_stx_pat ?nominals (v : value) : Syntax.pat option =
  match nominals, v with
  | _, VStx (StxPattern p) -> Some p
  | Some ns, _ -> u_pat ns v
  | None, _ -> None

let wrap_stx_decl ~nominals (bindings : Syntax.struct_binding list) : value =
  match nominals, bindings with
  | None, [ b ] -> VStx (StxDecl b)
  | None, _ -> VStx (StxDecls bindings)
  | Some ns, _ -> w_list ns (w_decl ns) bindings

(* A [Decl] macro returns one declaration or a list of them. *)
let unwrap_stx_decl ?nominals (v : value) : Syntax.struct_binding list option =
  match nominals, v with
  | _, VStx (StxDecl b) -> Some [ b ]
  | _, VStx (StxDecls bs) -> Some bs
  | _, VStx (StxExpr { kind = Syntax.Module { bindings } | Syntax.Struct { bindings; _ }; _ }) -> Some bindings
  | Some ns, _ -> (
      match u_list ns (u_decl ns) v with
      | Some bs -> Some bs
      | None -> let* b = u_decl ns v in Some [ b ])
  | None, _ -> None
