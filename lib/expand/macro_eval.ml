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
  ann_arg : value;
  macro_ann : value;
  quote_hole : value;
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
  record [ ("name", w_string id.name); ("span", w_span ns id.span); ("scope", VAtom (Scopes id.scope)) ]

let w_explicitness ns = function
  | Explicitness.Explicit -> con ns.explicitness "Explicit" []
  | Explicitness.Implicit -> con ns.explicitness "Implicit" []

let w_atom ns (a : Atom.t) =
  match a with
  | I64 n -> con ns.atom_val "I64Atom" [ VAtom (I64 n) ]
  | Char c -> con ns.atom_val "CharAtom" [ VAtom (Char c) ]
  | String s -> con ns.atom_val "StringAtom" [ VAtom (String s) ]
  | Unit -> con ns.atom_val "UnitAtom" []
  | Scopes s -> con ns.atom_val "ScopesAtom" [ VAtom (Scopes s) ]

let atom_ty_names =
  [ (Atom_ty.TI64, "TyI64"); (TUnit, "TyUnit"); (TChar, "TyChar"); (TString, "TyString");
    (TScopes, "TyScopes"); (TAbsurd, "TyAbsurd") ]

let w_atom_ty ns t = con ns.atom_ty (List.assoc t atom_ty_names) []

let w_trait_bound ns (b : Trait_bound.t) =
  record [ ("path", w_list ns w_string b.trait_path); ("name", w_string b.trait_name) ]

let w_ann_arg ns = function
  | Syntax.MacroAnnotation.Wildcard -> con ns.ann_arg "AnnWildcard" []
  | Named n -> con ns.ann_arg "AnnNamed" [ w_string n ]
  | Qualified (path, last) -> con ns.ann_arg "AnnQualified" [ w_list ns w_string path; w_string last ]

let w_macro_ann ns = function
  | Syntax.MacroAnnotation.Expr arg -> con ns.macro_ann "AnnExpr" [ w_option ns (w_ann_arg ns) arg ]
  | LegacyExprBinder n -> con ns.macro_ann "AnnLegacyBinder" [ w_string n ]
  | Decl -> con ns.macro_ann "AnnDecl" []

let w_fixity ns = function
  | Syntax.PrefixOp -> con ns.fixity "PrefixFixity" []
  | InfixOp -> con ns.fixity "InfixFixity" []

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
  | Annotated { inner; typ } -> e "RawAnnotated" [ x inner; x typ ]
  | Prod xs -> e "RawProd" [ w_list ns x xs ]
  | ProdTy xs -> e "RawProdTy" [ w_list ns x xs ]
  | Arrow (ex, name, dom, row, cod) ->
      e "RawArrow" [ w_explicitness ns ex; w_option ns (w_id ns) name; x dom; w_option ns (w_effect_row ns) row; x cod ]
  | FieldAccess (r, f) -> e "RawFieldAccess" [ x r; w_string f ]
  | Proj (r, i) -> e "RawProj" [ x r; w_i64 i ]
  | RecordConstruct { typ; fields } -> e "RawRecordConstruct" [ x typ; w_fields ns fields ]
  | Struct { con_fields; bindings } -> e "RawStruct" [ w_fields ns con_fields; w_list ns (w_decl ns) bindings ]
  | Module { bindings } -> e "RawModule" [ w_list ns (w_decl ns) bindings ]
  | Import path -> e "RawImport" [ w_string path ]
  | Open (m, body) -> e "RawOpen" [ x m; x body ]
  | RecordTypeDef { name; params; fields; body } ->
      e "RawRecordTypeDef" [ w_id ns name; ids params; w_fields ns fields; x body ]
  | TypeDef { name; params; ctors; body } ->
      e "RawTypeDef" [ w_type_decl ns { name; params; ctors }; x body ]
  | EffectDef { name; params; ops; body } ->
      e "RawEffectDef" [ w_id ns name; ids params; w_list ns (w_effect_op ns) ops; x body ]
  | TraitDef { name; params; fields; body } ->
      e "RawTraitDef" [ w_id ns name; ids params; w_fields ns fields; x body ]
  | ImplDef { name; trait_path; trait_name; args; fields; body } ->
      e "RawImplDef"
        [ w_option ns (w_id ns) name; w_list ns w_string trait_path; w_string trait_name;
          w_list ns x args; w_fields ns fields; x body ]
  | Perform { effect_path; op; arg } -> e "RawPerform" [ w_list ns w_string effect_path; w_string op; x arg ]
  | Resume a -> e "RawResume" [ x a ]
  | RefNew a -> e "RawRefNew" [ x a ]
  | RefGet a -> e "RawRefGet" [ x a ]
  | RefSet (l, r) -> e "RawRefSet" [ x l; x r ]
  | Match (scrut, branches) -> e "RawMatch" [ x scrut; w_list ns (w_branch ns) branches ]
  | Stx inner -> e "RawStx" [ x inner ]
  | Quote { template; holes } ->
      e "RawQuote"
        [ x template; w_list ns (fun (n, h) -> con ns.quote_hole "MkQuoteHole" [ w_string n; x h ]) holes ]
  | MacroDef { name; value; body; kind } ->
      e "RawMacroDef" [ w_id ns name; x value; x body; w_option ns (w_macro_ann ns) kind ]
  | MacroCall (f, args) -> e "RawMacroCall" [ x f; w_list ns x args ]
  | SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit } ->
      e "RawOperatorUse"
        [ w_id ns operator; w_fixity ns fixity; w_list ns x operands; w_span ns declaration_span;
          w_span ns use_span; w_option ns w_string unit ]

and w_fields ns fields = w_list ns (fun (n, v) -> con ns.field "MkField" [ w_string n; w_expr ns v ]) fields

and w_param ns (p : Syntax.param) =
  con ns.param "MkParam"
    [ w_id ns p.name; w_option ns (w_expr ns) p.type_; w_list ns (w_trait_bound ns) p.trait_bounds;
      w_explicitness ns p.explicitness ]

and w_effect_row ns (row : Syntax.effect_row) =
  con ns.effect_row "MkEffectRow" [ w_list ns (w_expr ns) row.effects; w_option ns (w_expr ns) row.tail ]

and w_effect_op ns (op : Syntax.effect_op) =
  con ns.effect_op "MkEffectOp" [ w_string op.name; w_expr ns op.input; w_expr ns op.output ]

and w_type_decl ns (d : Syntax.type_decl) =
  con ns.type_decl "MkTypeDecl"
    [ w_id ns d.name; w_list ns (w_id ns) d.params;
      w_list ns (fun (c, payloads) -> con ns.ctor "MkCtor" [ w_id ns c; w_list ns (w_expr ns) payloads ]) d.ctors ]

and w_branch ns = function
  | Syntax.ValueBranch (p, body) -> con ns.branch "ValueBranch" [ w_pat ns p; w_expr ns body ]
  | EffectBranch { effect_path; op; arg_pat; body } ->
      con ns.branch "EffectBranch" [ w_list ns w_string effect_path; w_string op; w_pat ns arg_pat; w_expr ns body ]

and w_pat ns (p : Syntax.pat) =
  (* Patterns carry no span in [Syntax.pat]; the reflected span is always [None]. *)
  let pc name spine = con ns.pat name (w_option ns Fun.id None :: spine) in
  let pat_field (n, p) = con ns.pat_field "MkPatField" [ w_string n; w_option ns (w_pat ns) p ] in
  match p with
  | PatWild -> pc "RawPatWild" []
  | PatBind id -> pc "RawPatBind" [ w_id ns id ]
  | PatCon (path, ctor, args) ->
      (* A constructor head is a plain string in [Syntax.pat]: no span or scope. *)
      pc "RawPatCon"
        [ w_list ns w_string path; w_id ns (Syntax.fresh_id ctor); w_list ns (w_pat ns) args ]
  | PatAtom a -> pc "RawPatAtom" [ w_atom ns a ]
  | PatProd ps -> pc "RawPatProd" [ w_list ns (w_pat ns) ps ]
  | PatOr (l, r) -> pc "RawPatOr" [ w_pat ns l; w_pat ns r ]
  | PatRecord { typ_path; typ; fields; partial } ->
      pc "RawPatRecord" [ w_list ns w_string typ_path; w_string typ; w_list ns pat_field fields; w_bool ns partial ]
  | PatStructType { fields; partial } ->
      pc "RawPatStructType" [ w_list ns pat_field (List.map (fun (n, p) -> (n, Some p)) fields); w_bool ns partial ]
  | PatType t -> pc "RawPatType" [ w_atom_ty ns t ]

and w_decl ns (b : Syntax.struct_binding) =
  let d name spine = con ns.decl name spine in
  let ids = w_list ns (w_id ns) in
  match b with
  | LetBinding { name; value; public; recursive } ->
      d "DeclLet" [ w_id ns name; w_expr ns value; w_bool ns public; w_bool ns recursive ]
  | MethodBinding { name; params; body; public } ->
      d "DeclMethod" [ w_id ns name; w_list ns (w_param ns) params; w_expr ns body; w_bool ns public ]
  | TypeBinding { members; public } -> d "DeclType" [ w_list ns (w_type_decl ns) members; w_bool ns public ]
  | RecordTypeBinding { name; params; fields; public } ->
      d "DeclRecordType" [ w_id ns name; ids params; w_fields ns fields; w_bool ns public ]
  | EffectBinding { name; params; ops; public } ->
      d "DeclEffect" [ w_id ns name; ids params; w_list ns (w_effect_op ns) ops; w_bool ns public ]
  | TraitBinding { name; params; fields; public } ->
      d "DeclTrait" [ w_id ns name; ids params; w_fields ns fields; w_bool ns public ]
  | ImplBinding { name; trait_path; trait_name; args; fields; public } ->
      d "DeclImpl"
        [ w_option ns (w_id ns) name; w_list ns w_string trait_path; w_string trait_name;
          w_list ns (w_expr ns) args; w_fields ns fields; w_bool ns public ]
  | MacroBinding { name; value; public; kind } ->
      d "DeclMacro" [ w_id ns name; w_expr ns value; w_bool ns public; w_option ns (w_macro_ann ns) kind ]
  | MacroCallBinding { f; args } -> d "DeclMacroCall" [ w_expr ns f; w_list ns (w_expr ns) args ]
  | PatternSynBinding { name; params; rhs; public } ->
      d "DeclPatternSyn" [ w_id ns name; ids params; w_pat ns rhs; w_bool ns public ]
  | OpenBinding m -> d "DeclOpen" [ w_expr ns m ]

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
  let* scope = match u_field "scope" v with Some (VAtom (Scopes s)) -> Some s | _ -> None in
  Some { Syntax.name; span; scope }

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
  | Some ("ScopesAtom", [ VAtom (Scopes s) ]) -> Some (Scopes s)
  | _ -> None

let u_atom_ty ns v =
  match payload ns.atom_ty v with
  | Some (name, []) -> List.find_map (fun (t, n) -> if String.equal n name then Some t else None) atom_ty_names
  | _ -> None

let u_trait_bound ns v : Trait_bound.t option =
  let* trait_path = let* p = u_field "path" v in u_list ns u_string p in
  let* trait_name = let* n = u_field "name" v in u_string n in
  Some { Trait_bound.trait_path; trait_name }

let u_ann_arg ns v : Syntax.MacroAnnotation.arg option =
  match payload ns.ann_arg v with
  | Some ("AnnWildcard", []) -> Some Wildcard
  | Some ("AnnNamed", [ n ]) -> let* n = u_string n in Some (Syntax.MacroAnnotation.Named n)
  | Some ("AnnQualified", [ p; l ]) ->
      let* p = u_list ns u_string p in let* l = u_string l in Some (Syntax.MacroAnnotation.Qualified (p, l))
  | _ -> None

let u_macro_ann ns v : Syntax.MacroAnnotation.t option =
  match payload ns.macro_ann v with
  | Some ("AnnExpr", [ a ]) -> let* a = u_option ns (u_ann_arg ns) a in Some (Syntax.MacroAnnotation.Expr a)
  | Some ("AnnLegacyBinder", [ n ]) -> let* n = u_string n in Some (Syntax.MacroAnnotation.LegacyExprBinder n)
  | Some ("AnnDecl", []) -> Some Syntax.MacroAnnotation.Decl
  | _ -> None

let u_fixity ns v =
  match payload ns.fixity v with
  | Some ("PrefixFixity", []) -> Some Syntax.PrefixOp
  | Some ("InfixFixity", []) -> Some Syntax.InfixOp
  | _ -> None

let rec u_expr ns (v : value) : Syntax.t option =
  let x = u_expr ns and ids = u_list ns (u_id ns) and strings = u_list ns u_string in
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
      | "RawStruct", [ con_fields; bindings ] ->
          let* con_fields = u_fields ns con_fields in
          let* bindings = u_list ns (u_decl ns) bindings in
          mk (Struct { con_fields; bindings })
      | "RawModule", [ bindings ] -> let* bindings = u_list ns (u_decl ns) bindings in mk (Module { bindings })
      | "RawImport", [ path ] -> let* path = u_string path in mk (Import path)
      | "RawOpen", [ m; body ] -> let* m = x m in let* body = x body in mk (Open (m, body))
      | "RawRecordTypeDef", [ name; params; fields; body ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* fields = u_fields ns fields in
          let* body = x body in
          mk (RecordTypeDef { name; params; fields; body })
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
      | "RawImplDef", [ name; trait_path; trait_name; args; fields; body ] ->
          let* name = u_option ns (u_id ns) name in
          let* trait_path = strings trait_path in
          let* trait_name = u_string trait_name in
          let* args = u_list ns x args in
          let* fields = u_fields ns fields in
          let* body = x body in
          mk (ImplDef { name; trait_path; trait_name; args; fields; body })
      | "RawPerform", [ path; op; arg ] ->
          let* effect_path = strings path in
          let* op = u_string op in
          let* arg = x arg in
          mk (Perform { effect_path; op; arg })
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
          let* holes =
            u_list ns
              (fun h ->
                match payload ns.quote_hole h with
                | Some ("MkQuoteHole", [ n; e ]) -> let* n = u_string n in let* e = x e in Some (n, e)
                | _ -> None)
              holes
          in
          mk (Quote { template; holes })
      | "RawMacroDef", [ name; value; body; kind ] ->
          let* name = u_id ns name in
          let* value = x value in
          let* body = x body in
          let* kind = u_option ns (u_macro_ann ns) kind in
          mk (MacroDef { name; value; body; kind })
      | "RawMacroCall", [ f; args ] -> let* f = x f in let* args = u_list ns x args in mk (MacroCall (f, args))
      | "RawOperatorUse", [ operator; fixity; operands; declaration_span; use_span; unit ] ->
          let* operator = u_id ns operator in
          let* fixity = u_fixity ns fixity in
          let* operands = u_list ns x operands in
          let* declaration_span = u_span ns declaration_span in
          let* use_span = u_span ns use_span in
          let* unit = u_option ns u_string unit in
          mk (SyntaxOperatorUse { operator; fixity; operands; declaration_span; use_span; unit })
      | _ -> None)

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
      let* trait_bounds = u_list ns (u_trait_bound ns) bounds in
      let* explicitness = u_explicitness ns ex in
      Some { Syntax.name; type_; trait_bounds; explicitness }
  | _ -> None

and u_effect_row ns v : Syntax.effect_row option =
  match payload ns.effect_row v with
  | Some ("MkEffectRow", [ effects; tail ]) ->
      let* effects = u_list ns (u_expr ns) effects in
      let* tail = u_option ns (u_expr ns) tail in
      Some { Syntax.effects; tail }
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
      let* ctors =
        u_list ns
          (fun c ->
            match payload ns.ctor c with
            | Some ("MkCtor", [ cname; payloads ]) ->
                let* cname = u_id ns cname in
                let* payloads = u_list ns (u_expr ns) payloads in
                Some (cname, payloads)
            | _ -> None)
          ctors
      in
      Some { Syntax.name; params; ctors }
  | _ -> None

and u_branch ns v : Syntax.match_branch option =
  match payload ns.branch v with
  | Some ("ValueBranch", [ p; body ]) -> let* p = u_pat ns p in let* body = u_expr ns body in Some (Syntax.ValueBranch (p, body))
  | Some ("EffectBranch", [ path; op; p; body ]) ->
      let* effect_path = u_list ns u_string path in
      let* op = u_string op in
      let* arg_pat = u_pat ns p in
      let* body = u_expr ns body in
      Some (Syntax.EffectBranch { effect_path; op; arg_pat; body })
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
      | "RawPatCon", [ path; ctor; ps ] ->
          let* path = u_list ns u_string path in
          let* ctor = u_id ns ctor in
          let* ps = u_list ns (u_pat ns) ps in
          Some (Syntax.PatCon (path, ctor.name, ps))
      | "RawPatAtom", [ a ] -> let* a = u_atom ns a in Some (Syntax.PatAtom a)
      | "RawPatProd", [ ps ] -> let* ps = u_list ns (u_pat ns) ps in Some (Syntax.PatProd ps)
      | "RawPatOr", [ l; r ] -> let* l = u_pat ns l in let* r = u_pat ns r in Some (Syntax.PatOr (l, r))
      | "RawPatRecord", [ path; typ; fields; partial ] ->
          let* typ_path = u_list ns u_string path in
          let* typ = u_string typ in
          let* fields = pat_fields fields in
          let* partial = u_bool ns partial in
          Some (Syntax.PatRecord { typ_path; typ; fields; partial })
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
      | "DeclLet", [ name; value; public; recursive ] ->
          let* name = u_id ns name in
          let* value = u_expr ns value in
          let* public = u_bool ns public in
          let* recursive = u_bool ns recursive in
          Some (Syntax.LetBinding { name; value; public; recursive })
      | "DeclMethod", [ name; params; body; public ] ->
          let* name = u_id ns name in
          let* params = u_list ns (u_param ns) params in
          let* body = u_expr ns body in
          let* public = u_bool ns public in
          Some (Syntax.MethodBinding { name; params; body; public })
      | "DeclType", [ members; public ] ->
          let* members = u_list ns (u_type_decl ns) members in
          let* public = u_bool ns public in
          Some (Syntax.TypeBinding { members; public })
      | "DeclRecordType", [ name; params; fields; public ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* fields = u_fields ns fields in
          let* public = u_bool ns public in
          Some (Syntax.RecordTypeBinding { name; params; fields; public })
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
      | "DeclImpl", [ name; trait_path; trait_name; args; fields; public ] ->
          let* name = u_option ns (u_id ns) name in
          let* trait_path = u_list ns u_string trait_path in
          let* trait_name = u_string trait_name in
          let* args = u_list ns (u_expr ns) args in
          let* fields = u_fields ns fields in
          let* public = u_bool ns public in
          Some (Syntax.ImplBinding { name; trait_path; trait_name; args; fields; public })
      | "DeclMacro", [ name; value; public; kind ] ->
          let* name = u_id ns name in
          let* value = u_expr ns value in
          let* public = u_bool ns public in
          let* kind = u_option ns (u_macro_ann ns) kind in
          Some (Syntax.MacroBinding { name; value; public; kind })
      | "DeclMacroCall", [ f; args ] ->
          let* f = u_expr ns f in
          let* args = u_list ns (u_expr ns) args in
          Some (Syntax.MacroCallBinding { f; args })
      | "DeclPatternSyn", [ name; params; rhs; public ] ->
          let* name = u_id ns name in
          let* params = ids params in
          let* rhs = u_pat ns rhs in
          let* public = u_bool ns public in
          Some (Syntax.PatternSynBinding { name; params; rhs; public })
      | "DeclOpen", [ m ] -> let* m = u_expr ns m in Some (Syntax.OpenBinding m)
      | _ -> None)

(* ---- the interface the expander and elaborator use ---- *)

let wrap_stx ~nominals (stx : Syntax.t) : value =
  match nominals with None -> VStx (StxExpr stx) | Some ns -> w_expr ns stx

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
