exception Unsupported of string
exception Error of string

type value_decl = {
  decl_name : Syntax.id;
  decl_type : Syntax.t option;
  decl_value : Syntax.t;
  decl_recursive : bool;
}

type syntax_decl =
  | MacroSyntaxDecl of {
      syntax_name : Syntax.id;
      syntax_value : Syntax.t;
      syntax_export : Binding.operator_info;
    }
  | TemplateSyntaxDecl of {
      syntax_name : Syntax.id;
      syntax_export : Binding.operator_info;
    }

type env = {
  mutable operators : Binding.t;
  mutable template_captures : (string * Syntax_template.captured) list;
  exports_collector : Binding.operator_info list ref;
  load_syntax : (string -> Binding.operator_info list) option;
  syntax_class : Syntax_class.t;
  errors : Parse_error.t list ref;
}

(* The compiler-known base operators. Only [<-] remains here: it is core
   ref-assignment machinery ([BuiltinRefSet]), always in scope, not a stdlib
   feature. The arithmetic/comparison operators ([+ - * / % == != < > <= >=])
   and prefix [not] were demoted into the prelude as ordinary [pub infix] /
   [pub prefix] declarations; they now reach parse envs through the prelude's
   syntax exports (see [Elab_prelude.stdlib_syntax_exports]), not this table. *)
let base_operators () : Binding.t =
  let tbl = Binding.create () in
  List.iter (Binding.add_operator tbl)
    [ Binding.make_operator ~symbol:"<-" ~fixity:Binding.Infix ~precedence:1
        ~associativity:Binding.Right ~expansion:Binding.BuiltinRefSet () ];
  tbl

let env ?load_syntax ?(syntax_class = Syntax_class.Expr) () =
  { operators = base_operators (); template_captures = []; load_syntax; syntax_class;
    exports_collector = ref []; errors = ref [] }

let unsupported msg = raise (Unsupported msg)
let error msg = raise (Error msg)

let push_error env span kind =
  env.errors := { Parse_error.kind; span } :: !(env.errors)

let get_errors env = List.rev !(env.errors)

let rec first_some parsers input =
  match parsers with
  | [] -> None
  | parser :: rest -> (
      match parser input with
      | Some value -> Some value
      | None -> first_some rest input)

open Raw_syntax

let id ?span name =
  match span with
  | Some span -> Syntax.fresh_id ~span name
  | None -> Syntax.fresh_id name

let stx ?(span = Source_span.synthetic) kind = { Syntax.kind; span }

let atom ?span atom = stx ?span (Syntax.Atom atom)
let var ?span name = stx ?span (Syntax.Var (id ?span name))

(* An id written by a token carries the token's scope set. *)
let token_scope (term : Raw_syntax.t) =
  match term.datum with Raw_syntax.Token tok -> tok.scope | Raw_syntax.Group _ -> Scope_set.empty

let id_of (term : Raw_syntax.t) name = Syntax.fresh_id ~span:term.span ~scope:(token_scope term) name

let var_of (term : Raw_syntax.t) name = stx ~span:term.span (Syntax.Var (id_of term name))

let syntax_operator_arg ~span ~(use : Raw_syntax.t) (op : Binding.operator_info) operands =
  let fixity = match op.fixity with Binding.Prefix -> Syntax.PrefixOp | Binding.Infix -> Syntax.InfixOp in
  let use_span = use.span in
  stx ~span
    (Syntax.SyntaxOperatorUse
       { operator = id_of use op.symbol;
         fixity;
         operands;
         declaration_span = op.declaration_span;
         use_span;
         unit = op.unit })

let span_between (a : Source_span.t) (b : Source_span.t) =
  if a.synthetic || b.synthetic then Source_span.synthetic
  else
    Source_span.make ?file:a.file ~start_byte:a.start_byte ~end_byte:b.end_byte
      ?start_line:a.start_line ?start_col:a.start_col ?end_line:b.end_line
      ?end_col:b.end_col ()

let syntax_span (terms : Raw_syntax.t list) =
  match terms with
  | [] -> Source_span.synthetic
  | first :: rest ->
      let last = List.fold_left (fun _ t -> t) first rest in
      span_between first.span last.span

let is_separator (term : Raw_syntax.t) =
  match term.datum with
  | Token { kind = Semi; _ } -> true
  | _ -> false

let rec drop_separators = function
  | term :: rest when is_separator term -> drop_separators rest
  | terms -> terms

let token_text (term : Raw_syntax.t) =
  match term.datum with
  | Token { kind = Ident s | Operator s; _ } -> Some s
  | Token { kind = KwDeref; _ } -> Some "deref"
  | Token { kind = KwRef; _ } -> Some "ref"
  | Token { kind = KwModule; _ } -> Some "module"
  | Token { kind = KwStruct; _ } -> Some "struct"
  | Token { kind = KwType; _ } -> Some "type"
  | Token { kind = KwEffect; _ } -> Some "effect"
  | Token { kind = KwTrait; _ } -> Some "trait"
  | Token { kind = KwImpl; _ } -> Some "impl"
  | _ -> None

let token_kind kind term =
  match term.datum with
  | Token { kind = k; _ } -> k = kind
  | _ -> false

let keyword_name = function
  | KwFn -> Some "fn"
  | KwEnd -> Some "end"
  | KwLet -> Some "let"
  | KwFun -> Some "fun"
  | KwThen -> Some "then"
  | KwSig -> Some "sig"
  | KwElse -> Some "else"
  | KwMatch -> Some "match"
  | KwWith -> Some "with"
  | KwEffect -> Some "effect"
  | KwType -> Some "type"
  | KwModule -> Some "module"
  | KwStruct -> Some "struct"
  | KwImpl -> Some "impl"
  | KwTrait -> Some "trait"
  | KwPub -> Some "pub"
  | KwImport -> Some "import"
  | KwOpen -> Some "open"
  | KwMacro -> Some "macro"
  | KwSelf -> Some "self"
  | KwSelfType -> Some "Self"
  | KwRef -> Some "ref"
  | KwDeref -> Some "deref"
  | KwRec -> Some "rec"
  | KwCan -> Some "can"
  | KwPerform -> Some "perform"
  | KwResume -> Some "resume"
  | KwMethod -> Some "method"
  | _ -> None

let punct_name = function
  | LParen -> Some "("
  | RParen -> Some ")"
  | LBracket -> Some "["
  | RBracket -> Some "]"
  | LBrace -> Some "{"
  | RBrace -> Some "}"
  | Comma -> Some ","
  | Dot -> Some "."
  | Colon -> Some ":"
  | Equals -> Some "="
  | Semi -> Some ";"
  | Bar -> Some "|"
  | ThinArrow -> Some "->"
  | DatumComment -> Some "#_"
  | _ -> None

let debug_tokens label tokens =
  Printf.eprintf "%s [%d]: " label (List.length tokens);
  List.iter (fun t ->
    match t.Raw_syntax.datum with
    | Token { kind = Ident n; _ } -> Printf.eprintf "Ident(%s) " n
    | Token { kind; _ } -> Printf.eprintf "%s " (punct_name kind |> Option.value ~default:"?")
    | Group (Paren, _, _) -> Printf.eprintf "(...) "
    | Group _ -> Printf.eprintf "[...] ")
    tokens;
  Printf.eprintf "\n%!"

let ap ?span f explicitness arg = stx ?span (Syntax.Ap (f, explicitness, arg))

let is_expr_start env term =
  match term.datum with
  | Token { kind = Int _ | Char _ | String _ | Unit | KwUnit | KwSelf | KwSelfType | KwFn | KwMatch | KwRef | KwDeref | KwResume | KwImport | KwModule | KwSig | KwStruct | KwMacro | KwType | KwEffect | KwTrait | KwImpl | Ident _; _ } -> true
  | Token { kind = Operator s; _ } -> Option.is_some (Binding.find_operator env.operators ~fixity:Binding.Prefix ~syntax_class:env.syntax_class s)
  | Group (Raw_syntax.Paren, _, _) -> true
  | _ -> false

let is_adjacent_postfix (lhs : Syntax.t) (term : Raw_syntax.t) =
  lhs.span.synthetic || term.span.synthetic || lhs.span.end_byte = term.span.start_byte

let spans_adjacent (lhs : Source_span.t) (rhs : Source_span.t) =
  lhs.synthetic || rhs.synthetic || lhs.end_byte = rhs.start_byte

let require_adjacent_postfix lhs term what =
  if not (is_adjacent_postfix lhs term) then
    error (what ^ " must be adjacent to the callee; whitespace application is not supported")

let require_adjacent_span lhs rhs what =
  if not (spans_adjacent lhs rhs) then
    error (what ^ " must be adjacent; whitespace form is not supported")

let split_last_expr terms =
  let rec go acc = function
    | [] -> error "expected trailing expression"
    | [ last ] -> (List.rev acc, [ last ])
    | term :: rest -> go (term :: acc) rest
  in
  go [] (drop_separators terms)

(* [M.N.x]: the head is a bare name, carried as an id; the rest are labels. *)
let path_from_terms terms : Syntax.path * Raw_syntax.t list =
  match drop_separators terms with
  | ({ datum = Token { kind = Ident head; _ }; _ } as head_term) :: rest ->
      let rec members acc = function
        | dot :: { datum = Token { kind = Ident name; _ }; _ } :: rest when token_kind Dot dot ->
            members (name :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      let members, rest = members [] rest in
      ({ Syntax.head = id_of head_term head; members; head_choice = None }, rest)
  | _ -> error "expected dotted identifier"

let binding_name_term = function
  | ({ datum = Token { kind = Ident name; _ }; _ } as term) -> Some (id_of term name)
  | { datum = Group (Raw_syntax.Paren, [ ({ datum = Token { kind = Operator name | Ident name; _ }; _ } as term) ], _); _ } ->
      Some (id_of term name)
  | _ -> None

let unit ?span () = atom ?span Atom.Unit

let unit_type ?span () = var ?span "Unit"

let param_id ?type_ explicitness name = { Syntax.name; type_; trait_bounds = []; explicitness }

let param ?span ?type_ explicitness name = param_id ?type_ explicitness (id ?span name)

let split_commas terms =
  let rec go current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when token_kind Comma term -> go [] (List.rev current :: acc) rest
    | term :: rest -> go (term :: current) acc rest
  in
  go [] [] terms

let parse_all parse terms =
  let first_unconsumed_name term =
    match term.datum with
    | Group (Paren, _, _) -> "("
    | Group (Bracket, _, _) -> "["
    | Group (Brace, _, _) -> "{"
    | Token _ -> (
        match token_text term with
        | Some text -> text
        | None -> (
            match term.datum with
            | Token { kind; _ } ->
                Option.value ~default:"<syntax>" (punct_name kind)
            | Group _ -> "<syntax>"))
  in
  let terms = drop_separators terms in
  match terms with
  | [] -> error "expected expression"
  | _ ->
      let expr, rest = parse terms in
      let rest = drop_separators rest in
      if rest = [] then expr
      else
        let first =
          match rest with
          | term :: _ -> first_unconsumed_name term
          | [] -> "<none>"
        in
        unsupported ("unconsumed terms after expression: " ^ first)

let rec split_at_pred pred acc = function
  | [] -> None
  | term :: rest when pred term -> Some (List.rev acc, term, rest)
  | term :: rest -> split_at_pred pred (term :: acc) rest

let split_at_token kind terms = split_at_pred (token_kind kind) [] terms

(* [=>] separates a pattern from its result: match arms, effect branches and
   template rules. It lexes as an ordinary operator token but is reserved. *)
let is_fat_arrow term =
  match term.datum with
  | Token { kind = Operator "=>"; _ } -> true
  | _ -> false

let split_at_fat_arrow terms = split_at_pred is_fat_arrow [] terms

(* Bodies are reader groups, so every split below is flat: nothing nested can
   hold a separator at this level. *)
let split_by_top_level is_separator terms =
  let rec go current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when is_separator term -> go [] (List.rev current :: acc) rest
    | term :: rest -> go (term :: current) acc rest
  in
  go [] [] terms |> List.filter (fun part -> drop_separators part <> [])

let split_by_top_level_bar terms = split_by_top_level (token_kind Bar) terms

(* [type A = … and B = …]: [and] separates the members of a type chain. It is
   contextual, not a keyword - it only means this directly inside a [type]. *)
let split_type_chain terms =
  split_by_top_level (fun term -> match term.datum with Token { kind = Ident "and"; _ } -> true | _ -> false) terms

(* Arms are split by the rule "a pattern holds no bare [=>], a result holds no
   bare [|]": a [|] ends an arm only once its [=>] has been seen. *)
let split_match_branches terms =
  let rec go seen_arrow current acc = function
    | [] -> List.rev (List.rev current :: acc)
    | term :: rest when token_kind Bar term && drop_separators current = [] -> go false current acc rest
    | term :: rest when token_kind Bar term && seen_arrow -> go false [] (List.rev current :: acc) rest
    | term :: rest -> go (seen_arrow || is_fat_arrow term) (term :: current) acc rest
  in
  go false [] [] terms |> List.filter (fun part -> drop_separators part <> [])

(* [module { … }], [sig { … }], [struct { … }]: the items of a brace group. *)
let brace_body what terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Brace, items, span); _ } :: rest -> (items, rest, span)
  | _ -> error (what ^ " is written " ^ what ^ " { … }")

let ensure_no_rest what rest =
  match drop_separators rest with
  | [] -> ()
  | _ -> error (what ^ " has trailing terms")

let with_operator_scope env f =
  (* [operators] is a mutable Hashtbl; copy it so operator definitions inside the
     nested scope do not leak to the outer env (outer ones stay visible). *)
  f { env with operators = Binding.copy env.operators; template_captures = env.template_captures }

let syntax_name term = token_text term

let required_syntax_name term what =
  match syntax_name term with Some name -> name | None -> error (what ^ " requires an operator or identifier name")

let load_syntax_exports env path =
  match env.load_syntax with
  | None -> ()
  | Some load ->
      let exports = Binding.from_unit path (load path) in
      (match Binding.duplicate_operator_exports_message exports with Some msg -> error msg | None -> ());
      Binding.apply_operator_exports env.operators exports

(* Recursively scan a term list for [import "path"] occurrences and eagerly
   harvest each one's syntax exports. Operator delivery everywhere else goes
   through the in-order [Enforest_forms.parse_import] harvest; this whole-body
   pre-scan survives at exactly ONE call site — [parse_syntax_template_decl] in
   enforest.ml — where a syntax-template body's imports must be resolved before
   the branches are enforested (for out-of-order operator use and for circular
   syntax-visit detection). See that call site for the full rationale. *)
let rec load_imports_in_terms env = function
  | { datum = Token { kind = KwImport; _ }; _ }
    :: { datum = Token { kind = String path; _ }; _ } :: rest ->
      load_syntax_exports env path;
      load_imports_in_terms env rest
  | { datum = Group (_, items, _); _ } :: rest ->
      load_imports_in_terms env items;
      load_imports_in_terms env rest
  | _ :: rest -> load_imports_in_terms env rest
  | [] -> ()


and split_statements terms =
  split_by_top_level (fun term -> is_separator term || token_kind Comma term) terms

let desc_token term =
  match term.Raw_syntax.datum with
  | Token { kind = Ident n; _ } -> n
  | Token { kind; _ } -> Raw_syntax.show_token_kind kind
  | Group (Paren, _, _) -> "(...)"
  | Group (Bracket, _, _) -> "[...]"
  | Group (Brace, _, _) -> "{...}"

let rec push_and_recover env span kind rest =
  push_error env span kind;
  skip_to_statement_boundary rest

and skip_to_statement_boundary = function
  | [] -> []
  | t :: rest when is_separator t -> rest
  | _ :: rest -> skip_to_statement_boundary rest
