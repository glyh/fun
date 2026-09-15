exception Unsupported of string
exception Error of string

type value_decl = {
  decl_name : Syntax.id;
  decl_type : Syntax.t option;
  decl_value : Syntax.t;
  decl_recursive : bool;
}

(* A syntactic role's declaration: [syntax], [infix], [prefix]. [macro_value]
   is the procedural macro an operator's body defines, if it has one. *)
type role_decl = { role_name : Syntax.id; role : Syntax.role; macro_value : Syntax.t option }

type env = {
  (* The roles the forms are read with: the expander's binder table when
     reading as expansion reaches a form; a copy when reading quoted syntax. *)
  operators : Binding.t;
  (* Reading quoted syntax - a quote, or a rule's replacement (M10): it is
     parsed completely where it is written, and a role declared in it is
     registered as it is read, for the statements after it. *)
  eager : bool;
  (* Whether a role declared while reading is registered as it is read, for the
     statements after it: reading quoted syntax, or a struct's items, which
     are read together (their bodies still wait for expansion). *)
  registers : bool;
  (* The captures of the rules enclosing quoted syntax, which a rule declared
     in it may use (M9). *)
  holes : string list;
  (* Roles registered while reading eagerly, so a statement that declared one
     scopes it over the statements after it. *)
  mutable declared : int;
  errors : Parse_error.t list ref;
  (* The roles the unit a module expression denotes exports, when it denotes one:
     a dotted group reference [M.g] reads [g] among them. The expander answers. *)
  unit_roles : Syntax.t -> (string * Syntax.role) list;
  (* The parameter kinds of the macro a call's head names, when it names one:
     its arguments are read as those kinds (M9). The expander answers. *)
  macro_params : Syntax.t -> Syntax.hole_kind list option;
}

(* The compiler-known base roles, always in scope: [<-], ref assignment, and
   its order group [assignment] - [weakest] (weaker than every group that states
   no relation to it) and non-associative, so
   [r <- x + 1] is [r <- (x + 1)] and [a <- b <- c] is an error. *)
let assignment_order =
  { Syntax.group = "assignment@base"; group_name = "assignment"; group_assoc = Syntax.NonAssoc; weakest = true; stronger_than = []; weaker_than = [] }

let base_roles (tbl : Binding.t) =
  Binding.extend tbl ~name:"assignment" ~scope:Scope_set.empty ~kind:Binding.Role ~resolved_name:"assignment"
    ~role:(Binding.role ~fixity:Syntax.PrefixOp ~order:assignment_order Syntax.OrderGroup);
  Binding.extend tbl ~name:"<-" ~scope:Scope_set.empty ~kind:Binding.Role ~resolved_name:"<-"
    ~role:(Binding.role ~fixity:Syntax.InfixOp ~order:assignment_order Syntax.AssignRef);
  Binding.extend tbl ~name:"type" ~scope:Scope_set.empty ~kind:Binding.Role ~resolved_name:"type"
    ~role:(Binding.role ~fixity:Syntax.PrefixOp Syntax.TypeDeclaration);
  (* [~>]: read like [->], a base role so the arrow family needs no lexer rule. *)
  Binding.extend tbl ~name:"~>" ~scope:Scope_set.empty ~kind:Binding.Role ~resolved_name:"~>"
    ~role:(Binding.role ~fixity:Syntax.InfixOp Syntax.PolyArrow)

(* Where an expression is read, which decides what may continue it. Precedence
   among operators is relative (brackets-decide-grouping): an operand continues
   with an infix operator only if the operator binds tighter than the one whose
   operand it is. The built-in grammar keeps two fixed positions: [ArrowRhs],
   where [->] continues, and [Tight], an argument no infix
   operator continues. *)
type prec =
  | Top
  | ArrowRhs
  | Tight
  | Operand of string * Syntax.role

(* An order group's identity: its declaration, told apart by a counter. The
   [@] no written name contains keeps it apart from any spelling. *)
let order_counter = ref 0

let fresh_order_group name =
  incr order_counter;
  Printf.sprintf "%s@%d" name !order_counter

(* Reading forms as expansion reaches them, with the expander's roles. *)
let lazy_env ?(macro_params = fun _ -> None) ?(unit_roles = fun _ -> []) operators =
  { operators; eager = false; registers = false; holes = []; declared = 0; errors = ref []; macro_params; unit_roles }

(* Reading quoted syntax where it is written. *)
let eager_env ?(holes = []) env =
  if env.eager then { env with holes = holes @ env.holes }
  else { env with operators = Binding.copy env.operators; eager = true; registers = true; holes; declared = 0 }

(* Reading a struct's items together, with its own copy of the roles. *)
let registering_env env =
  if env.registers then env else { env with operators = Binding.copy env.operators; registers = true; declared = 0 }

(* Scopes the enforester mints for the statements of quoted syntax count down
   from -1, apart from the expander's. *)
let scope_counter = ref (-1)

let fresh_scope () =
  let scope = !scope_counter in
  decr scope_counter;
  scope

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

(* [Decl] in a macro annotation: the prelude's [Syntax.Decl], its head written at
   the annotation token's scopes (as a parameter kind's type is). *)
let syntax_decl_type (decl : Raw_syntax.t) =
  stx ~span:decl.span
    (Syntax.FieldAccess (var_of decl Compiler_names.Module_name.syntax, Compiler_names.Syntax_name.decl))

let syntax_operator_arg ~span ~(use : Raw_syntax.t) name (role : Syntax.role) operands =
  stx ~span
    (Syntax.SyntaxOperatorUse
       { operator = id_of use name;
         fixity = role.fixity;
         operands;
         declaration_span = role.declared_at;
         use_span = use.span;
         unit = role.from_unit })

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
  | Token { kind = KwEnum; _ } -> Some "enum"
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
  | KwModule -> Some "module"
  | KwStruct -> Some "struct"
  | KwEnum -> Some "enum"
  | KwImpl -> Some "impl"
  | KwTrait -> Some "trait"
  | KwPub -> Some "pub"
  | KwImport -> Some "import"
  | KwOpen -> Some "open"
  | KwExport -> Some "export"
  | KwMacro -> Some "macro"
  | KwSelf -> Some "self"
  | KwSelfType -> Some "Self"
  | KwRef -> Some "ref"
  | KwDeref -> Some "deref"
  | KwRec -> Some "rec"
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
  | Token { kind = Int _ | Char _ | String _ | Unit | KwUnit | KwSelf | KwSelfType | KwFn | KwMatch | KwRef | KwDeref | KwResume | KwImport | KwModule | KwSig | KwStruct | KwEnum | KwMacro | KwEffect | KwTrait | KwImpl | Ident _; _ } -> true
  | Token { kind = Operator s; scope; _ } -> Option.is_some (Binding.find_role env.operators ~fixity:Syntax.PrefixOp ~scope s)
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

(* Arms - match arms, effect branches, syntax rules - are [pattern => result],
   ended by brackets or a comma: a result that is exactly one [{ … }] group
   ends at its [}] (a comma after it is optional); any other result ends at the
   next top-level [,]. A [|] belongs to the pattern (union). An arm with no
   [=>] is returned whole, for the caller to name what it lacks. *)
let split_match_branches terms =
  let skip_comma = function term :: rest when token_kind Comma term -> rest | rest -> rest in
  let rec go acc terms =
    match drop_separators terms with
    | [] -> List.rev acc
    | term :: _ when token_kind Bar term ->
        error "an arm does not begin with |: write pattern => result, … (| is pattern union)"
    | terms -> (
        match split_at_fat_arrow terms with
        | None -> List.rev (terms :: acc)
        | Some (pattern, arrow, ({ datum = Group (Raw_syntax.Brace, _, _); _ } as body) :: after) ->
            let after = skip_comma after in
            if drop_separators after <> [] && Option.is_none (split_at_fat_arrow after) then
              error "an arm whose result is { … } ends at its }: parenthesise a longer result, pattern => ({ … } …)";
            go ((pattern @ [ arrow; body ]) :: acc) after
        | Some (pattern, arrow, result) ->
            let result, after =
              match split_at_token Comma result with
              | Some (result, _, after) -> (result, after)
              | None -> (result, [])
            in
            if Option.is_some (split_at_fat_arrow result) then
              error "an arm's result ends at , before the next arm: pattern => result, pattern => …";
            go ((pattern @ (arrow :: result)) :: acc) after)
  in
  go [] terms

(* [module { … }], [sig { … }], [struct { … }]: the items of a brace group. *)
let brace_body what terms =
  match drop_separators terms with
  | { datum = Group (Raw_syntax.Brace, items, span); _ } :: rest -> (items, rest, span)
  | _ -> error (what ^ " is written " ^ what ^ " { … }")

let ensure_no_rest what rest =
  match drop_separators rest with
  | [] -> ()
  | _ -> error (what ^ " has trailing terms")

let syntax_name term = token_text term

let split_statements terms =
  split_by_top_level (fun term -> is_separator term || token_kind Comma term) terms

(* A definition context's statements, read in order as quoted syntax: [f
   ~last stmt] reads one. A statement that declared a role adds its own scope
   to the statements after it, so the role is visible after it and neither
   before it nor outside the context (M7). *)
let map_context_statements env f body_terms =
  let rec go acc = function
    | [] -> List.rev acc
    | stmt :: rest ->
        let declared = env.declared in
        let result = f ~last:(rest = []) stmt in
        let rest =
          if env.declared = declared then rest
          else List.map (Raw_syntax.add_scope (Scope_set.singleton (fresh_scope ()))) rest
        in
        go (result :: acc) rest
  in
  go [] (split_statements body_terms)

(* The first statement of a definition context, and the terms after it. *)
let take_statement terms =
  let rec go acc = function
    | [] -> (List.rev acc, [])
    | term :: rest when is_separator term || token_kind Comma term -> (List.rev acc, term :: rest)
    | term :: rest -> go (term :: acc) rest
  in
  go [] (drop_separators terms)

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
