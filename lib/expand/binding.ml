(** Whether a resolved binding names a value or a (procedural) macro.
    Under the unified namespace, name resolution returns exactly one
    binding and its [kind] decides expand-vs-call at an application head. *)
type binding_kind = Value | Macro

(* Operator fixity/precedence carried as an optional attribute on a binding.
   An operator is a binding that also carries [operator_info]; [kind]
   (Value/Macro) and operator-ness are orthogonal — e.g. [(+)] is a callable
   value that is also an infix operator, and [&&] is a macro that is also an
   infix operator. Lifted from the former [Operator_env] so that fixity lives
   in the one binding table rather than a parallel structure. *)
type associativity = Left | Right
type operator_fixity = Prefix | Infix
type operator_expansion =
  | BuiltinApply
  | BuiltinRefSet
  | MacroOp
  | Template of Syntax_template.t

type operator_info = {
  symbol : string;
  fixity : operator_fixity;
  precedence : int;
  associativity : associativity;
  syntax_class : Syntax_class.t;
  expansion : operator_expansion;
  declaration_span : Source_span.t;
  (* The compilation unit this operator was imported from, if it was imported.
     An operator's fixity and its macro body must come from the SAME
     declaration: [find_operator] resolves fixity last-wins, so a body found
     independently - by scanning units for the written name - could belong to a
     different unit than the precedence the parse already committed to. Carrying
     the unit here means there is nothing left to resolve separately. *)
  unit : string option;
}

type binding_info = {
  scope : Scope_set.t;
  resolved_name : string;
  kind : binding_kind;
  operator : operator_info option;
}

type t = (string, binding_info list) Hashtbl.t

let create () : t = Hashtbl.create 32

let copy (tbl : t) : t =
  let new_tbl = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun k v -> Hashtbl.add new_tbl k v) tbl;
  new_tbl

let extend (tbl : t) ~name ~scope ~kind ~resolved_name =
  let info = { scope; resolved_name; kind; operator = None } in
  let existing = try Hashtbl.find tbl name with Not_found -> [] in
  Hashtbl.replace tbl name (info :: existing)

let has_name (tbl : t) name = Hashtbl.mem tbl name

(** Add a binding. Bindings for the same written name are stacked;
    during resolution the one with the largest subset-scope wins. *)

(** Resolve an identifier occurrence: find the binding with the same
    written name whose scope set is a subset of the occurrence scope
    set, choosing the one with the largest binding scope. *)
let incompatible_best name a b =
  failwith (Printf.sprintf "ambiguous binding for %s: scopes %s and %s"
              name
              (Format.asprintf "%a" Scope_set.pp a.scope)
              (Format.asprintf "%a" Scope_set.pp b.scope))

let more_specific name a b =
  if Scope_set.equal a.scope b.scope then b
  else if Scope_set.subset a.scope b.scope then b
  else if Scope_set.subset b.scope a.scope then a
  else incompatible_best name a b

let resolve (tbl : t) (id : Syntax.id) : binding_info option =
  let candidates = try Hashtbl.find tbl id.name with Not_found -> [] in
  match candidates with
  | [] -> None
  | _ ->
    let matches = List.filter (fun info -> Scope_set.subset info.scope id.scope) candidates in
    match matches with
    | [] -> None
    | _ ->
      Some (List.fold_left (more_specific id.name) (List.hd matches) (List.tl matches))

(* --- Operators as bindings ---------------------------------------------- *)

(* A syntactic role is resolved like any binder (M7): among the roles of that
   name, fixity and class whose scope set is a subset of the occurrence's, the
   one with the largest wins, and two incomparable ones are ambiguous. Roles
   with equal scope sets - two imports of one operator, or two declarations in
   one statement - resolve to the one added last; [add_operator] prepends, so
   that is the first candidate. *)
let find_operator (tbl : t) ~fixity ~syntax_class ~(scope : Scope_set.t) name : operator_info option =
  let candidates =
    Option.value ~default:[] (Hashtbl.find_opt tbl name)
    |> List.filter (fun info ->
           Scope_set.subset info.scope scope
           && match info.operator with
              | Some op -> op.fixity = fixity && op.syntax_class = syntax_class
              | None -> false)
  in
  let best a b =
    if Scope_set.subset b.scope a.scope then a
    else if Scope_set.subset a.scope b.scope then b
    else incompatible_best name a b
  in
  match candidates with
  | [] -> None
  | first :: rest -> (List.fold_left best first rest).operator

(* Incremented whenever a role is added: the enforester mints a statement's
   scope only when the statement declared one. *)
let role_generation = ref 0

let add_operator ?(scope = Scope_set.empty) (tbl : t) (op : operator_info) =
  incr role_generation;
  let info = { scope; resolved_name = op.symbol; kind = Value; operator = Some op } in
  let existing = try Hashtbl.find tbl op.symbol with Not_found -> [] in
  Hashtbl.replace tbl op.symbol (info :: existing)

let make_operator ?(syntax_class = Syntax_class.Expr)
    ?(declaration_span = Source_span.synthetic) ?unit ~symbol ~fixity ~precedence
    ~associativity ~expansion () =
  { symbol; fixity; precedence; associativity; syntax_class; expansion; declaration_span; unit }

(* Stamp exports with the unit they were loaded from, at the import site - the
   only place that knows the written path. *)
let from_unit path (ops : operator_info list) =
  List.map
    (fun op ->
      let expansion =
        match op.expansion with
        | Template t -> Template { t with Syntax_template.unit = Some path }
        | e -> e
      in
      { op with unit = Some path; expansion })
    ops

let template_infix ?(declaration_span = Source_span.synthetic) symbol template precedence associativity =
  make_operator ~declaration_span ~symbol ~fixity:Infix ~precedence ~associativity
    ~expansion:(Template template) ()

let template_prefix ?(declaration_span = Source_span.synthetic) symbol template precedence =
  make_operator ~declaration_span ~symbol ~fixity:Prefix ~precedence
    ~associativity:Left ~expansion:(Template template) ()

let macro_infix ?(declaration_span = Source_span.synthetic) symbol precedence associativity =
  make_operator ~declaration_span ~symbol ~fixity:Infix ~precedence ~associativity
    ~expansion:MacroOp ()

let apply_operator_exports (tbl : t) (ops : operator_info list) =
  List.iter (fun op -> add_operator tbl op) ops

let same_operator_key a b =
  String.equal a.symbol b.symbol && a.fixity = b.fixity && a.syntax_class = b.syntax_class

let fixity_name = function Prefix -> "prefix" | Infix -> "infix"

let duplicate_operator_exports_message (exports : operator_info list) =
  let rec go seen = function
    | [] -> None
    | op :: rest -> (
        match List.find_opt (same_operator_key op) seen with
        | Some previous ->
            Some
              (Printf.sprintf
                 "ambiguous syntax extension candidates for %s %s operator %S: declarations at %s and %s"
                 (fixity_name op.fixity)
                 (Syntax_class.to_string op.syntax_class)
                 op.symbol
                 (Format.asprintf "%a" Source_span.pp previous.declaration_span)
                 (Format.asprintf "%a" Source_span.pp op.declaration_span))
        | None -> go (op :: seen) rest)
  in
  go [] exports
