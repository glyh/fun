(** What a macro expands to, decided syntactically from its definition: [Decl]
    for [: Decl]; [TypedExpr] when it binds a type parameter [macro m[A](..)],
    so its call is deferred to the elaborator, which hands it the expected type;
    [Expr] otherwise. *)
module MacroKind = struct
  type t = Expr | TypedExpr | Decl
  let default = Expr
  let to_string = function Expr | TypedExpr -> "Expr" | Decl -> "Decl"
  let has_type_binding = function TypedExpr -> true | Expr | Decl -> false
  (* The position a macro of this kind may be used in. *)
  let position = function Expr | TypedExpr -> Expr | Decl -> Decl
end

(** A macro's annotation as written: [: Decl] or [: Expr(T)]. The names in [T]
    only refer; the enforester makes them a reference in the macro's body, so
    they resolve by scope like any other. *)
module MacroAnnotation = struct
  type t = Expr | Decl
  let default = Expr
end

type id = {
  name : string;
  span : Source_span.t;
  scope : Scope_set.t;
}

type param = {
  name : id;
  type_ : t option;
  trait_bounds : path list;
  explicitness : Explicitness.t;
}

and effect_op = { name : string; input : t; output : t }

(** A dotted name, [M.N.x]: its head is a bare name - an id, resolved by scope
    set and renamed like any other occurrence - and the rest are member labels,
    resolved by their container. A single name is a path with no members.
    [head_choice] is the head's open choice (see [OpenChoice]), set only by
    expansion; [None] means the head is a binder's resolved name, or unexpanded. *)
and path = { head : id; members : string list; head_choice : open_choice option }

and open_choice = { opens : string list; fallback : string option }

and type_decl = { name : id; params : id list; ctors : (id * t list) list }

and effect_row = { effects : t list; tail : t option }

and struct_binding =
  | LetBinding of { name : id; value : t; public : bool; recursive : bool }
  | MethodBinding of { name : id; params : param list; body : t; public : bool }
  | TypeBinding of { members : type_decl list; public : bool }
      (** [type A = … and B = …]: one binding per chain, its members mutually
          recursive; a single declaration is the one-member chain. *)
  | RecordTypeBinding of {
      name : id;
      params : id list;
      fields : (string * t) list;
      public : bool;
    }
  | EffectBinding of {
      name : id;
      params : id list;
      ops : effect_op list;
      public : bool;
    }
  | TraitBinding of {
      name : id;
      params : id list;
      fields : (string * t) list;
      public : bool;
    }
  | ImplBinding of {
      name : id option;   (* [impl NAME : Trait(Args) = …] - see impl-visibility *)
      trait : path;
      args : t list;
      fields : (string * t) list;
      public : bool;
    }
  | MacroBinding of { name : id; value : t; public : bool; kind : MacroAnnotation.t option }
  | MacroCallBinding of { f : t; args : t list }
  | PatternSynBinding of { name : id; params : id list; rhs : pat; public : bool }
  | OpenBinding of t * string
      (** [open <module-expr>] at module/struct top level — the binding-list
          counterpart of the expression-level [Open]. Scopes over the subsequent
          bindings only. The string is the open's label (see [Open]). *)
  | HoleBinding of id
      (** A declaration hole [$d] in quoted items ([quote { … }], M10): a
          declaration, filled when the quote is evaluated. *)
  | SyntaxBinding of { name : id; role : role; public : bool }
      (** A syntax form or fixity declaration, as the binder it is: expansion
          registers the role, so the forms after it are read with it and a
          value binder of the same name visible with it is an error (M7). A
          fixity-only role ([ApplyValue]) attaches to the value of its name
          visible where it is declared instead of binding a new one. *)
  | Items of Token_tree.t list
      (** Declarations not read yet: a definition context's remaining items,
          enforested one form at a time as expansion reaches them (M9). *)
  | InstantiateBinding of instantiation
      (** A declaration syntax form's use, filled and expanded like a macro
          application (M9). *)

(** A syntactic role (M7): what a binder means to the enforester. *)
and role = {
  fixity : operator_fixity;
  precedence : int;
  assoc : assoc;
  meaning : role_meaning;
  declared_at : Source_span.t;
  (* The unit an imported role came from. *)
  from_unit : string option;
}

and assoc = LeftAssoc | RightAssoc

and role_meaning =
  | ApplyValue  (** fixity only: the use calls the value of its name *)
  | AssignRef  (** [<-] *)
  | CallMacro  (** the use applies the procedural macro of its name *)
  | Rules of { rules_kind : MacroAnnotation.t; rules : rule list }
      (** a syntax form: a macro whose rules match tokens and fill a quote (M9) *)

(** One rule: the tokens a use consumes and what each hole captures, and the
    replacement - quoted syntax parsed where the rule is written. *)
and rule = { pattern : rule_part list; replacement : rule_replacement; rule_span : Source_span.t }

and rule_part =
  | PartToken of Token_tree.t
  | PartGroup of Token_tree.delimiter * rule_part list * Source_span.t
  | PartHole of { hole : string; hole_kind : hole_kind; hole_span : Source_span.t }

(** What a hole captures: the reflection types (M10). *)
and hole_kind = HoleExpr | HoleBlock | HoleId | HoleDecl | HolePattern

and rule_replacement = ReplaceExpr of t | ReplaceDecls of struct_binding list

(** A syntax form's use: the rule that matched and what its holes captured. *)
and instantiation = {
  form : id;
  rule : rule;
  captures : (string * capture) list;
  (* The unit the form was imported from: ids its replacement introduces mean
     that unit's names. *)
  from_unit : string option;
}

and capture =
  | CapExpr of t
  | CapBlock of Token_tree.t list
  | CapId of Token_tree.token
  | CapPattern of pat
  | CapDecls of struct_binding list

and t = {
  kind : kind;
  span : Source_span.t;
}

and kind =
  | Atom of Atom.t
  | Var of id
  | Self
  | SelfType
  | Ap of t * Explicitness.t * t
  | Lam of param * t
  | Let of { name : id; type_ : t option; value : t; body : t; recursive : bool }
  | Annotated of { inner : t; typ : t }
  | Prod of t list
  | ProdTy of t list
  | Arrow of Explicitness.t * id option * t * effect_row option * t
  | FieldAccess of t * string
  | Proj of t * int
  | RecordConstruct of { typ : t; fields : (string * t) list }
  | Struct of {
      con_fields : (string * t) list;
      bindings : struct_binding list;
    }
  | Module of { bindings : struct_binding list }
  | Import of string
  | Open of t * t * string
      (** [open m; body]. The string labels this open, so an open choice can
          name it: [""] until expansion assigns one - ["unit:p"] for an open of
          [import "p"], ["open:n"] otherwise. *)
  | OpenChoice of { name : id; opens : string list; fallback : string option }
      (** Produced only by expansion: a bare name some open may supply. [opens]
          are the candidate opens' labels, innermost first; [fallback] is the
          resolved name of the binder they shadow, if any. With no opens and no
          binder it names only the base context. *)
  | RecordTypeDef of {
      name : id;
      params : id list;
      fields : (string * t) list;
      body : t;
    }
  | TypeDef of {
      name : id;
      params : id list;
      ctors : (id * t list) list;
      body : t;
    }
  | EffectDef of {
      name : id;
      params : id list;
      ops : effect_op list;
      body : t;
    }
  | TraitDef of {
      name : id;
      params : id list;
      fields : (string * t) list;
      body : t;
    }
  | ImplDef of {
      name : id option;
      trait : path;
      args : t list;
      fields : (string * t) list;
      body : t;
    }
  | Perform of { op : path; arg : t }
  | Resume of t
  | RefNew of t
  | RefGet of t
  | RefSet of t * t
  | Match of t * match_branch list
  | Block of Token_tree.t list
      (** A [{ … }] body not read yet: its statements are enforested one form
          at a time as expansion reaches them (M9). *)
  | Instantiate of instantiation
  | Stx of t  (* opaque syntax wrapper *)
  | Quote of { template : t; holes : (string * t) list }
      (** [quote(…)]: syntax written literally in a macro body. Each hole [$x]
          stands in [template] as an id spelled ["$x"] - [$] cannot begin a
          source identifier - and in [holes] as the ordinary reference [x]. *)
  | QuoteDecls of { items : struct_binding list; holes : (string * t) list }
      (** [quote { … }]: declarations written literally, holes as in [Quote]. *)
  | MacroDef of { name : id; value : t; body : t; kind : MacroAnnotation.t option }
  | SyntaxDef of { name : id; role : role; body : t }
      (** A [SyntaxBinding] scoped over the rest of a block. *)
  | MacroCall of t * t list
  | SyntaxOperatorUse of {
      operator : id;
      fixity : operator_fixity;
      operands : t list;
      declaration_span : Source_span.t;
      use_span : Source_span.t;
      (* The unit that supplied this operator, so its macro body is looked up in
         the same declaration the fixity came from. [None] when declared here. *)
      unit : string option;
    }

and operator_fixity = PrefixOp | InfixOp

and match_branch =
  | ValueBranch of pat * t
  | EffectBranch of {
      op : path;
      arg_pat : pat;
      body : t;
    }

and pat =
  | PatCon of path * pat list
  | PatRecord of { typ : path; fields : (string * pat option) list; partial : bool }
  | PatStructType of { fields : (string * pat) list; partial : bool }
  | PatOr of pat * pat
  | PatProd of pat list
  | PatAtom of Atom.t
  | PatType of Atom_ty.t
  | PatWild
  | PatBind of id

let fresh_id ?(span = Source_span.synthetic) ?(scope = Scope_set.empty) name =
  { name; span; scope }

let path_of_id head = { head; members = []; head_choice = None }

(* [(M.N, x)] for [M.N.x]: the prefix and the last segment, as the string lists
   everything after lowering speaks. *)
let path_split (p : path) : string list * string =
  match List.rev (p.head.name :: p.members) with
  | last :: rev_prefix -> (List.rev rev_prefix, last)
  | [] -> assert false

let path_of_segments ?span = function
  | head :: members -> { head = fresh_id ?span head; members; head_choice = None }
  | [] -> invalid_arg "path_of_segments: empty path"

let path_last (p : path) = snd (path_split p)

(** The kind of a macro with annotation [ann] and value [value] (its parameter
    lambdas): arity is syntactic, so a leading implicit parameter is its type
    binder. *)
let macro_kind (ann : MacroAnnotation.t option) (value : t) : MacroKind.t =
  match ann, value.kind with
  | Some MacroAnnotation.Decl, _ -> MacroKind.Decl
  | _, Lam ({ explicitness = Explicitness.Implicit; _ }, _) -> MacroKind.TypedExpr
  | _ -> MacroKind.Expr

(* A form the compiler writes itself, with no source position. *)
let synth kind = { kind; span = Source_span.synthetic }

(* A fixity-only role attaches to the value of its name (M7). *)
let attaches (role : role) = role.meaning = ApplyValue

(* An id a hole is written as in quoted syntax: [$x] ([$] cannot begin a source
   identifier). *)
let hole_name (name : string) =
  if String.length name > 1 && name.[0] = '$' then Some (String.sub name 1 (String.length name - 1)) else None

let names (ids : id list) = List.map (fun (i : id) -> i.name) ids

(* The name a bare-name form was written with, whether expansion resolved it to
   a binder or left it an open choice. Only for sugar matched as written - the
   [+] of a trait bound [A : Eq + Show] - never for a lookup. *)
let written_name (stx : t) =
  match stx.kind with
  | Var id -> Some id.name
  | OpenChoice { name; _ } -> Some name.name
  | _ -> None

(* A form that names an entry - [x], an open choice, [M.x] - as a path, so it
   resolves like any other path head (M12). *)
let rec path_of_form (stx : t) : path option =
  match stx.kind with
  | Var id -> Some (path_of_id id)
  | OpenChoice { name; opens; fallback } -> Some { head = name; members = []; head_choice = Some { opens; fallback } }
  | FieldAccess (e, member) -> Option.map (fun p -> { p with members = p.members @ [ member ] }) (path_of_form e)
  | _ -> None
