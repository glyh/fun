(** What a macro expands to, decided syntactically from its definition: [Decl]
    for [: Decl]; [TypedExpr] when its signature promises a type ([macro_signature]),
    so its call is deferred to the elaborator, which solves its type binders and
    checks its arguments and output; [Expr] otherwise. *)
module MacroKind = struct
  type t = Expr | TypedExpr | Decl
  let default = Expr
  let to_string = function Expr | TypedExpr -> "Expr" | Decl -> "Decl"
  let has_type_binding = function TypedExpr -> true | Expr | Decl -> false
  (* The position a macro of this kind may be used in. *)
  let position = function Expr | TypedExpr -> Expr | Decl -> Decl
end

(** A macro's annotation as written: [: Decl] or [: Expr(T)]. The [T] is kept as
    the binding's [output] and elaborated as part of the macro's signature. *)
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
  | MacroBinding of { name : id; value : t; public : bool; kind : MacroAnnotation.t option; output : t option }
      (** [output] is the [T] of a [: Expr(T)] annotation - the type its output
          promises; [None] for [: Expr(_)], [: Decl] or none. *)
  | MacroCallBinding of { f : t; args : capture list }
  | PatternSynBinding of { name : id; params : id list; rhs : pat; public : bool }
  | FieldBinding of { name : string; type_ : t }
      (** A struct field [name : type_]. Its type sees the items written before
          it; a method sees every field. Only in a [Struct]. *)
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
  (* The order group the operator or form belongs to; [None] is weaker than
     every grouped one. *)
  order : order option;
  meaning : role_meaning;
  declared_at : Source_span.t;
  (* The unit an imported role came from. *)
  from_unit : string option;
}

and assoc = LeftAssoc | RightAssoc | NonAssoc  (** [assoc(none)]: members do not chain *)

(** An order group (brackets-decide-grouping): precedence is relative. A group
    is its declaration - [group] is unique - and carries the groups its
    declaration names, so two groups compare wherever their roles travel. *)
and order = {
  group : string;
  group_name : string;
  group_assoc : assoc;
  weakest : bool;
      (** [weakest]: weaker than every group that states no relation to it *)
  stronger_than : order list;
  weaker_than : order list;
}

and role_meaning =
  | ApplyValue  (** fixity only: the use calls the value of its name *)
  | AssignRef  (** [<-] *)
  | CallMacro  (** the use applies the procedural macro of its name *)
  | Rules of { rules_kind : MacroAnnotation.t; rules : rule list }
      (** a syntax form: a macro whose rules match tokens and fill a quote (M9) *)
  | OrderGroup  (** an order group's name, [order] its declaration *)

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
  | Struct of { bindings : struct_binding list }
      (** A struct's items in source order: a [FieldBinding] is one of its fields. *)
  | Module of { bindings : struct_binding list }
  | Sig of { bindings : struct_binding list }
      (** [sig { x : I64 }]: a signature value, its own kind of value (not a
          module); each item is a [LetBinding] whose value is a type. *)
  | Import of { path : string; scope : Scope_set.t }
      (** [import "path"]. [scope] is where it is written, the scope set the
          [import] keyword carries: the roles visible there are what an open of
          it must not supply (M7). *)
  | Open of t * t * string
      (** [open m; body]. The string labels this open, so an open choice can
          name it: [""] until expansion assigns one - ["unit:p"] for an open of
          [import "p"], ["open:n"] otherwise. *)
  | OpenChoice of { name : id; opens : string list; fallback : string option }
      (** Produced only by expansion: a bare name some open may supply. [opens]
          are the candidate opens' labels, innermost first; [fallback] is the
          resolved name of the binder they shadow, if any. With no opens and no
          binder it names only the base context. *)
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
  | Elaborated of { arg : int; form : t }
      (** A typed macro argument placed by the macro's output where the call's
          elaborator already elaborated it: [arg] names that result, [form] is
          the argument as expanded at the call. Internal - written only after the
          macro returns - so expansion leaves it alone, and a macro handed it
          (a nested call in the output) receives [form]: plain syntax, which
          elaborates again. *)
  | Quote of { template : t; holes : (string * t) list }
      (** [quote(…)]: syntax written literally in a macro body. Each hole [$x]
          stands in [template] as an id spelled ["$x"] - [$] cannot begin a
          source identifier - and in [holes] as the ordinary reference [x]. *)
  | QuoteDecls of { items : struct_binding list; holes : (string * t) list }
      (** [quote { … }]: declarations written literally, holes as in [Quote]. *)
  | MacroDef of { name : id; value : t; body : t; kind : MacroAnnotation.t option; output : t option }
  | SyntaxDef of { name : id; role : role; body : t }
      (** A [SyntaxBinding] scoped over the rest of a block. *)
  | MacroCall of t * capture list
      (** A macro call whose arguments were read as its parameters' kinds, or
          one compiler-derived; each argument is a capture, as a syntax
          form's hole takes (M9). *)
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

(* A declaration's label: the name it exports and a member is reached by
   ([M.x]), as a struct field's label. A binder's resolved name is minted as its
   label followed by [#n] ([#] begins a comment, so no label contains one); the
   label is only ever read off a binder the declaration owns, never used to find
   one. *)
let label (name : string) = match String.index_opt name '#' with Some i -> String.sub name 0 i | None -> name

(* A resolved name is written with [#], which no token can contain, so source
   never spells one: only the expander mints them. *)
let is_resolved_name name = String.contains name '#'

(* Reflection is the one other way a name reaches the expander, so a reflected
   id's opaque scopes carry the resolved name it was minted with ([Atom.Scopes]),
   and a reflected resolved name is accepted only under that certificate: a macro
   that did not receive an id cannot make one that reaches its binder (M11, M12). *)
let certificate name = if is_resolved_name name then Some name else None
let certified name cert = (not (is_resolved_name name)) || cert = Some name

(* The label a path's last segment names: a member label, or the label of the
   binder its head resolved to (a constructor, an effect). *)
let path_last (p : path) = label (snd (path_split p))

(* A form the compiler writes itself, with no source position. *)
let synth kind = { kind; span = Source_span.synthetic }

(* A fixity-only role attaches to the value of its name (M7). *)
let attaches (role : role) = role.meaning = ApplyValue

type relation = Stronger | Weaker | Same | Unrelated

(** How group [a] relates to group [b]: the transitive closure of the relations
    the two declarations, and the declarations they name, state. A stated relation
    wins; otherwise a [weakest] group is weaker than one that is not. *)
let order_relation (a : order) (b : order) : relation =
  let rec collect seen (o : order) =
    if List.exists (fun (s : order) -> String.equal s.group o.group) seen then seen
    else List.fold_left collect (o :: seen) (o.stronger_than @ o.weaker_than)
  in
  let nodes = collect (collect [] a) b in
  (* The groups [x] is declared directly stronger than, by either side. *)
  let below (x : string) =
    List.concat_map
      (fun (n : order) ->
        if String.equal n.group x then List.map (fun (o : order) -> o.group) n.stronger_than
        else if List.exists (fun (o : order) -> String.equal o.group x) n.weaker_than then [ n.group ]
        else [])
      nodes
  in
  let rec reaches seen x target =
    String.equal x target
    || (not (List.mem x seen) && List.exists (fun y -> reaches (x :: seen) y target) (below x))
  in
  if String.equal a.group b.group then Same
  else if reaches [] a.group b.group then Stronger
  else if reaches [] b.group a.group then Weaker
  else if a.weakest && not b.weakest then Weaker
  else if b.weakest && not a.weakest then Stronger
  else Unrelated

(* An id a hole is written as in quoted syntax: [$x] ([$] cannot begin a source
   identifier). *)
let hole_name (name : string) =
  if String.length name > 1 && name.[0] = '$' then Some (String.sub name 1 (String.length name - 1)) else None

(* A kind is written as its reflection type, in a hole [$(x : Id)] and a macro
   parameter [(x : Id)] alike. *)
let hole_kind_of_name = function
  | "Expr" -> Some HoleExpr
  | "Block" -> Some HoleBlock
  | "Id" -> Some HoleId
  | "Decl" -> Some HoleDecl
  | "Pattern" -> Some HolePattern
  | _ -> None

let hole_kind_name = function
  | HoleExpr -> "Expr" | HoleBlock -> "Block" | HoleId -> "Id" | HoleDecl -> "Decl" | HolePattern -> "Pattern"

(* The id an identifier or operator token names. *)
let token_id (tok : Token_tree.token) =
  let name = match tok.kind with Ident s | Operator s -> s | _ -> "" in
  { name; span = tok.span; scope = tok.scope }

(** A macro's parameter kinds (M9): each explicit parameter annotated with a
    kind - [(n : Id)] - takes that kind, any other an [Expr]. The kind is the
    parameter's type, the reflection type in the [Syntax] module, found from
    the annotation's own scopes; a [Block] is an [Expr] (a [RawBlock]) and a
    [Decl] the [Decls] its brace group of items is, as [quote { … }]. Returns
    the kinds and the value with each kind annotation made that type. *)
let macro_params (value : t) : hole_kind list * t =
  let kind_type (p : param) =
    match p.type_ with
    | Some { kind = Ap ({ kind = Var ({ name = "Expr"; _ } as written); span }, Explicitness.Explicit, _); span = ty_span } ->
        (* [(x : Expr(T))]: an [Expr] whose type the macro's signature promises. *)
        let syntax = { written with name = Compiler_names.Module_name.syntax } in
        (HoleExpr, Some { kind = FieldAccess ({ kind = Var syntax; span }, "Expr"); span = ty_span })
    | Some ({ kind = Var ({ name; _ } as written); span } as ty) -> (
        match hole_kind_of_name name with
        | Some kind ->
            let type_name = match kind with HoleBlock -> "Expr" | HoleDecl -> "Decls" | k -> hole_kind_name k in
            let syntax = { written with name = Compiler_names.Module_name.syntax } in
            (kind, Some { ty with kind = FieldAccess ({ kind = Var syntax; span }, type_name) })
        | None -> (HoleExpr, p.type_))
    | _ -> (HoleExpr, p.type_)
  in
  let rec go (stx : t) =
    match stx.kind with
    | Lam (({ explicitness = Explicitness.Implicit; _ } as p), body) ->
        let kinds, body = go body in
        (kinds, { stx with kind = Lam (p, body) })
    | Lam (p, body) ->
        let kind, type_ = kind_type p in
        let kinds, body = go body in
        (kind :: kinds, { stx with kind = Lam ({ p with type_ }, body) })
    | _ -> ([], stx)
  in
  go value

(* The [T] of a parameter annotated [(x : Expr(T))]. *)
let typed_expr_param (p : param) =
  match p.type_ with
  | Some { kind = Ap ({ kind = Var { name = "Expr"; _ }; _ }, Explicitness.Explicit, t); _ } -> Some t
  | _ -> None

(** A macro's signature (macro-annotation-constraints-mean-nothing): the type the
    macro has as a function over types, written as a pi type so it elaborates
    where the macro is defined - its type binders, then the [T] of each
    [(x : Expr(T))] parameter, then the [T] its [: Expr(T)] output promises. An
    output that promises nothing is one more type binder, solved at the call.
    [binders] are the macro's own type binders, as written; [params] names each
    explicit parameter and says whether the signature has a domain for it. [None] when
    the macro promises no type: it runs during expansion. *)
type macro_signature = { signature : t; binders : string list; params : (string * bool) list }

let macro_signature ~(output : t option) (value : t) : macro_signature option =
  let rec params (stx : t) = match stx.kind with Lam (p, body) -> p :: params body | _ -> [] in
  let params = params value in
  let binders = List.filter (fun (p : param) -> p.explicitness = Explicitness.Implicit) params in
  let explicit = List.filter (fun (p : param) -> p.explicitness = Explicitness.Explicit) params in
  let params = List.map (fun (p : param) -> (p.name.name, Option.is_some (typed_expr_param p))) explicit in
  if binders = [] && output = None && not (List.exists snd params) then None
  else
    let span = value.span in
    let type_at (id : id) = { kind = Var { id with name = Compiler_names.Type_name.type_ }; span = id.span } in
    let pi explicitness name domain codomain = { kind = Arrow (explicitness, name, domain, None, codomain); span } in
    let result, result_binder =
      match output with
      | Some t -> (t, [])
      (* [$] cannot begin a source identifier, so no annotation can mention it. *)
      | None -> let r = fresh_id ~span "$output" in ({ kind = Var r; span }, [ r ])
    in
    let codomain =
      List.fold_right
        (fun p acc -> match typed_expr_param p with Some t -> pi Explicitness.Explicit None t acc | None -> acc)
        explicit result
    in
    let codomain = List.fold_right (fun r acc -> pi Explicitness.Implicit (Some r) (type_at r) acc) result_binder codomain in
    let pis =
      List.fold_right (fun (p : param) acc -> pi Explicitness.Implicit (Some p.name) (type_at p.name) acc) binders codomain
    in
    (* Annotated as a type, so a promised [T] that is not one is an error. *)
    let signature = { kind = Annotated { inner = pis; typ = type_at (fresh_id ~span "Type") }; span } in
    Some { signature; binders = List.map (fun (p : param) -> p.name.name) binders; params }

(** The kind of a macro with annotation [ann] and value [value]: [TypedExpr]
    when its signature promises a type, so its call waits for the elaborator. *)
let macro_kind ~output (ann : MacroAnnotation.t option) (value : t) : MacroKind.t =
  match ann with
  | Some MacroAnnotation.Decl -> MacroKind.Decl
  | _ when Option.is_some (macro_signature ~output value) -> MacroKind.TypedExpr
  | _ -> MacroKind.Expr

(** A macro's value as it is compiled, and its signature. A [: Decl] macro's
    [output] is the type its body returns - [Syntax.Decl] for one declaration,
    [List(Syntax.Decl)] for any number - so its body is checked against it where
    the macro is defined; it has no signature, and runs during expansion. *)
let macro_compiled ~output (ann : MacroAnnotation.t option) (value : t) : t * macro_signature option =
  match ann, output with
  | Some MacroAnnotation.Decl, Some typ ->
      let rec annotate (stx : t) =
        match stx.kind with
        | Lam (p, body) -> { stx with kind = Lam (p, annotate body) }
        | _ -> { stx with kind = Annotated { inner = stx; typ } }
      in
      (annotate value, None)
  | Some MacroAnnotation.Decl, None -> (value, None)
  | _ -> (value, macro_signature ~output value)

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

(* A struct whose items are exactly these fields: a record declaration's body. *)
let struct_of_fields (fields : (string * t) list) =
  Struct { bindings = List.map (fun (name, type_) -> FieldBinding { name; type_ }) fields }
