type ix = int (* de Bruijn index: distance to binder *)
type lvl = int (* de Bruijn level: distance from outermost scope *)
type meta_id = int
type nominal_id = int (* unique identity for each nominal type definition *)
type effect_id = int (* unique identity for each effect family definition *)

(* Bound = introduced by lambda/pi, Defined = introduced by let.
   InsertedMeta uses this to know which scope vars to abstract over. *)
type bd = Bound | Defined

type struct_field_kind = Field | Public | Private | Method | PrivateMethod

type explicitness = Implicit | Explicit

(** What an [open] binds, in order: decided by the opened module's type (its
    public entries), never by its value - a field by name, an impl by its
    position among the public impls. *)
type open_member = OpenField of string | OpenImpl of int

type effect_row = { effects : term list; tail : term option }

and term =
  | Var of ix
  | Lam of term
  | Ap of term * explicitness * term
  | Let of term * term * term   (* let _ : A = def in body *)
  | Pi of {
      explicitness : explicitness;
      domain : term;
      effects : effect_row;
      codomain : term;
    }
      (* (x : A) -> B or {x : A} -> B, with latent effects *)
  | U (* Type : Type *)
  | EffectRowTy
  | EffectRowLit of effect_row
  | Atom of Atom.t
  | AtomTy of Atom_ty.t
  | Prod of term list (* value-level tuple: (a, b) has type ProdTy [A, B] *)
  | ProdTy of term list (* type-level tuple: (A, B) has type U *)
  | Fix of { members : fix_member list; index : int }
      (* the [index]th member of a group of mutually recursive definitions
         ([rec f = … and g = …]; a single [rec] is a group of one). Every body
         sits under one entry per member, the first member outermost. *)
  | Proj of term * int             (* positional tuple projection: e.0 *)
  | Dot of term * string           (* named member/field access: e.field *)
  | RecordConstruct of { typ : term; fields : (string * term) list }
  | Module of { bindings : struct_binding_term list; signature : bool }
      (** [signature]: a [sig { … }] value, whose members are types; it evaluates to
          a [VModule] with [partial = true]. A signature is its own kind of value,
          never a module. *)
  | Sig of term
      (** A signature: a telescope over the module it describes. The body is a
          [Module { signature = true }] under one binder, the described module
          ([self]), so a member's type reads an earlier member as [self.T]. It
          evaluates to a [VSig] closure; instantiating it with a module (a
          parameter, or the argument checked against it) gives the member types. *)
  | Struct of {
      con_fields : (string * term) list;
      bindings : struct_binding_term list;
      partial : bool;
    }
  | Open of term * open_member list * term  (* open S in body — the evaluator pushes each member of S *)
  | Prim of string (* evaluated as VNeutral with HPrim head — no VPrim needed *)
  | NomRef of { id : nominal_id; name : string; num_params : int; captures : term list; params : term list }
      (** Applied nominal type reference. A nominal's identity is its declaration
          ([id], whose constructors [nominal_decls] holds) and the values of the
          declaration's own free variables ([captures]) - applicative (E11), so
          the same declaration over convertible captures is the same type. [eval]
          builds the [VNominal] directly, never looking the declaration up in an
          environment ([name] is for display), and applies [params]. *)
  | EffectRef of string * term list
      (** Applied effect family reference. [eval] scans the environment for a
          [VEffect] template with this name, evaluates the param terms, and
          returns [VEffect] with those params. *)
  | TraitRef of { trait_id : int; trait_name : string }
      (** Internal trait declaration reference. Quotation uses this to preserve
          trait identity without string marker atoms. *)
  | TraitDictTy of {
      trait_id : int;
      trait_name : string;
      args : term list;
      fields : (string * term) list;
    }
      (** Internal trait dictionary type reference. Used by quotation to preserve
          trait dictionary identity without encoding marker fields in structs. *)
  | RecOcc of { id : int; name : string; captures : term list; args : term list }
      (** A recursive occurrence: a [rec] struct type's reference to itself (or to
          a member of its [rec] group), by the identity its binding minted and the
          values of what its enclosing scope names ([captures], E11), applied to
          its parameters. Unfolded on demand ([finished_records]). *)
  | Ctor of {
      name : string;
      spine : term list;           (* type args then payload args *)
      nominal_name : string;       (* the template's name, for display *)
      nominal_spine : term list;   (* type args for the nominal's params *)
      nominal : term;              (* the unapplied nominal (a [NomRef]) *)
    }
      (** Constructor value term. [eval] evaluates spine terms to values,
          applies [nominal_spine] as the params of the template [nominal]
          ([nominal_name] is for display), and constructs [VCon]. Built by
          constructor lambda chains, and by [quote] for a [VCon]: a constructor
          value carries its nominal, so it is never found again by spelling. *)
  | Meta of meta_id
      (** Residual metavariable in quoted output. Created when [quote] hits an
          unsolved [VFlex]. Used as the head of stuck neutrals
          ([HMeta id]) inside [quote_neutral]. Never constructed directly by the
          elaborator — use [InsertedMeta] instead. *)
  | InsertedMeta of meta_id * bd list
      (** Fresh metavariable at creation time. The [bd list] records, for each
          variable currently in scope, whether it was introduced by [Lam]/[Pi]
          ([Bound]) or by [let] ([Defined]). During evaluation,
          [eval_inserted_meta] applies the meta to every [Bound] variable (skipping
          [Defined] ones), so the solver only abstracts over variables with
          unknown values. This keeps unification solutions minimal and avoids
          spurious occurs-check failures. *)
  | Match of term * match_branch list
      (** Pattern matching. Scrutinee + branches. Value branches bind variables
          from the pattern — de Bruijn indices in the body count from the
          innermost pattern binding outward. Effect branches bind operation
          argument pattern variables plus an innermost continuation. *)
  | NominalDef of {
      id : nominal_id;
      name : string;
      num_params : int;
      captures : term list;
      ctors : (string * term list) list;
      body : term;
    }
      (** Nominal type definition. [captures] are the declaration's own free
          variables, as terms in the definition's scope. Evaluator builds the
          [VNominal] over their values, builds constructor values, extends env
          with [type, ctor1, ..., ctorN], then evaluates [body]. The body's Var
          indices account for these bindings. *)
  | EffectDef of {
      id : effect_id;
      name : string;
      num_params : int;
      ops : (string * term * term) list;
      body : term;
    }
      (** Effect family definition. The [id] is allocated during elaboration so
          repeated evaluation of the same declaration remains applicative. *)
  | Perform of { eff : term; op : string; arg : term }
  | Tunnel of { named : term list; handlers : int list; body : term }
      (** An application whose latent row has an open tail (tunneling, E5): a
          request coming out of it whose effect instance is none of [named] (the
          instances the row names, family and parameters, E1) belongs to the
          caller's caller, so it skips [handlers] - the handlers lexically
          enclosing the call in its function body. *)
      (** Effect operation invocation. Handlers/runtime bubbling are not implemented yet. *)
  | RefTy of term * term
      (** [Ref(h, A)]: a reference into the hidden heap [h] holding an [A]. The
          heap is never written in source: surface [Ref(A)] takes it as an
          implicit argument, so each use gets a fresh one. *)
  | RefNew of term
  | RefGet of term
  | RefSet of term * term
  | Imported of value
      (** A compilation unit spliced in at its import site. The unit is
          elaborated against the base context, so its *term* is anchored there
          and would carry the wrong de Bruijn indices anywhere else - notably at
          a second import, at a different binder depth. Its *value* carries its
          own environment, so transporting that is always sound. [eval] returns
          it unchanged.
          See docs/wayfinder/tickets/imported-module-elaboration-context.md. *)
  | Stx of Syntax.t
  | Quote of { template : value; holes : (string * term) list }
      (** Quoted syntax: [template] is the reflection value of the quoted
          form, holes still in place; evaluating fills each hole with its
          term's value (see [Quote_holes]). *)

and match_branch =
  | ValueBranch of core_pat * term
  | EffectBranch of {
      handler : int;  (** the lexical handler (match) this branch belongs to *)
      eff : value;
      op : string;
      arg_pat : core_pat;
      body : term;
    }

and core_pat =
  | CPatCon of string * int * core_pat list
      (** Constructor pattern. [name] is the constructor tag, [num_type_params]
          is how many leading spine elements are type args (skipped during
          matching), [sub_pats] bind the payload elements. *)
  | CPatSyn of { name : string; sub_pats : core_pat list; rhs : core_pat }
      (** Pattern synonym reference. [name] is the synonym name, [sub_pats] are
          user-provided sub-patterns, [rhs] is the synonym's RHS pattern with
          user sub-patterns already substituted. *)
  | CPatAtom of Atom.t
      (** Literal atom pattern. *)
  | CPatType of Atom_ty.t
      (** Primitive type-head pattern. *)
  | CPatProd of core_pat list
      (** Tuple pattern. *)
  | CPatOr of core_pat * core_pat
      (** Or-pattern. Both alternatives bind the same variables. *)
  | CPatRecord of { fields : (string * core_pat) list; partial : bool }
      (** Record-instance pattern. *)
  | CPatStructType of { fields : (string * core_pat) list; partial : bool }
      (** Struct type pattern for type-case. Field subpatterns match field types. *)
  | CPatWild
      (** Wildcard — matches anything, binds nothing. *)
  | CPatNominalHead of { id : nominal_id; name : string; num_params : int; head : term option; param_pats : core_pat list }
      (** Nominal type-head pattern for type-case. [id] is the unique nominal identity
          for switch comparison, [name] is the nominal type name,
          [num_params] is how many type params the nominal has, [param_pats] are
          type-level sub-patterns matched against the nominal's parameter values.
          [head] is the written head, read in the match's scope: at run time a
          type matches it only if it is that instance (E11) - the same captures,
          and for a generative declaration the same evaluation. *)
  | CPatBind
      (** Variable binding — matches anything, binds the matched value.
          No name needed — binding is by de Bruijn index. *)

and struct_binding_term =
  | LetBind of string * struct_field_kind * term
  | TypeBind of { name : string; kind : struct_field_kind; id : nominal_id; num_params : int;
                  captures : term list; ctors : (string * int) list }
      (** A nominal type binding: its declaration [id], its own free variables
          ([captures], terms in the binding's scope) and each constructor's
          payload arity. The evaluator builds the nominal and its constructors
          in the scope it is pushed in ([binding_slots]), so a type declared
          under a binder is instantiated per evaluation. *)
  | EffectBind of string * struct_field_kind * value
      (** name, kind, effect_family_value. Operations are metadata and are not
          exposed as fields in phase one. *)
  | ImplBind of string option * struct_field_kind * term * value
      (** name, kind, dictionary term, dictionary type. Impl evidence extends
          runtime scope for trait resolution. An impl is anonymous by default;
          [impl NAME : Trait(Args) = …] names it, and a named impl is also
          reachable as a member, so a use site can say WHICH impl it means
          instead of having to [open] the module that defines it.
          See docs/wayfinder/topics/impl-visibility.md. *)
  | PatternSynBind of string * struct_field_kind * value
      (** name, kind, VPatternSyn value. *)
  | OpenBind of term * open_member list
      (** [open <module-term>] inside a binding list — the binding-list
          counterpart of the expression form [Open]. Contributes no field; it
          extends the runtime scope with the members its type opens (in entry
          order) so the de Bruijn indices of the *subsequent* bindings, which
          the elaborator resolved against the opened context, line up. *)

and module_entry =
  | ModuleField of string * struct_field_kind * value
  | ModuleImpl of string option * struct_field_kind * value * value
      (** name, kind, dictionary type, dictionary value. Kept in source order with
          fields so de Bruijn references across module bindings remain valid. *)

and struct_entry =
  | StructField of string * struct_field_kind * value
  | StructImpl of string option * struct_field_kind * value * value
      (** name, kind, dictionary type, dictionary value. Kept in source order with
          fields so de Bruijn references across struct bindings remain valid. *)

and syntax_object =
  | StxExpr of Syntax.t
  | StxTypeExpr of Syntax.t
  | StxPattern of Syntax.pat
  | StxDecl of Syntax.struct_binding
  | StxDecls of Syntax.struct_binding list

(* Semantic domain — de Bruijn levels for variables *)

and env = value list (* head = most recently bound *)

and effect_row_closure = { env : env; effects : term list; tail : term option }

and effect_row_value = { effect_values : value list; tail_value : value option }

(*
   Notation used in the [value] constructor comments:

     #n         de Bruijn level variable, i.e. VRigid {lvl = n; spine = []}
     ?n         metavariable with id n, i.e. VFlex {id = n; spine = []}
     +, *, ==   primitives by name, i.e. HPrim "+"
     x, y       when used in a spine position, also VRigid variables
     5, true    literal constants = VAtom (I64 5L), VAtom (Bool true), etc.
     [a, b]     spine contents (arguments applied to a variable or meta)
     |          empty spine / no arguments
     >          separates head from frames in VNeutral
     if _       shorthand for an FIf frame

   So:
     [?2[x, y]]    = VFlex {id = 2; spine = [VRigid {lvl=0}; VRigid {lvl=1}]}
     [#3 > if _]   = VNeutral {head = HVar 3; frames = [FIf ...]}
     [+|]         = VNeutral {head = HPrim "+"; frames = []}
     [#1[5]]       = VRigid {lvl = 1; spine = [VAtom (I64 5L)]}
*)

and value =
  | VLam of { body : closure }
  | VPi of {
      explicitness : explicitness;
      domain : value;
      effects : effect_row_closure;
      codomain : closure;
    }
  | VU
  | VPatternSyn of {
      name : string;
      params : string list;
      rhs : core_pat;
      scrutinee_ty : value;
    }
  | VEffectRowTy
  | VEffectRow of effect_row_value
  | VAtom of Atom.t
  | VAtomTy of Atom_ty.t
  | VProd of value list (* value-level tuple *)
  | VProdTy of value list (* type-level tuple — lives in VU *)
  | VFix of fix_closure
  | VGlued of { fix : fix_closure; arg : value; unfolded : value Lazy.t }
      (** A pure fixpoint applied to [arg] under the checker, unfolded only when
          inspected ([force]): conversion compares two calls of the same
          fixpoint by their arguments first (lazy delta). *)
  | VSig of closure
      (** A signature value (see [Sig]): apply to the described module to get
          its member types as a [VModule] with [partial = true]. *)
  | VModule of {
      entries : module_entry list;
      partial : bool;
    }
      (** Namespace/module value or partial module signature. Entries preserve
          binding order. Field entries carry visibility and member kind, while
          impl entries carry trait evidence without exposing generated fields. *)
  | VStruct of {
      entries : struct_entry list;
      partial : bool;
    }
      (** Record struct type. Entries preserve binding order. Field entries carry
          record constructor fields and associated members; impl entries carry
          trait evidence scoped to this struct type without exposing generated
          fields. [partial] = width-subtyped record shape. *)
  | VRecord of { typ : value; fields : (string * value) list }
      (** Record instance whose type is a [VStruct]. *)
  | VNominal of {
      id : nominal_id;
      name : string;
      num_params : int;
      captures : value list;
      params : value list;
    }
      (** Nominal ADT type. Its identity is its declaration [id] and the values
          of the declaration's own free variables [captures] (E11: applicative);
          two nominals are equal when both agree, by conversion. [num_params] is
          the arity of the template (unapplied) nominal; [params] are the applied
          type arguments, e.g. [Option I64] has params = [VAtomTy TI64]. Its
          constructors are read from the declaration ([nominal_constructors]). *)
  | VEffect of {
      id : effect_id;
      name : string;
      params : value list;
      operations : (string * closure * closure) list;
    }
      (** Nominal effect family instance. Unifies by [id] and applied params;
          operation signatures are declaration metadata, not structural identity. *)
  | VTrait of { trait_id : int; trait_name : string }
      (** Trait declaration value. Trait lookup metadata lives in the elaborator;
          this value preserves declaration identity through evaluation. *)
  | VTraitDict of {
      trait_id : int;
      trait_name : string;
      args : value list;
      fields : (string * value) list;
    }
      (** Trait dictionary type. The runtime dictionary value is struct-like,
          but the type is not encoded as private marker fields on [VStruct]. *)
  | VRecOcc of { id : int; name : string; captures : value list; args : value list }
      (** A recursive occurrence (see [RecOcc]): equal only to an occurrence of the
          same identity; unfolds to its struct type where a shape is needed. *)
  | VRefTy of value * value (* heap, element *)
  | VRef of value ref
  | VCon of { name : string; spine : value list; nominal : value }
      (** Fully saturated constructor value. [name] is the constructor tag,
          [spine] collects type+value arguments in application order
          ([spine = []] for nullary constructors like [Red]),
          [nominal] is the fully-applied nominal type this constructor
          belongs to. Pattern matching (Phase 5) dispatches on [name] and
          binds [spine] elements to sub-patterns. *)
  | VCont of cont
      (** Interpreter-only one-shot continuation token for algebraic effect handlers. *)
  | VStx of syntax_object
      (** Compile-time syntax object used by macro expansion. The tagged shape
          lets the public [Syntax] API distinguish parser syntax classes. *)
  | VNeutral of { ty : value; neutral : neutral }
      (** Stuck computation with a primitive or a variable/metavariable wrapped in
          elimination frames ([FIf], [FProj]). Unification decomposes these:
          check head equality, then recurse on frames.

          Examples:
          - [+|]  — bare "+" primitive with no frames.
          - [#3 > if _]  — variable #3 stuck as an if-condition.
          - [?7 > match _]  — metavariable ?7 stuck as a match scrutinee: the
            VFlex becomes a VNeutral with [head = HMeta 7] and an [FMatch]
            frame. *)

  | VFlex of { id : meta_id; spine : spine }
      (** Metavariable applied to a spine. Unification dispatches to [solve] when
          the spine is a *pattern* (distinct rigid variables). Otherwise
          [spine_to_renaming] raises an error.

          Examples:
          - [?2[x, y]]  — ?M applied to distinct bound variables. Valid pattern;
            [solve] renames x→λa, y→λb and substitutes to produce [λa b. rhs].
          - [?2[3]]  — ?M applied to constant. Structurally valid ([apply] extends
            the spine with no checks), but unification rejects it: non-variable
            in meta spine.
          - [?2[x, x]]  — duplicate variable; non-linear spine, also rejected. *)

  | VRigid of { lvl : lvl; spine : spine }
      (** Bound variable applied to arguments. Created by [bind] as [#n[]] and
          extended by [apply] as arguments accumulate.

          Examples:
          - [#3]  — bare variable at level 3.
          - [#1[5]]  — variable #1 applied to constant 5.

          Why a separate constructor? Three kinds of "stuck", three strategies:
          [VRigid] → compare levels; [VNeutral] → decompose head + frames;
          [VFlex] → solve via pattern unification. *)

and neutral = { head : head; frames : frame list }
and head =
  | HVar of lvl
  | HMeta of meta_id
  | HPrim of string
and spine = value list

and cont = { mutable used : bool; resume : value -> result }

and result = Done of value | Effect of effect_request

and effect_request = {
  eff : value;
  op : string;
  arg : value;
  skips : int list;
    (** The lexical handlers this request passes without being handled (tunneling). *)
  k : value -> result;
}

(* Elimination frames on stuck terms *)
and frame =
  | FApp of value
  | FProj of int
  | FDot of string
  | FRefGet
  | FRefSet of value
  | FMatch of (core_pat * closure) list
      (** Match frame for stuck scrutinees. Effect branches are runtime-only
          handlers and do not participate in neutral matching. *)

and closure = { env : env; body : term }

(* A recursive group member: its binder name (for errors only), and whether its
   call is known pure (an empty effect row), so the checker may compare two calls
   of it without unfolding them. *)
and fix_member = { fix_name : string; fix_pure : bool; fix_body : term }

(* The [fix_index]th member of a recursive group, closed over [fix_env]. *)
and fix_closure = { fix_members : fix_member list; fix_env : env; fix_index : int }

let empty_effect_row = { effects = []; tail = None }
let is_empty_effect_row row = List.is_empty row.effects && Option.is_none row.tail
let effect_row_closure env row = { env; effects = row.effects; tail = row.tail }

(* Pure arrows, for types the compiler writes itself (primitives, constructors). *)
let pure_effects = effect_row_closure [] empty_effect_row
let ( ^-> ) = fun lhs rhs -> VPi { explicitness = Explicit; domain = lhs; effects = pure_effects; codomain = { env = []; body = rhs } }
let ( ^=> ) = fun lhs rhs -> VPi { explicitness = Implicit; domain = lhs; effects = pure_effects; codomain = { env = []; body = rhs } }
let ( ^->> ) = fun lhs rhs -> Pi { explicitness = Explicit; domain = lhs; effects = empty_effect_row; codomain = rhs }
let ( ^=>> ) = fun lhs rhs -> Pi { explicitness = Implicit; domain = lhs; effects = empty_effect_row; codomain = rhs }

let validate_module_fields fields =
  if List.exists (fun (_, kind, _) -> kind = Field || kind = Method || kind = PrivateMethod) fields then
    failwith "VModule invariant violation: non-module field"

(* A dotted lookup resolves to the LAST field of a given name, so a later
   binding shadows an earlier one exactly as it does in a [do] block or through
   [open]. Duplicate field names are legal and reachable: [type T = T I64] binds
   the type and the constructor both as [T], and the constructor, coming later,
   is what [M.T] means. Every field lookup in the elaborator and the evaluator
   must use this, or the two disagree about which binding a path denotes. *)
(* THE binding-list contribution contract, in one place.

   A [struct_binding_term] contributes an ordered list of *slots* to the scope.
   The elaborator and the evaluator must push exactly these, in this order, or
   the de Bruijn index of every later binding is wrong - and wrong quietly,
   yielding a [Failure "nth"] or a silently incorrect value rather than a type
   error. The two sides hang different payloads on a slot (a type and a value on
   one, a value on the other), which is why the slot carries the payload's
   *source* rather than the payload: what the term already holds, what has to be
   evaluated, and what each side fills in for itself.

   [OpenBind] has no slot list: its members are projections of a module that has
   to be evaluated first; their count is the term's member list.
   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
type slot_source =
  | SlotDef of term  (** evaluate this term in the scope so far *)
  | SlotValue of value  (** the term already holds the value *)
  | SlotPlaceholder
      (** a nominal's parameter: each side supplies its own stand-in, since the
          term records how many there are but not what they are called *)

type slot = {
  sl_name : string option;
  sl_kind : struct_field_kind;
  sl_source : slot_source;
}

let slot ?name kind sl_source = { sl_name = name; sl_kind = kind; sl_source }

(* A capture is a variable of the scope a nominal is declared in; [pushed]
   entries later it sits that much further out. *)
let shift_capture pushed = function
  | Var ix -> Var (ix + pushed)
  | _ -> invalid_arg "a nominal's capture is a variable of its declaring scope"

(* The unapplied nominal of a declaration, over its captures. *)
let nominal_template ~id ~name ~num_params captures = NomRef { id; name; num_params; captures; params = [] }

(* A constructor as the lambda chain over its type params and payloads.
   [nominal] is the unapplied nominal, as a term in the scope the chain is built
   in; the chain's own binders push it out. *)
let ctor_term ~nominal ~name ~nominal_name ~num_params ~payload_count =
  let total = num_params + payload_count in
  let param_vars = List.init num_params (fun i -> Var (total - 1 - i)) in
  let payload_vars = List.init payload_count (fun i -> Var (payload_count - 1 - i)) in
  let nominal =
    match nominal with
    | NomRef n -> NomRef { n with captures = List.map (shift_capture total) n.captures }
    | t -> shift_capture total t
  in
  let body = Ctor { name; spine = param_vars @ payload_vars; nominal_name; nominal_spine = param_vars; nominal } in
  let rec wrap n t = if n = 0 then t else wrap (n - 1) (Lam t) in
  wrap total body

let binding_slots : struct_binding_term -> slot list option = function
  | LetBind (name, kind, def) -> Some [ slot ~name kind (SlotDef def) ]
  | EffectBind (name, kind, eff) -> Some [ slot ~name kind (SlotValue eff) ]
  | PatternSynBind (name, kind, syn) -> Some [ slot ~name kind (SlotValue syn) ]
  | ImplBind (name, kind, def, _ty) ->
      Some [ { sl_name = name; sl_kind = kind; sl_source = SlotDef def } ]
  | TypeBind { name; kind; id; num_params; captures; ctors } ->
      (* Pushed in order: the params (placeholders), each constructor, then the
         type; each term is read in the scope so far, so the captures shift by
         what this binding has already pushed. *)
      let nominal pushed = nominal_template ~id ~name ~num_params (List.map (shift_capture pushed) captures) in
      Some
        (List.init num_params (fun _ -> slot kind SlotPlaceholder)
        @ List.mapi
            (fun i (cname, payload_count) ->
              slot ~name:cname kind (SlotDef (ctor_term ~nominal:(nominal (num_params + i)) ~name:cname ~nominal_name:name ~num_params ~payload_count)))
            ctors
        @ [ slot ~name kind (SlotDef (nominal (num_params + List.length ctors))) ])
  | OpenBind _ -> None

(* The slots of a binding list, or [None] if it contains an [open]. *)
let binding_list_slots bindings =
  List.fold_left
    (fun acc b ->
      match (acc, binding_slots b) with
      | Some slots, Some s -> Some (slots @ s)
      | _ -> None)
    (Some []) bindings

let binding_list_width bindings =
  Option.map List.length (binding_list_slots bindings)

(* How many environment entries a pattern binds (an or-pattern's sides bind
   alike). *)
let rec pat_binder_count = function
  | CPatBind -> 1
  | CPatWild | CPatAtom _ | CPatType _ -> 0
  | CPatSyn { rhs; _ } -> pat_binder_count rhs
  | CPatOr (lhs, _) -> pat_binder_count lhs
  | CPatProd pats | CPatCon (_, _, pats) | CPatNominalHead { param_pats = pats; _ } ->
      List.fold_left (fun n p -> n + pat_binder_count p) 0 pats
  | CPatRecord { fields; _ } | CPatStructType { fields; _ } ->
      List.fold_left (fun n (_, p) -> n + pat_binder_count p) 0 fields

(* A pattern with [f] applied to each nominal head's term - read in the match's
   scope, whatever binders the pattern around it adds. *)
let rec map_pat_heads f = function
  | CPatNominalHead h -> CPatNominalHead { h with head = Option.map f h.head; param_pats = List.map (map_pat_heads f) h.param_pats }
  | CPatSyn s -> CPatSyn { s with sub_pats = List.map (map_pat_heads f) s.sub_pats; rhs = map_pat_heads f s.rhs }
  | CPatOr (l, r) -> CPatOr (map_pat_heads f l, map_pat_heads f r)
  | CPatProd ps -> CPatProd (List.map (map_pat_heads f) ps)
  | CPatCon (n, k, ps) -> CPatCon (n, k, List.map (map_pat_heads f) ps)
  | CPatRecord r -> CPatRecord { r with fields = List.map (fun (n, p) -> (n, map_pat_heads f p)) r.fields }
  | CPatStructType r -> CPatStructType { r with fields = List.map (fun (n, p) -> (n, map_pat_heads f p)) r.fields }
  | (CPatWild | CPatBind | CPatAtom _ | CPatType _) as p -> p

(* A single [rec]: a recursive group of one. *)
let fix_one name pure body = Fix { members = [ { fix_name = name; fix_pure = pure; fix_body = body } ]; index = 0 }

let fix_member (fc : fix_closure) = List.nth fc.fix_members fc.fix_index

(* The environment a group member's body runs in: every member, the first
   outermost, over the group's own environment. *)
let fix_body_env (fc : fix_closure) =
  List.rev (List.mapi (fun i _ -> VFix { fc with fix_index = i }) fc.fix_members) @ fc.fix_env

(** Rebuild a term with [f under sub] applied to each immediate subterm [sub],
    where [under] is how many environment entries the evaluator has pushed
    between the term and that subterm: [Some n], or [None] when the count is
    known only by evaluating.
    This is the one statement of each form's binder count; [Nbe.eval] is what it
    restates, and every de Bruijn traversal reads it instead of its own copy.
    [Var] and the leaves are returned unchanged. *)
let map_subterms (f : int option -> term -> term) (t : term) : term =
  let at n = f (Some n) in
  let row n (r : effect_row) = { effects = List.map (at n) r.effects; tail = Option.map (at n) r.tail } in
  let bindings bs =
    let step (under, acc) b =
      let g = f under in
      let b' =
        match b with
        | LetBind (name, kind, def) -> LetBind (name, kind, g def)
        | ImplBind (name, kind, def, ty) -> ImplBind (name, kind, g def, ty)
        | OpenBind (def, members) -> OpenBind (g def, members)
        | TypeBind t -> TypeBind { t with captures = List.map g t.captures }
        | EffectBind _ | PatternSynBind _ -> b
      in
      let width =
        match b with
        | OpenBind (_, members) -> Some (List.length members)
        | _ -> Option.map List.length (binding_slots b)
      in
      ((match (under, width) with Some u, Some w -> Some (u + w) | _ -> None), b' :: acc)
    in
    List.rev (snd (List.fold_left step (Some 0, []) bs))
  in
  match t with
  | Var _ | Atom _ | AtomTy _ | U | EffectRowTy | Prim _ | Meta _ | InsertedMeta _ | TraitRef _ | Stx _
  | Imported _ ->
      t
  | Lam body -> Lam (at 1 body)
  | Fix f ->
      let n = List.length f.members in
      Fix { f with members = List.map (fun m -> { m with fix_body = at n m.fix_body }) f.members }
  | Ap (fn, expl, arg) -> Ap (at 0 fn, expl, at 0 arg)
  | Let (ty, def, body) -> Let (at 0 ty, at 0 def, at 1 body)
  | Pi { explicitness; domain; effects; codomain } ->
      Pi { explicitness; domain = at 0 domain; effects = row 1 effects; codomain = at 1 codomain }
  | EffectRowLit r -> EffectRowLit (row 0 r)
  | Prod ts -> Prod (List.map (at 0) ts)
  | ProdTy ts -> ProdTy (List.map (at 0) ts)
  | RecOcc r -> RecOcc { r with captures = List.map (at 0) r.captures; args = List.map (at 0) r.args }
  | NomRef n -> NomRef { n with captures = List.map (at 0) n.captures; params = List.map (at 0) n.params }
  | EffectRef (name, ts) -> EffectRef (name, List.map (at 0) ts)
  | RefTy (h, a) -> RefTy (at 0 h, at 0 a)
  | RefNew a -> RefNew (at 0 a)
  | RefGet a -> RefGet (at 0 a)
  | RefSet (r, v) -> RefSet (at 0 r, at 0 v)
  | Proj (a, i) -> Proj (at 0 a, i)
  | Dot (a, field) -> Dot (at 0 a, field)
  | Perform p -> Perform { p with eff = at 0 p.eff; arg = at 0 p.arg }
  | Tunnel t -> Tunnel { t with named = List.map (at 0) t.named; body = at 0 t.body }
  | Quote q -> Quote { q with holes = List.map (fun (n, h) -> (n, at 0 h)) q.holes }
  | RecordConstruct { typ; fields } ->
      RecordConstruct { typ = at 0 typ; fields = List.map (fun (n, v) -> (n, at 0 v)) fields }
  | TraitDictTy d ->
      TraitDictTy { d with args = List.map (at 0) d.args; fields = List.map (fun (n, v) -> (n, at 0 v)) d.fields }
  | Ctor c -> Ctor { c with spine = List.map (at 0) c.spine; nominal_spine = List.map (at 0) c.nominal_spine; nominal = at 0 c.nominal }
  | Open (s, members, body) -> Open (at 0 s, members, at (List.length members) body)
  | Module { bindings = bs; signature } -> Module { bindings = bindings bs; signature }
  | Sig body -> Sig (at 1 body)
  | Struct s ->
      Struct { s with con_fields = List.map (fun (n, ty) -> (n, at 0 ty)) s.con_fields; bindings = bindings s.bindings }
  | Match (scrut, branches) ->
      let branch = function
        | ValueBranch (pat, body) -> ValueBranch (map_pat_heads (at 0) pat, at (pat_binder_count pat) body)
        | EffectBranch e ->
            (* the continuation, then the argument pattern's binders *)
            EffectBranch { e with arg_pat = map_pat_heads (at 0) e.arg_pat; body = at (1 + pat_binder_count e.arg_pat) e.body }
      in
      Match (at 0 scrut, List.map branch branches)
  | NominalDef d ->
      (* parameters, the nominal, its type-name entry when parameterised, then
         one entry per constructor *)
      let body_under = d.num_params + 1 + (if d.num_params > 0 then 1 else 0) + List.length d.ctors in
      NominalDef
        { d with
          captures = List.map (at 0) d.captures;
          ctors = List.map (fun (c, payloads) -> (c, List.map (at d.num_params) payloads)) d.ctors;
          body = at body_under d.body }
  | EffectDef d ->
      EffectDef
        { d with
          ops = List.map (fun (op, input, output) -> (op, at d.num_params input, at d.num_params output)) d.ops;
          body = at 1 d.body }

(** The immediate subterms of a term, each with the entries it sits under (see
    [map_subterms]). *)
let subterms (t : term) : (int option * term) list =
  let acc = ref [] in
  ignore (map_subterms (fun under sub -> acc := (under, sub) :: !acc; sub) t);
  List.rev !acc

(* A dotted path denotes the last member of its name (I3): every member lookup
   takes the last match. *)
let find_map_last f items =
  List.fold_left (fun acc item -> match f item with Some _ as found -> found | None -> acc) None items

let find_field_last p fields = find_map_last (fun field -> if p field then Some field else None) fields

(* A named impl is also a member: [M.eq_C] denotes it. Anonymous impls are not
   reachable this way and stay available only through [open]. The type view of a
   module and its value view both carry (type, value) on an impl entry, so which
   one a lookup wants has to be said at the call site. *)
let module_impl_opt entries name =
  find_map_last
    (function ModuleImpl (Some n, kind, ty, v) when String.equal n name -> Some (kind, ty, v) | _ -> None)
    entries

let module_impl_type_opt entries name = Option.map (fun (kind, ty, _) -> (kind, ty)) (module_impl_opt entries name)
let module_impl_value_opt entries name = Option.map (fun (kind, _, v) -> (kind, v)) (module_impl_opt entries name)

let module_entry_fields entries =
  List.filter_map (function ModuleField (name, kind, value) -> Some (name, kind, value) | ModuleImpl _ -> None) entries

let struct_entry_fields entries =
  List.filter_map (function StructField (name, kind, value) -> Some (name, kind, value) | StructImpl _ -> None) entries

module MetaContext = struct
  type entry = Solved of value | Unsolved
  (* The metas and the evaluation budget travel together: both are the state of
     one checking session that every evaluator call is handed. *)
  type t = { entries : entry Dynarray.t; budget : value Eval_budget.t }

  (* [budget] is shared, not copied: a macro application evaluates its body
     with fresh metas (it solves nothing its caller needs) under the budget of
     the expansion it belongs to. *)
  let create ?(budget = Eval_budget.create ()) () : t = { entries = Dynarray.create (); budget }

  let fresh (mc : t) : meta_id =
    let id = Dynarray.length mc.entries in
    Dynarray.add_last mc.entries Unsolved;
    id

  let solve (mc : t) (id : meta_id) (v : value) =
    Dynarray.set mc.entries id (Solved v)

  let lookup (mc : t) (id : meta_id) : entry =
    Dynarray.get mc.entries id

  (* The id the next fresh meta gets: every meta created from here on is newer. *)
  let count (mc : t) : int = Dynarray.length mc.entries

  let snapshot (mc : t) : entry array =
    Array.init (Dynarray.length mc.entries) (Dynarray.get mc.entries)

  let restore (mc : t) (snapshot : entry array) : unit =
    while Dynarray.length mc.entries > Array.length snapshot do
      ignore (Dynarray.pop_last mc.entries)
    done;
    Array.iteri (Dynarray.set mc.entries) snapshot
end

(** Global counter for fresh nominal type identities.
    Each [type Foo = ...] definition gets a unique [nominal_id].
    Equality of nominal types compares by id, not by name —
    two separately-defined types with the same name are distinct. *)
module NominalId : sig
  val fresh : unit -> nominal_id
  (* The id [fresh] mints next: ids from here on are declared after this point. *)
  val next : unit -> nominal_id
end = struct
  let counter = ref 0
  let fresh () =
    let id = !counter in
    incr counter;
    id
  let next () = !counter
end

(* Nominals declared by a module whose evaluation performs something (E11):
   each evaluation is a new instance, named by the binder it is sealed at. *)
let generative_nominals : (nominal_id, unit) Hashtbl.t = Hashtbl.create 16

(* A nominal's declaration: each constructor's payload types, as terms over the
   type params (innermost, the last param at index 0) and then the declaration's
   captures. A recursive nominal's payloads refer to it before this is recorded;
   [finish_nominal] records it under the id once the payloads exist, and
   [nominal_constructors] is the one way to read a nominal's constructors - an
   instance's payload closures, over its own captures. *)
let nominal_decls : (nominal_id, (string * term list) list) Hashtbl.t = Hashtbl.create 64

let finish_nominal id ctors = Hashtbl.replace nominal_decls id ctors

let nominal_constructors id (captures : value list) : (string * closure list) list =
  match Hashtbl.find_opt nominal_decls id with
  | None -> []
  | Some ctors -> List.map (fun (c, payloads) -> (c, List.map (fun body -> { env = captures; body }) payloads)) ctors

(* A [rec] struct type's declaration. The binding mints its id before its body is
   elaborated, so the body's references to the type are occurrences of it;
   [finish_record] records the finished body (a struct type, or a function of the
   parameters to one) as a term in the declaring environment, and the levels an
   occurrence captures (E11). An occurrence unfolds to that body evaluated with
   its own captures in place of those levels - one instance per captures. *)
type finished_record = { record_env : env; record_body : term; record_levels : lvl list }

let record_counter = ref 0
let fresh_record_id () = let id = !record_counter in incr record_counter; id
let finished_records : (int, finished_record) Hashtbl.t = Hashtbl.create 64
let finish_record id record = Hashtbl.replace finished_records id record

(* The environment a recursive occurrence's body is read in: the declaring one,
   with the occurrence's captures at the levels they were taken from. *)
let record_instance_env (r : finished_record) (captures : value list) =
  let width = List.length r.record_env in
  let at_position = List.map2 (fun level c -> (width - 1 - level, c)) r.record_levels captures in
  List.mapi (fun i v -> match List.assoc_opt i at_position with Some c -> c | None -> v) r.record_env

(** Global counter for fresh effect family identities.
    Equality of effect families compares by id and instantiated params, not by
    operation names or signatures. *)
module EffectId : sig
  val fresh : unit -> effect_id
end = struct
  let counter = ref 0
  let fresh () =
    let id = !counter in
    incr counter;
    id
end

(** The one mutation effect family, [Mutate(h)] over a hidden heap [h]: allocating,
    reading or writing a reference of that heap. Its family carries no
    operations; a reference form performs it directly. *)
let mutate_effect_id = EffectId.fresh ()
