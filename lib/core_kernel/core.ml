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
  | Fix of term
  | Proj of term * int             (* positional tuple projection: e.0 *)
  | Dot of term * string           (* named member/field access: e.field *)
  | RecordConstruct of { typ : term; fields : (string * term) list }
  | Module of { bindings : struct_binding_term list }
  | Struct of {
      con_fields : (string * term) list;
      bindings : struct_binding_term list;
      partial : bool;
    }
  | Open of term * term            (* open S in body — evaluator extends env with module-view fields *)
  | Prim of string (* evaluated as VNeutral with HPrim head — no VPrim needed *)
  | Con of string
      (** Residual constructor/type reference in quoted output. Created when
          [quote] hits a [VCon] or [VNominal]. [eval] scans the environment
          by name for a matching VCon or VNominal. Like [Meta], never
          produced by the elaborator. *)
  | NomRef of string * term list
      (** Applied nominal type reference. [eval] scans the environment for
          a [VNominal] template with this name, evaluates the param terms,
          and returns [VNominal] with those params. Used in constructor Pi
          types to express e.g. [Option a] where [a] is a de Bruijn var. *)
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
  | SelfTypeRef of term list
      (** Internal recursive record [Self] type reference while a record type is
          being elaborated. The arguments are the current record parameters. *)
  | Ctor of {
      name : string;
      spine : term list;           (* type args then payload args *)
      nominal_name : string;       (* VNominal template name, looked up in env *)
      nominal_spine : term list;
    nominal_value : value;   (* type args for the nominal's params *)
    }
      (** Constructor value term. [eval] evaluates spine terms to values,
          looks up [nominal_name] in env for the VNominal template, applies
          [nominal_spine] as params, and constructs [VCon]. Used only inside
          constructor lambda chains; never in surface syntax. *)
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
      ctors : (string * term list) list;
      body : term;
    }
      (** Nominal type definition. Evaluator creates a fresh VNominal, builds
          constructor values, extends env with [type, ctor1, ..., ctorN], then
          evaluates [body]. The body's Var indices account for these bindings. *)
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
      (** Effect operation invocation. Handlers/runtime bubbling are not implemented yet. *)
  | RefTy of term
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

and match_branch =
  | ValueBranch of core_pat * term
  | EffectBranch of {
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
  | CPatNominalHead of { id : nominal_id; name : string; num_params : int; param_pats : core_pat list }
      (** Nominal type-head pattern for type-case. [id] is the unique nominal identity
          for switch comparison, [name] is the nominal type name,
          [num_params] is how many type params the nominal has, [param_pats] are
          type-level sub-patterns matched against the nominal's parameter values. *)
  | CPatBind
      (** Variable binding — matches anything, binds the matched value.
          No name needed — binding is by de Bruijn index. *)

and struct_binding_term =
  | LetBind of string * struct_field_kind * term
  | TypeBind of string * struct_field_kind * value * (string * value) list
      (** name, kind, nominal_value, [(ctor_name, ctor_value)].
          Evaluator stores values directly without routing through [eval]. *)
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
  | OpenBind of term
      (** [open <module-term>] inside a binding list — the binding-list
          counterpart of the expression form [Open]. Contributes no field; it
          extends the runtime scope with the opened module's public values (in
          entry order) so the de Bruijn indices of the *subsequent* bindings,
          which the elaborator resolved against the opened context, line up. *)

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
  | VFix of { body : closure }
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
      params : value list;
      constructors : (string * closure list) list;
          (** (ctor_name, payload_type_closures). [[]] = nullary. Each closure's
              env is the definition env (without type params); its body is the
              payload type term with de Bruijn indices 0..n-1 referencing the
              type params. Instantiate by evaluating with
              [List.rev actual_params @ clo.env]. *)
    }
      (** Nominal ADT type. Unifies by [id] equality. [num_params] is the
          arity of type parameters for the template (unapplied) nominal.
          [params] are the applied type arguments, e.g. [Option I64] has
          params = [VAtomTy Atom_ty.TI64]. [constructors] maps each constructor name
          to its payload type closure. *)
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
  | VSelfType of value list
      (** Recursive record [Self] type while the record type is being built. *)
  | VRefTy of value
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
          - [?7 > if _]  — metavariable ?7 stuck in the same situation. [eval_if]
            calls [stuck_head_frames] which converts the VFlex into a VNeutral
            with [head = HMeta 7] and an [FIf] frame. *)

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
and head = HVar of lvl | HMeta of meta_id | HPrim of string
and spine = value list

and cont = { mutable used : bool; resume : value -> result }

and result = Done of value | Effect of effect_request

and effect_request = {
  eff : value;
  op : string;
  arg : value;
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

let empty_effect_row = { effects = []; tail = None }
let is_empty_effect_row row = List.is_empty row.effects && Option.is_none row.tail
let effect_row_closure env row = { env; effects = row.effects; tail = row.tail }

let validate_module_fields fields =
  if List.exists (fun (_, kind, _) -> kind = Field || kind = Method || kind = PrivateMethod) fields then
    failwith "VModule invariant violation: non-module field"

(* A dotted lookup resolves to the LAST field of a given name, so a later
   binding shadows an earlier one exactly as it does in a [do] block or through
   [open]. Duplicate field names are legal and reachable: [type T = T I64] binds
   the type and the constructor both as [T], and the constructor, coming later,
   is what [M.T] means. Every field lookup in the elaborator and the evaluator
   must use this, or the two disagree about which binding a path denotes. *)
(* THE binding-list environment-width contract, in one place.

   For every [struct_binding_term] the elaborator and the evaluator must extend
   their environments by the same number of entries in the same order, or the de
   Bruijn index of every later binding is wrong - and wrong quietly, yielding a
   [Failure "nth"] or a silently incorrect value rather than a type error. The
   two sides live in different libraries and carry different payloads (types on
   one side, values on the other), so they cannot share the extension code; this
   function is what they can share, and what a port must reproduce.

   [OpenBind] returns [None]: its width is the number of public entries of a
   module that has to be evaluated first, so it is not recoverable from the term.
   See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
let binding_width : struct_binding_term -> int option = function
  | LetBind _ | EffectBind _ | ImplBind _ | PatternSynBind _ -> Some 1
  | TypeBind (_, _, nominal, ctors) ->
      let params =
        match nominal with VNominal { num_params; _ } -> num_params | _ -> 0
      in
      Some (params + List.length ctors + 1)
  | OpenBind _ -> None

(* Total width of a binding list, or [None] if it contains an [open]. *)
let binding_list_width bindings =
  List.fold_left
    (fun acc b ->
      match (acc, binding_width b) with
      | Some n, Some w -> Some (n + w)
      | _ -> None)
    (Some 0) bindings

(* A named impl is also a member: [M.eq_C] denotes it. Anonymous impls are not
   reachable this way and stay available only through [open]. The type view of a
   module and its value view both carry (type, value) on an impl entry, so which
   one a lookup wants has to be said at the call site. *)
let module_impl_type_opt entries name =
  List.find_map
    (function
      | ModuleImpl (Some n, kind, ty, _) when String.equal n name -> Some (kind, ty)
      | _ -> None)
    entries

let module_impl_value_opt entries name =
  List.find_map
    (function
      | ModuleImpl (Some n, kind, _, v) when String.equal n name -> Some (kind, v)
      | _ -> None)
    entries

let struct_impl_type_opt entries name =
  List.find_map
    (function
      | StructImpl (Some n, kind, ty, _) when String.equal n name -> Some (kind, ty)
      | _ -> None)
    entries

let find_field_last p fields =
  List.fold_left (fun acc field -> if p field then Some field else acc) None fields

let module_entry_fields entries =
  List.filter_map (function ModuleField (name, kind, value) -> Some (name, kind, value) | ModuleImpl _ -> None) entries

let struct_entry_fields entries =
  List.filter_map (function StructField (name, kind, value) -> Some (name, kind, value) | StructImpl _ -> None) entries

module MetaContext = struct
  type entry = Solved of value | Unsolved
  type t = entry Dynarray.t

  let create () : t = Dynarray.create ()

  let fresh (mc : t) : meta_id =
    let id = Dynarray.length mc in
    Dynarray.add_last mc Unsolved;
    id

  let solve (mc : t) (id : meta_id) (v : value) =
    Dynarray.set mc id (Solved v)

  let lookup (mc : t) (id : meta_id) : entry =
    Dynarray.get mc id

  let snapshot (mc : t) : entry array =
    Array.init (Dynarray.length mc) (Dynarray.get mc)

  let restore (mc : t) (snapshot : entry array) : unit =
    while Dynarray.length mc > Array.length snapshot do
      ignore (Dynarray.pop_last mc)
    done;
    Array.iteri (Dynarray.set mc) snapshot
end

(** Global counter for fresh nominal type identities.
    Each [type Foo = ...] definition gets a unique [nominal_id].
    Equality of nominal types compares by id, not by name —
    two separately-defined types with the same name are distinct. *)
module NominalId : sig
  val fresh : unit -> nominal_id
end = struct
  let counter = ref 0
  let fresh () =
    let id = !counter in
    incr counter;
    id
end

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
