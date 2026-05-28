type ix = int (* de Bruijn index: distance to binder *)
type lvl = int (* de Bruijn level: distance from outermost scope *)
type meta_id = int
type nominal_id = int (* unique identity for each nominal type definition *)
type effect_id = int (* unique identity for each effect family definition *)
type atom_ty = TI64 | TBool | TUnit | TChar | TString | TAbsurd [@@deriving eq]

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
  | Atom of Atom.t
  | AtomTy of atom_ty
  | If of term * term * term
  | Prod of term list (* value-level tuple: (a, b) has type ProdTy [A, B] *)
  | ProdTy of term list (* type-level tuple: (A, B) has type U *)
  | Fix of term
  | Proj of term * int             (* positional tuple projection: e.0 *)
  | Dot of term * string           (* named struct field access: e.field *)
  | RecordConstruct of { typ : term; fields : (string * term) list }
  | Struct of {
      con_fields : (string * term) list;
      bindings : struct_binding_term list;
      partial : bool;
    }
  | Open of term * term            (* open S in body — evaluator extends env with struct fields *)
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
  | Ctor of {
      name : string;
      spine : term list;           (* type args then payload args *)
      nominal_name : string;       (* VNominal template name, looked up in env *)
      nominal_spine : term list;   (* type args for the nominal's params *)
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
      ctors : (string * term option) list;
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

and match_branch =
  | ValueBranch of core_pat * term
  | EffectBranch of {
      eff : term;
      op : string;
      arg_pat : core_pat;
      body : term;
    }

and core_pat =
  | CPatCon of string * int * core_pat list
      (** Constructor pattern. [name] is the constructor tag, [num_type_params]
          is how many leading spine elements are type args (skipped during
          matching), [sub_pats] bind the payload elements. *)
  | CPatAtom of Atom.t
      (** Literal atom pattern. *)
  | CPatType of atom_ty
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

(* Semantic domain — de Bruijn levels for variables *)

and env = value list (* head = most recently bound *)

and effect_row_closure = { env : env; effects : term list; tail : term option }

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
  | VAtom of Atom.t
  | VAtomTy of atom_ty
  | VProd of value list (* value-level tuple *)
  | VProdTy of value list (* type-level tuple — lives in VU *)
  | VFix of { body : closure }
  | VStruct of {
      fields : (string * struct_field_kind * value) list;
      partial : bool;
    }
      (** Struct value/type. [fields] carry a [struct_field_kind]:
          [Field] = record constructor field, [Public]/[Private] = value field,
          [Method]/[PrivateMethod] = function-valued method field. [Private]
          and [PrivateMethod] are invisible outside. [partial] = width-subtyped
          (matches any struct with at least these fields). *)
  | VRecord of { typ : value; fields : (string * value) list }
      (** Record instance whose type is a [VStruct]. *)
  | VNominal of {
      id : nominal_id;
      name : string;
      num_params : int;
      params : value list;
      constructors : (string * closure option) list;
          (** (ctor_name, payload_type_closure option). [None] = nullary.
              The closure's env is the definition env (without type params);
              its body is the payload type term with de Bruijn indices 0..n-1
              referencing the type params. Instantiate by evaluating with
              [List.rev actual_params @ clo.env]. *)
    }
      (** Nominal ADT type. Unifies by [id] equality. [num_params] is the
          arity of type parameters for the template (unapplied) nominal.
          [params] are the applied type arguments, e.g. [Option I64] has
          params = [VAtomTy TI64]. [constructors] maps each constructor name
          to its payload type closure. *)
  | VEffect of {
      id : effect_id;
      name : string;
      params : value list;
      operations : (string * closure * closure) list;
    }
      (** Nominal effect family instance. Unifies by [id] and applied params;
          operation signatures are declaration metadata, not structural identity. *)
  | VCon of { name : string; spine : value list; nominal : value }
      (** Fully saturated constructor value. [name] is the constructor tag,
          [spine] collects type+value arguments in application order
          ([spine = []] for nullary constructors like [Red]),
          [nominal] is the fully-applied nominal type this constructor
          belongs to. Pattern matching (Phase 5) dispatches on [name] and
          binds [spine] elements to sub-patterns. *)
  | VCont of Obj.t
      (** Interpreter-only one-shot continuation token for algebraic effect handlers. *)
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

(* Elimination frames on stuck terms *)
and frame =
  | FApp of value
  | FIf of { then_ : closure; else_ : closure }
  | FProj of int
  | FDot of string
  | FMatch of (core_pat * closure) list
      (** Match frame for stuck scrutinees. Effect branches are runtime-only
          handlers and do not participate in neutral matching. *)

and closure = { env : env; body : term }

let empty_effect_row = { effects = []; tail = None }
let is_empty_effect_row row = List.is_empty row.effects && Option.is_none row.tail
let effect_row_closure env row = { env; effects = row.effects; tail = row.tail }

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
