type ix = int (* de Bruijn index: distance to binder *)
type lvl = int (* de Bruijn level: distance from outermost scope *)
type meta_id = int
type atom_ty = TI64 | TBool | TUnit | TChar [@@deriving eq]

(* Bound = introduced by lambda/pi, Defined = introduced by let.
   InsertedMeta uses this to know which scope vars to abstract over. *)
type bd = Bound | Defined

type struct_field_kind = Field | Public | Private

type term =
  | Var of ix
  | Lam of term
  | Ap of term * term
  | Let of term * term * term   (* let _ : A = def in body *)
  | Pi of term * term           (* (x : A) -> B, x bound in B *)
  | U (* Type : Type *)
  | Atom of Syntax.Ast.Atom.t
  | AtomTy of atom_ty
  | If of term * term * term
  | Prod of term list (* value-level tuple: (a, b) has type ProdTy [A, B] *)
  | ProdTy of term list (* type-level tuple: (A, B) has type U *)
  | Fix of term
  | Proj of term * int             (* positional tuple projection: e.0 *)
  | Dot of term * string           (* named struct field access: e.field *)
  | Struct of {
      con_fields : (string * term) list;
      bindings : (string * struct_field_kind * term) list;
      partial : bool;
    }
      (** [con_fields] = field type declarations; [bindings] = ordered
          let-bindings tagged [Computed]/[Private]. [partial] = width-subtyped
          constraint (only from rename, never from elaboration). *)
  | Open of term * term            (* open S in body — evaluator extends env with struct fields *)
  | Prim of string (* evaluated as VNeutral with HPrim head — no VPrim needed *)
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

(* Semantic domain — de Bruijn levels for variables *)

type env = value list (* head = most recently bound *)

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
  | VPi of { domain : value; codomain : closure }
  | VU
  | VAtom of Syntax.Ast.Atom.t
  | VAtomTy of atom_ty
  | VProd of value list (* value-level tuple *)
  | VProdTy of value list (* type-level tuple — lives in VU *)
  | VFix of { body : closure }
  | VStruct of {
      fields : (string * struct_field_kind * value) list;
      partial : bool;
    }
      (** Struct value/type. [fields] carry a [struct_field_kind]:
          [Constructor] = must be provided at construction,
          [Computed] = in type but forbidden at construction,
          [Private] = invisible outside.  [partial] = width-subtyped
          (matches any struct with at least these fields). *)
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

and closure = { env : env; body : term }

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
