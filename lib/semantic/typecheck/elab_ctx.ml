open Core
open Elab_error
open Elab_common

(* A SCOPE: one ordered sequence of entries, viewed through several columns that
   must stay the same length, [lvl]. [env] is the column the evaluator receives -
   NbE is handed that projection alone, never the scope. [bds] records, per entry,
   whether it is a bound variable or a definition, and is the mask a meta is
   abstracted over. An entry's *type* lives in [name_table], not in a column;
   there used to be a parallel [types] list, but nothing ever read it except the
   code that rebuilt it. See docs/wayfinder/topics/core-tt-domain-model.md. *)
module Ctx = struct
  type t = {
    env : env;
    lvl : lvl;
    metas : MetaContext.t;
    bds : bd list;
    name_table : name_entry NameMap.t;
    (* Each open entered, by label, with the members it brought in. An open
       choice looks its name up here, never in [name_table]. *)
    opened : (string * name_entry NameMap.t) list;
    trait_evidence : trait_evidence list;
    self_entry : name_entry option;
    self_type : value option;
    resume_entry : name_entry option;
    loader : Core_loader.t option;
    (* The capabilities the elaborator needs from the expander to run a
       type-aware macro, and nothing more: how to apply a macro value, how to
       run an application as a call under the evaluation budget, and how to expand its output. It never consults the
       expander's binding table, so it is handed these rather than a context.
       See docs/wayfinder/tickets/expander-handle-is-a-capability-not-a-context.md. *)
    mutable macro_runtime : macro_runtime option;
    (* The base context this one grew out of: the atom types, the primitives and
       [stdlib] bound as a name. Set once, by [init_ctx]; every extension carries
       it forward, so an imported compilation unit can be elaborated against it
       instead of against whatever the importer happened to have in scope.
       [None] only in the half-built context [init_ctx] is itself assembling. *)
    base : t option;
  }

and macro_runtime = {
  run_macro : value -> value -> value;
  macro_application : 'a. name:string -> (unit -> 'a) -> 'a;
  expand : Syntax.t -> Syntax.t;
  application : unit -> Expand.application;
  (* The compiled macro a deferred call's head names. *)
  lookup_macro : string -> Expand_ctx.macro_entry option;
  (* The names of the syntactic roles visible in an open's region (M7). *)
  roles_in_open : string -> string list;
}

  (* The expander, narrowed to the capabilities above. [None] when the
     expander cannot run a macro at all. *)
  let macro_runtime_of_expander (ectx : Expand_ctx.t) : macro_runtime option =
    Option.map
      (fun eval_and_apply ->
        { run_macro = eval_and_apply ectx.Expand_ctx.budget;
          macro_application = (fun ~name f -> Expand_ctx.macro_application ectx ~name ~expand:(Expand.expand ectx) f);
          expand = Expand.expand ectx;
          application = (fun () -> Expand.application ectx);
          lookup_macro = Expand_ctx.lookup_macro_entry ectx;
          roles_in_open = Expand_ctx.roles_in_open ectx })
      ectx.Expand_ctx.eval_and_apply

  let empty () : t =
    let metas = MetaContext.create () in
    {
      env = [];
      lvl = 0;
      metas;
      bds = [];
      name_table = NameMap.empty;
      opened = [];
      trait_evidence = [];
      self_entry = None;
      self_type = None;
      resume_entry = None;
      loader = None;
      macro_runtime = None;
      base = None;
    }

  (* The context an imported compilation unit is elaborated against: this
     context's base, carrying the live loader, macro table and expander state so
     the unit can import and expand in turn. Shares [metas], so metas the unit
     leaves unsolved stay meaningful to the importer.
     See docs/wayfinder/tickets/imported-module-elaboration-context.md. *)
  let unit_base (ctx : t) : t =
    match ctx.base with
    | None -> ctx
    | Some base ->
        { base with loader = ctx.loader;
                    macro_runtime = ctx.macro_runtime }

  let bind (ctx : t) (name : string) (ty : value) : t =
    { ctx with
      env = VRigid { lvl = ctx.lvl; spine = [] } :: ctx.env;
      lvl = ctx.lvl + 1;
      bds = Bound :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table }

  let bind_anonymous (ctx : t) (ty : value) : t * name_entry =
    ({ ctx with
       env = VRigid { lvl = ctx.lvl; spine = [] } :: ctx.env;
       lvl = ctx.lvl + 1;
       bds = Bound :: ctx.bds },
     { level = ctx.lvl; ty })

  let define (ctx : t) (name : string) (ty : value) (v : value) : t =
    { ctx with
      env = v :: ctx.env;
      lvl = ctx.lvl + 1;
      bds = Defined :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table }

  (* Give an entry that already exists a name, without widening the context. A
     named impl occupies exactly the one entry [define_anonymous] pushed for it;
     naming it must not add a second, or the elaborator and the evaluator would
     disagree about that binding's width.
     See docs/wayfinder/tickets/env-width-contract-is-unnamed.md. *)
  let alias (ctx : t) (name : string) (entry : name_entry) : t =
    { ctx with name_table = NameMap.add name entry ctx.name_table }

  let hide_names (ctx : t) names : t =
    {
      ctx with
      name_table = List.fold_left (fun table name -> NameMap.remove name table) ctx.name_table names;
    }

  let define_anonymous (ctx : t) (ty : value) (v : value) : t * name_entry =
    ({ ctx with env = v :: ctx.env; lvl = ctx.lvl + 1; bds = Defined :: ctx.bds },
     { level = ctx.lvl; ty })

  let entry_ix (ctx : t) ({ level; ty } : name_entry) : ix * value = (Nbe.lvl_to_ix ctx.lvl level, ty)

  let lookup_opt (ctx : t) (name : string) : (ix * value) option =
    Option.map (entry_ix ctx) (NameMap.find_opt name ctx.name_table)

  let lookup (ctx : t) (name : string) : ix * value =
    match lookup_opt ctx name with
    | Some found -> found
    | None -> raise (ElabError (UnboundVariable name))

  (* An open choice (M: open choice): the first candidate open that has
     [name], else the binder it shadows, else the base context. Nothing here
     finds a name by spelling among the locals. *)
  let lookup_choice_opt (ctx : t) (name : string) ({ opens; fallback } : Syntax.open_choice) : (ix * value) option =
    let entry =
      List.find_map
        (fun label -> Option.bind (List.assoc_opt label ctx.opened) (NameMap.find_opt name))
        opens
    in
    match entry, fallback with
    | Some entry, _ -> Some (entry_ix ctx entry)
    | None, Some resolved -> lookup_opt ctx resolved
    | None, None ->
        let base_names = match ctx.base with Some base -> base.name_table | None -> ctx.name_table in
        Option.map (entry_ix ctx) (NameMap.find_opt name base_names)

  (* The entry a path's head names (M12): through its open choice when
     expansion left one, else by the name expansion resolved it to. *)
  let lookup_head_opt (ctx : t) (p : Syntax.path) : (ix * value) option =
    match p.head_choice with
    | Some choice -> lookup_choice_opt ctx p.head.name choice
    | None -> lookup_opt ctx p.head.name

  let lookup_self (ctx : t) : ix * value =
    match ctx.self_entry with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable "self"))

  let lookup_self_type (ctx : t) : value =
    match ctx.self_type with
    | Some ty -> ty
    | None -> raise (ElabError (UnboundVariable "Self"))

  let lookup_resume (ctx : t) : ix * value =
    match ctx.resume_entry with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable "resume"))

  let with_self_type (ctx : t) (ty : value) : t = { ctx with self_type = Some ty }

  let with_loader (ctx : t) (loader : Core_loader.t) : t = { ctx with loader = Some loader }


  let add_trait_evidence (ctx : t) (evidence : trait_evidence) : t =
    { ctx with trait_evidence = evidence :: ctx.trait_evidence }

  let clear_self (ctx : t) : t = { ctx with self_entry = None }
  let clear_self_scope (ctx : t) : t = { ctx with self_entry = None; self_type = None }

  let fresh_meta (ctx : t) : term =
    let id = MetaContext.fresh ctx.metas in
    InsertedMeta (id, ctx.bds)

  let raw_meta (ctx : t) : value =
    VFlex { id = MetaContext.fresh ctx.metas; spine = [] }

  let eval (ctx : t) (t : term) : value = Nbe.eval ctx.metas ctx.env t

  (* Whether a call of a function of type [ty] is known pure: its (first) arrow's
     effect row is empty and closed. An unsolved row is not known pure. *)
  let pure_call (ctx : t) (ty : value) : bool =
    let rec closed_empty (row : effect_row_value) =
      row.effect_values = []
      && match Option.map (Nbe.force ctx.metas) row.tail_value with
         | None -> true
         | Some (VEffectRow row) -> closed_empty row
         | Some _ -> false
    in
    match Nbe.force ctx.metas ty with
    | VPi { effects; _ } -> closed_empty (Nbe.eval_effect_row_closure ctx.metas effects (VRigid { lvl = ctx.lvl; spine = [] }))
    | _ -> false
  (* Running the checked program, not checking it: no evaluation budget. *)
  let run (ctx : t) (t : term) : value = Nbe.run ctx.metas ctx.env t
  let quote (ctx : t) (v : value) : term = Nbe.quote ctx.metas ctx.lvl v

  let unify (ctx : t) (v1 : value) (v2 : value) : unit =
    Eval_budget.request ~demand:"a unification" ctx.metas.budget (fun () -> Unify.unify ctx.metas ctx.env ctx.lvl v1 v2)

  let try_unify (ctx : t) (v1 : value) (v2 : value) : bool =
    Eval_budget.request ~demand:"a unification" ctx.metas.budget (fun () -> Unify.try_unify ctx.metas ctx.env ctx.lvl v1 v2)

  let conv (ctx : t) (v1 : value) (v2 : value) : bool =
    Nbe.conv ctx.metas ctx.lvl v1 v2
end
