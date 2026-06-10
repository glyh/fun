open Core
open Elab_error
open Elab_common

module Ctx = struct
  type t = {
    env : env;
    types : value list;
    lvl : lvl;
    metas : MetaContext.t;
    bds : bd list;
    name_table : name_entry NameMap.t;
    traits : trait_info NameMap.t;
    trait_evidence : trait_evidence list;
    self_entry : name_entry option;
    self_type : value option;
    resume_entry : name_entry option;
    loader : Core_loader.t option;
  }

  let empty () : t =
    let metas = MetaContext.create () in
    {
      env = [];
      types = [];
      lvl = 0;
      metas;
      bds = [];
      name_table = NameMap.empty;
      traits = NameMap.empty;
      trait_evidence = [];
      self_entry = None;
      self_type = None;
      resume_entry = None;
      loader = None;
    }

  let bind (ctx : t) (name : string) (ty : value) : t =
    let var = VRigid { lvl = ctx.lvl; spine = [] } in
    {
      env = var :: ctx.env;
      types = ty :: ctx.types;
      lvl = ctx.lvl + 1;
      metas = ctx.metas;
      bds = Bound :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table;
      traits = ctx.traits;
      trait_evidence = ctx.trait_evidence;
      self_entry = ctx.self_entry;
      self_type = ctx.self_type;
      resume_entry = ctx.resume_entry;
      loader = ctx.loader;
    }

  let bind_anonymous (ctx : t) (ty : value) : t * name_entry =
    let entry = { level = ctx.lvl; ty } in
    let var = VRigid { lvl = ctx.lvl; spine = [] } in
    ({
       env = var :: ctx.env;
       types = ty :: ctx.types;
       lvl = ctx.lvl + 1;
       metas = ctx.metas;
       bds = Bound :: ctx.bds;
       name_table = ctx.name_table;
       traits = ctx.traits;
       trait_evidence = ctx.trait_evidence;
       self_entry = ctx.self_entry;
       self_type = ctx.self_type;
       resume_entry = ctx.resume_entry;
       loader = ctx.loader;
     },
     entry)

  let define (ctx : t) (name : string) (ty : value) (v : value) : t =
    {
      env = v :: ctx.env;
      types = ty :: ctx.types;
      lvl = ctx.lvl + 1;
      metas = ctx.metas;
      bds = Defined :: ctx.bds;
      name_table = NameMap.add name { level = ctx.lvl; ty } ctx.name_table;
      traits = ctx.traits;
      trait_evidence = ctx.trait_evidence;
      self_entry = ctx.self_entry;
      self_type = ctx.self_type;
      resume_entry = ctx.resume_entry;
      loader = ctx.loader;
    }

  let hide_names (ctx : t) names : t =
    {
      ctx with
      name_table = List.fold_left (fun table name -> NameMap.remove name table) ctx.name_table names;
    }

  let define_anonymous (ctx : t) (ty : value) (v : value) : t * name_entry =
    let entry = { level = ctx.lvl; ty } in
    ({
       env = v :: ctx.env;
       types = ty :: ctx.types;
       lvl = ctx.lvl + 1;
       metas = ctx.metas;
       bds = Defined :: ctx.bds;
       name_table = ctx.name_table;
       traits = ctx.traits;
       trait_evidence = ctx.trait_evidence;
       self_entry = ctx.self_entry;
       self_type = ctx.self_type;
       resume_entry = ctx.resume_entry;
       loader = ctx.loader;
     },
     entry)

  let lookup (ctx : t) (name : string) : ix * value =
    match NameMap.find_opt name ctx.name_table with
    | Some { level; ty } -> (Nbe.lvl_to_ix ctx.lvl level, ty)
    | None -> raise (ElabError (UnboundVariable name))

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

  let add_trait (ctx : t) (trait_info : trait_info) : t =
    { ctx with traits = NameMap.add trait_info.trait_name trait_info ctx.traits }

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
  let quote (ctx : t) (v : value) : term = Nbe.quote ctx.metas ctx.lvl v

  let unify (ctx : t) (v1 : value) (v2 : value) : unit =
    Unify.unify ctx.metas ctx.env ctx.lvl v1 v2

  let try_unify (ctx : t) (v1 : value) (v2 : value) : bool =
    Unify.try_unify ctx.metas ctx.env ctx.lvl v1 v2

  let conv (ctx : t) (v1 : value) (v2 : value) : bool =
    Nbe.conv ctx.metas ctx.lvl v1 v2
end
