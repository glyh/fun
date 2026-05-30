open Core

let expl_of_surface = function Surface.Explicit -> Explicit | Surface.Implicit -> Implicit

module NameMap = Map.Make (String)

let trait_id_counter = ref 0
let fresh_trait_id () =
  let id = !trait_id_counter in
  incr trait_id_counter;
  id

type name_entry = { level : lvl; ty : value }

type trait_info = {
  trait_id : int;
  trait_name : string;
  trait_params : string list;
  trait_fields : (string * closure) list;
}

type trait_evidence = {
  evidence_trait_id : int;
  evidence_trait_name : string;
  evidence_args : value list;
  evidence_level : lvl;
  evidence_ty : value;
}

let trait_registry : (int, trait_info) Hashtbl.t = Hashtbl.create 16

let pure_effects = effect_row_closure [] empty_effect_row
let ( ^-> ) = fun lhs rhs -> VPi { explicitness = Explicit; domain = lhs; effects = pure_effects; codomain = { env = []; body = rhs } }
let ( ^=> ) = fun lhs rhs -> VPi { explicitness = Implicit; domain = lhs; effects = pure_effects; codomain = { env = []; body = rhs } }
let ( ^->> ) = fun lhs rhs -> Pi { explicitness = Explicit; domain = lhs; effects = empty_effect_row; codomain = rhs }
let ( ^=>> ) = fun lhs rhs -> Pi { explicitness = Implicit; domain = lhs; effects = empty_effect_row; codomain = rhs }
