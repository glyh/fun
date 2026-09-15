(* The evaluation budget: how many calls the checker may spend on one
   evaluation. An evaluation is one request from the checker to the evaluator,
   however it re-enters itself; the budget is refilled when a request starts
   and spent by every function call made before it returns, a fixpoint
   unfolded on an unknown variable included. Running a
   program is not checking: it is a request with no limit. Termination is never
   checked, so a divergent evaluation is a budget error, not a hang.
   See docs/wayfinder/tickets/checker-evaluation-budget.md. *)

(* Where the checker was when it asked for an evaluation: the source form it was
   elaborating. Only this error reads it; elaborator errors in general carry no
   location yet. *)
type site = { span : Source_span.t; mode : string }

(* An overrun names the fixpoint it was calling, the checker request that
   demanded the evaluation, and the form being elaborated. *)
exception Exceeded of { limit : int; call : string; demand : string; site : site option }

(* The innermost macro application running under the budget, as the errors it
   raises: an overrun, and any other evaluation failure. Each builds that
   application's own error, carrying its site, so an error names where it
   happened at the point it is raised and nothing re-catches it. *)
type 'value application = {
  exceeded : limit:int -> call:string -> exn;
  failed : string -> exn;
  (* [expand_block]: expand a reflected block where the application runs (M9). *)
  expand : 'value -> 'value;
}

(* [limit = None] while running a program. [application = None] outside any
   macro application: an overrun is the checker's [Exceeded], and a failure is
   the evaluator's own error. *)
type 'value t = {
  mutable limit : int option;
  mutable remaining : int;
  mutable depth : int;
  mutable application : 'value application option;
  (* The outermost checker request in progress, e.g. "a conversion". *)
  mutable demand : string option;
  (* The innermost source form the elaborator is at. *)
  mutable site : site option;
  (* The fixpoint most recently unfolded: a divergent evaluation keeps unfolding
     it, so it names the call an overrun is in. *)
  mutable calling : string option;
}

(* No surface syntax raises it yet; the ticket leaves that open. *)
let default_limit = 1_000_000

let create () = { limit = Some default_limit; remaining = default_limit; depth = 0; application = None; demand = None; site = None; calling = None }

(* [demand] names the request, unless an enclosing one already does. *)
let demanding budget demand f =
  match budget.demand with
  | Some _ -> f ()
  | None ->
      budget.demand <- Some demand;
      Fun.protect ~finally:(fun () -> budget.demand <- None) f

(* The elaborator is at [site] while [f] runs; a synthetic span keeps the
   enclosing site. *)
let at budget (site : site) f =
  if site.span.Source_span.synthetic then f ()
  else
    let outer = budget.site in
    budget.site <- Some site;
    Fun.protect ~finally:(fun () -> budget.site <- outer) f

let start ~limit ~demand budget f =
  if budget.depth = 0 then begin
    budget.limit <- limit;
    budget.remaining <- Option.value limit ~default:0;
    budget.calling <- None
  end;
  budget.depth <- budget.depth + 1;
  Fun.protect ~finally:(fun () -> budget.depth <- budget.depth - 1) (fun () -> demanding budget demand f)

let request ~demand budget f = start ~limit:(Some default_limit) ~demand budget f
let run budget f = start ~limit:None ~demand:"running a program" budget f

(* [call] describes the callee; it is only built when the budget runs out. *)
let spend budget ~call =
  match budget.limit with
  | None -> ()
  | Some limit ->
      if budget.remaining <= 0 then raise
          (match budget.application with
           | Some app -> app.exceeded ~limit ~call:(call ())
           | None -> Exceeded { limit; call = call (); demand = Option.value budget.demand ~default:"an evaluation"; site = budget.site });
      budget.remaining <- budget.remaining - 1

(* A macro application is a call (M5): it spends one unit, and its body and the
   expansion of its output spend from the same request. So a nest of
   applications is bounded as a whole, breadth included. An overrun while it
   runs, or any evaluation failure, is [application]'s error. *)
let macro_application budget ~call ~application f =
  request ~demand:"a macro application" budget (fun () ->
      let outer = budget.application in
      budget.application <- Some application;
      Fun.protect ~finally:(fun () -> budget.application <- outer) (fun () ->
          spend budget ~call:(fun () -> call);
          f ()))

(* A resolved name ([loop#3]) as written. *)
let written name = match String.index_opt name '#' with Some i -> String.sub name 0 i | None -> name

let message ~limit ~call ~demand ~site =
  let where =
    match site with
    | Some { span; mode } -> Format.asprintf " while %s at %a" mode Source_span.pp span
    | None -> ""
  in
  Printf.sprintf
    "evaluation exceeded the budget of %d calls while type checking: calling %s, in %s%s (the budget cannot yet be raised from source)"
    limit (written call) demand where

let () =
  Printexc.register_printer (function
    | Exceeded { limit; call; demand; site } -> Some (message ~limit ~call ~demand ~site)
    | _ -> None)
