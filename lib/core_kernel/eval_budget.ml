(* The evaluation budget: how many calls the checker may spend on one
   evaluation. An evaluation is one request from the checker to the evaluator,
   however it re-enters itself; the budget is refilled when a request starts
   and spent by every function call made before it returns. Only closed calls
   evaluate, so a call stuck on an unknown variable costs nothing. Running a
   program is not checking: it is a request with no limit. Termination is never
   checked, so a divergent evaluation is a budget error, not a hang.
   See docs/wayfinder/tickets/checker-evaluation-budget.md. *)

exception Exceeded of { limit : int; call : string }

(* The innermost macro application running under the budget, as the errors it
   raises: an overrun, and any other evaluation failure. Each builds that
   application's own error, carrying its site, so an error names where it
   happened at the point it is raised and nothing re-catches it. *)
type application = {
  exceeded : limit:int -> call:string -> exn;
  failed : string -> exn;
}

(* [limit = None] while running a program. [application = None] outside any
   macro application: an overrun is the checker's [Exceeded], and a failure is
   the evaluator's own error. *)
type t = {
  mutable limit : int option;
  mutable remaining : int;
  mutable depth : int;
  mutable application : application option;
}

(* No surface syntax raises it yet; the ticket leaves that open. *)
let default_limit = 1_000_000

let create () = { limit = Some default_limit; remaining = default_limit; depth = 0; application = None }

let start ~limit budget f =
  if budget.depth = 0 then begin
    budget.limit <- limit;
    budget.remaining <- Option.value limit ~default:0
  end;
  budget.depth <- budget.depth + 1;
  Fun.protect ~finally:(fun () -> budget.depth <- budget.depth - 1) f

let request budget f = start ~limit:(Some default_limit) budget f
let run budget f = start ~limit:None budget f
let checking budget = Option.is_some budget.limit

(* [call] describes the callee; it is only built when the budget runs out. *)
let spend budget ~call =
  match budget.limit with
  | None -> ()
  | Some limit ->
      if budget.remaining <= 0 then raise
          (match budget.application with
           | Some app -> app.exceeded ~limit ~call:(call ())
           | None -> Exceeded { limit; call = call () });
      budget.remaining <- budget.remaining - 1

(* A macro application is a call (M5): it spends one unit, and its body and the
   expansion of its output spend from the same request. So a nest of
   applications is bounded as a whole, breadth included. An overrun while it
   runs, or any evaluation failure, is [application]'s error. *)
let macro_application budget ~call ~application f =
  request budget (fun () ->
      let outer = budget.application in
      budget.application <- Some application;
      Fun.protect ~finally:(fun () -> budget.application <- outer) (fun () ->
          spend budget ~call:(fun () -> call);
          f ()))

let () =
  Printexc.register_printer (function
    | Exceeded { limit; call } ->
        Some
          (Printf.sprintf
             "evaluation exceeded the budget of %d calls while type checking, calling %s \
              (the budget cannot yet be raised from source)"
             limit call)
    | _ -> None)
