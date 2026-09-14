(* The evaluation budget: how many calls the checker may spend on one
   evaluation. An evaluation is one request from the checker to the evaluator,
   however it re-enters itself; the budget is refilled when a request starts
   and spent by every function call made before it returns. Only closed calls
   evaluate, so a call stuck on an unknown variable costs nothing. Running a
   program is not checking: it is a request with no limit. Termination is never
   checked, so a divergent evaluation is a budget error, not a hang.
   See docs/wayfinder/tickets/checker-evaluation-budget.md. *)

exception Exceeded of { limit : int; call : string }

(* [limit = None] while running a program. [exceeded] is the error an overrun
   raises: the checker's [Exceeded], or, inside a macro application, that
   application's error, which carries its site - so the error names where it
   happened at the point it is raised, and nothing re-catches it. *)
type t = {
  mutable limit : int option;
  mutable remaining : int;
  mutable depth : int;
  mutable exceeded : limit:int -> call:string -> exn;
}

(* No surface syntax raises it yet; the ticket leaves that open. *)
let default_limit = 1_000_000

let checker_exceeded ~limit ~call = Exceeded { limit; call }

let create () =
  { limit = Some default_limit; remaining = default_limit; depth = 0; exceeded = checker_exceeded }

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
      if budget.remaining <= 0 then raise (budget.exceeded ~limit ~call:(call ()));
      budget.remaining <- budget.remaining - 1

(* A macro application is a call (M5): it spends one unit, and its body and the
   expansion of its output spend from the same request. So a nest of
   applications is bounded as a whole, breadth included. An overrun while it
   runs raises [exceeded], the innermost application's error. *)
let macro_application budget ~call ~exceeded f =
  request budget (fun () ->
      let outer = budget.exceeded in
      budget.exceeded <- exceeded;
      Fun.protect ~finally:(fun () -> budget.exceeded <- outer) (fun () ->
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
