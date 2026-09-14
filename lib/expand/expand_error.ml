(* What can go wrong applying a macro (M5, M8): each failure is a value naming
   the macro, raised once at the application site and never caught to decide
   what expansion does next. Budget exhaustion inside an application is
   [BudgetExceeded], and any other evaluation failure [EvalFailed], both raised
   where they happen with the application's site (see [Eval_budget.application]). *)

type t =
  | KindMismatch of { macro : string; kind : Syntax.MacroKind.t; position : Syntax.MacroKind.t }
  | NotSyntax of { macro : string; got : string }
  | NotDeclarations of { macro : string }
  | ExpandedDuringDefinition of { macro : string }
  | MissingCallback of { callback : string }
  | BudgetExceeded of { macro : string; limit : int; call : string }
  | EvalFailed of { macro : string; message : string }
  | RoleConflict of { name : string; span : Source_span.t }
      (** M7: a binder of [name] where a syntax form, operator or macro of that
          name is visible, or the reverse. *)

(* A syntax operator's use and declaration, when the application came from one. *)
type site = { operator : string; use_span : Source_span.t; declaration_span : Source_span.t }

exception Error of { error : t; site : site option }

let message = function
  | KindMismatch { macro; kind; position } ->
      Printf.sprintf "macro '%s' has kind %s but was used in %s context" macro
        (Syntax.MacroKind.to_string kind) (Syntax.MacroKind.to_string position)
  | NotSyntax { macro; got } -> Printf.sprintf "macro '%s' did not return a syntax value, got %s" macro got
  | NotDeclarations { macro } -> Printf.sprintf "decl macro '%s' did not return declarations" macro
  | ExpandedDuringDefinition { macro } -> Printf.sprintf "macro '%s' cannot be expanded during its own definition" macro
  | MissingCallback { callback } -> Printf.sprintf "macro expansion requires the %s callback in the expand context" callback
  | BudgetExceeded { macro; limit; call } ->
      Printf.sprintf "expanding macro '%s' exceeded the evaluation budget of %d calls, calling %s \
                      (the budget cannot yet be raised from source)" macro limit call
  | EvalFailed { macro; message } -> Printf.sprintf "expanding macro '%s' failed: %s" macro message
  | RoleConflict { name; span } ->
      Printf.sprintf "'%s' at %s is bound both as a syntax form, operator or macro and as another binder \
                      where both are visible" name (Format.asprintf "%a" Source_span.pp span)

let site_prefix { operator; use_span; declaration_span } =
  Printf.sprintf "syntax operator %S used at %s, declared at %s: " operator
    (Format.asprintf "%a" Source_span.pp use_span)
    (Format.asprintf "%a" Source_span.pp declaration_span)

let to_string error site = Option.fold ~none:"" ~some:site_prefix site ^ message error

let raise_at ?site error = raise (Error { error; site })

let () =
  Printexc.register_printer (function
    | Error { error; site } -> Some (to_string error site)
    | _ -> None)
