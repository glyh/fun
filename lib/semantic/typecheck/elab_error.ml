type elab_error =
  | UnboundVariable of string
  | ApplyingNonFunction
  | TupleLengthMismatch
  | NotANominalType
  | NotAModule
  | NotASignature of string option
      (** a module used as a type: only a signature value ([sig { … }]) is one *)
  | UnknownConstructor of string
  | PatternArityMismatch
  | PatternBindingMismatch
  | UnknownRecordField of string
  | DuplicateRecordField of string
  | MissingRecordField of string
  | DuplicateEffectOperation of string
  | ExpectedEffect
  | UnsupportedRowUnion of int
  | PolyArrowOutsideSignature
  | RowVariableAmongEffects
  | UnsolvedEffectRow
  | DuplicateEffect
  | DuplicateEffectBranch of string
  | UnknownEffectOperation of string
  | EffectOperationPathExpected
  | UnhandledEffects of string list
  | HandledEffectEscapes of string
  | GenerativeTypeEscapes of string
      (** Effects left where nothing handles them, named. *)
  | NonExhaustive of string
  | InvalidRecursiveRecord of string
  | ImportRequiresLoader of string
  | UnknownTrait of string
  | UnknownTraitMethod of string
  | DuplicateTraitField of string
  | DuplicateTraitBound of string
  | MissingTraitField of string
  | AmbiguousTraitImplementation of string
  | MissingTraitImplementation of string
  | MacroDidNotReturnSyntax of string
  | MacroBinderUnsolved of { macro : string; binder : string }
      (** A macro's type binder is not solved when the macro must run. *)
  | MacroArgumentType of { macro : string; param : string; promised : string; reason : string }
  | MacroOutputType of { macro : string; promised : string; reason : string }
  | QuoteHoleKindConflict of string
  | QuoteNotOneDecl of int
      (** A [quote { … }] where one [Decl] is expected holds another number of
          items, or its one item is a declaration hole (which splices a list). *)
  | EvaluationBudgetExceeded of { limit : int; call : string; demand : string; site : Eval_budget.site option }
  | EvaluationFailed of { message : string; site : Eval_budget.site option }
      (** Evaluating a term while checking failed (a [panic] in a type, a
          negative [Tuple] count): an error in the program, reported where. *)
  | OpenSuppliesRole of string
  | ExportUnknownMember of string
  | ExportClash of string
  | ExportImpls
      (** [export M] of a module with public impls: impls are not re-exported *)
      (** M7: an open supplies a member named like a syntax form, operator or
          macro visible in its region. *)
  | FieldTypeMentionsMethod of { field : string; method_ : string }
      (** A field's type mentions a method written before it: the method needs
          every field (its [self]), so the field cannot wait for it. *)

exception ElabError of elab_error

let string_of_elab_error = function
  | UnboundVariable n -> "UnboundVariable \"" ^ n ^ "\""
  | ApplyingNonFunction -> "ApplyingNonFunction"
  | TupleLengthMismatch -> "TupleLengthMismatch"
  | NotANominalType -> "NotANominalType"
  | NotAModule -> "NotAModule"
  | NotASignature name -> "NotASignature " ^ (match name with Some n -> "\"" ^ n ^ "\" (a module, not a sig { … })" | None -> "(a module, not a sig { … })")
  | UnknownConstructor n -> "UnknownConstructor \"" ^ n ^ "\""
  | PatternArityMismatch -> "PatternArityMismatch"
  | PatternBindingMismatch -> "PatternBindingMismatch"
  | UnknownRecordField n -> "UnknownRecordField \"" ^ n ^ "\""
  | DuplicateRecordField n -> "DuplicateRecordField \"" ^ n ^ "\""
  | MissingRecordField n -> "MissingRecordField \"" ^ n ^ "\""
  | DuplicateEffectOperation n -> "DuplicateEffectOperation \"" ^ n ^ "\""
  | ExpectedEffect -> "ExpectedEffect"
  | UnsupportedRowUnion n -> Printf.sprintf "UnsupportedRowUnion %d: a row holds one row variable; a union of %d is not supported yet" n n
  | PolyArrowOutsideSignature -> "PolyArrowOutsideSignature: ~> is read where it sits in a signature (a parameter or result position)"
  | RowVariableAmongEffects -> "RowVariableAmongEffects: a row variable next to effects is the row's tail; write {Log | e}"
  | UnsolvedEffectRow -> "UnsolvedEffectRow: can't infer the effects of a ->{_} arrow; write ->{…} or a pure ->"
  | DuplicateEffect -> "DuplicateEffect"
  | DuplicateEffectBranch n -> "DuplicateEffectBranch \"" ^ n ^ "\""
  | UnknownEffectOperation n -> "UnknownEffectOperation \"" ^ n ^ "\""
  | EffectOperationPathExpected -> "EffectOperationPathExpected"
  | UnhandledEffects names -> "UnhandledEffects \"" ^ String.concat ", " names ^ "\""
  | HandledEffectEscapes name -> "HandledEffectEscapes \"" ^ name ^ "\""
  | GenerativeTypeEscapes name -> "GenerativeTypeEscapes \"" ^ name ^ "\""
  | NonExhaustive msg -> "NonExhaustive \"" ^ msg ^ "\""
  | InvalidRecursiveRecord msg -> "InvalidRecursiveRecord \"" ^ msg ^ "\""
  | ImportRequiresLoader path -> "ImportRequiresLoader \"" ^ path ^ "\""
  | UnknownTrait n -> "UnknownTrait \"" ^ n ^ "\""
  | UnknownTraitMethod n -> "UnknownTraitMethod \"" ^ n ^ "\""
  | DuplicateTraitField n -> "DuplicateTraitField \"" ^ n ^ "\""
  | DuplicateTraitBound n -> "DuplicateTraitBound \"" ^ n ^ "\""
  | MissingTraitField n -> "MissingTraitField \"" ^ n ^ "\""
  | AmbiguousTraitImplementation n -> "AmbiguousTraitImplementation \"" ^ n ^ "\""
  | MissingTraitImplementation n -> "MissingTraitImplementation \"" ^ n ^ "\""
  | MacroDidNotReturnSyntax n -> "MacroDidNotReturnSyntax \"" ^ n ^ "\""
  | MacroBinderUnsolved { macro; binder } -> Printf.sprintf "MacroBinderUnsolved \"cannot infer %s for `%s`\"" binder macro
  | MacroArgumentType { macro; param; promised; reason } ->
      Printf.sprintf "MacroArgumentType \"argument %s of `%s` expects Expr(%s): %s\"" param macro promised reason
  | MacroOutputType { macro; promised; reason } ->
      Printf.sprintf "MacroOutputType \"macro `%s` promises Expr(%s), its output does not have that type: %s\"" macro promised reason
  | QuoteHoleKindConflict n -> "QuoteHoleKindConflict \"" ^ n ^ "\""
  | QuoteNotOneDecl n ->
      Printf.sprintf "QuoteNotOneDecl \"a Decl is one declaration, this quote holds %d; annotate the macro : List(Decl)\"" n
  | EvaluationBudgetExceeded { limit; call; demand; site } ->
      "EvaluationBudgetExceeded \"" ^ Eval_budget.message ~limit ~call ~demand ~site ^ "\""
  | EvaluationFailed { message; site } ->
      "EvaluationFailed \"" ^ message ^ Eval_budget.where site ^ " (while type checking)\""
  | OpenSuppliesRole n -> "OpenSuppliesRole \"" ^ n ^ "\""
  | ExportUnknownMember n -> "ExportUnknownMember \"" ^ n ^ "\""
  | ExportClash n -> "ExportClash \"" ^ n ^ "\""
  | ExportImpls -> "ExportImpls"
  | FieldTypeMentionsMethod { field; method_ } ->
      Printf.sprintf "field %s's type mentions method %s, which needs every field: a cycle" field method_

let () =
  Printexc.register_printer (function
    | ElabError e -> Some (Printf.sprintf "ElabError(%s)" (string_of_elab_error e))
    | _ -> None)
