type elab_error =
  | UnboundVariable of string
  | ApplyingNonFunction
  | TupleLengthMismatch
  | NotANominalType
  | UnknownConstructor of string
  | PatternArityMismatch
  | PatternBindingMismatch
  | UnknownRecordField of string
  | DuplicateRecordField of string
  | MissingRecordField of string
  | DuplicateEffectOperation of string
  | ExpectedEffect
  | DuplicateEffect
  | DuplicateEffectBranch of string
  | UnknownEffectOperation of string
  | EffectOperationPathExpected
  | UnhandledEffects
  | NonExhaustive of string
  | InvalidRecursiveRecord of string
  | ImportRequiresLoader of string
  | UnknownTrait of string
  | UnknownTraitMethod of string
  | DuplicateTraitField of string
  | MissingTraitField of string
  | AmbiguousTraitImplementation of string

exception ElabError of elab_error

let string_of_elab_error = function
  | UnboundVariable n -> "UnboundVariable \"" ^ n ^ "\""
  | ApplyingNonFunction -> "ApplyingNonFunction"
  | TupleLengthMismatch -> "TupleLengthMismatch"
  | NotANominalType -> "NotANominalType"
  | UnknownConstructor n -> "UnknownConstructor \"" ^ n ^ "\""
  | PatternArityMismatch -> "PatternArityMismatch"
  | PatternBindingMismatch -> "PatternBindingMismatch"
  | UnknownRecordField n -> "UnknownRecordField \"" ^ n ^ "\""
  | DuplicateRecordField n -> "DuplicateRecordField \"" ^ n ^ "\""
  | MissingRecordField n -> "MissingRecordField \"" ^ n ^ "\""
  | DuplicateEffectOperation n -> "DuplicateEffectOperation \"" ^ n ^ "\""
  | ExpectedEffect -> "ExpectedEffect"
  | DuplicateEffect -> "DuplicateEffect"
  | DuplicateEffectBranch n -> "DuplicateEffectBranch \"" ^ n ^ "\""
  | UnknownEffectOperation n -> "UnknownEffectOperation \"" ^ n ^ "\""
  | EffectOperationPathExpected -> "EffectOperationPathExpected"
  | UnhandledEffects -> "UnhandledEffects"
  | NonExhaustive msg -> "NonExhaustive \"" ^ msg ^ "\""
  | InvalidRecursiveRecord msg -> "InvalidRecursiveRecord \"" ^ msg ^ "\""
  | ImportRequiresLoader path -> "ImportRequiresLoader \"" ^ path ^ "\""
  | UnknownTrait n -> "UnknownTrait \"" ^ n ^ "\""
  | UnknownTraitMethod n -> "UnknownTraitMethod \"" ^ n ^ "\""
  | DuplicateTraitField n -> "DuplicateTraitField \"" ^ n ^ "\""
  | MissingTraitField n -> "MissingTraitField \"" ^ n ^ "\""
  | AmbiguousTraitImplementation n -> "AmbiguousTraitImplementation \"" ^ n ^ "\""

let () =
  Printexc.register_printer (function
    | ElabError e -> Some (Printf.sprintf "ElabError(%s)" (string_of_elab_error e))
    | _ -> None)
