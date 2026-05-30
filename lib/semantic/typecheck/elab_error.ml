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
