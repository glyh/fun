{ f : Bool -> Type = fn(b) { if (b) { I64 } else { Bool } }; (42 : f(True)) }
