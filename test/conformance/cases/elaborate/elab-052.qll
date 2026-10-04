{ f : Bool -> Type = fn(b) { if (b) { I64 } else { Bool } }; (True : f(False)) }
