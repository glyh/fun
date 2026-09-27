# parameter-type-metas-capture-earlier-parameters
# The soundness row: True is not an I64, so this must stay refused.
{ F = fn(b : Bool) { match (b) { True => I64, False => Bool } };
  f = fn(b : Bool, v : F(b)) : F(b) { v };
  f(True, True) }
