# parameter-type-metas-capture-earlier-parameters
{ F = fn(b : Bool) { match (b) { True => I64, False => Bool } };
  f = fn(b : Bool, v : F(b)) : F(b) { v };
  f(False, True) }
