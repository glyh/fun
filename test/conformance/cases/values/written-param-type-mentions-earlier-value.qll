# parameter-type-metas-capture-earlier-parameters
# F(b) is a type depending on the earlier parameter's VALUE; the written Ref(F(b))
# must still see it.
{ F = fn(b : Bool) { match (b) { True => I64, False => Bool } };
  f = fn(b : Bool, r : Ref(F(b))) : I64 { 0 };
  x = ref(1);
  f(True, x) }
