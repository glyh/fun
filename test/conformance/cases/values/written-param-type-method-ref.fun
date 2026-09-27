# parameter-type-metas-capture-earlier-parameters
# A type former whose method takes a written Ref(I64) parameter.
{ Box = fn[A : Type] { struct { v : A; pub method get(r : Ref(I64)) : I64 { 3 } } };
  x = ref(40);
  Box[I64]{ v = 1 }.get(x) }
