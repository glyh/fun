# method-signature-metas-capture-self: an implicit argument in a parameter's type
{ S = struct { k : I64; pub method keep(r : Ref(I64)) : Ref(I64) { r } };
  s = S{ k = 2 }; x = ref(40); y = s.keep(x); 7 }
