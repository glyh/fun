# method-signature-metas-capture-self: the same, with a declared row naming the parameter
{ S = struct { k : I64; pub method bump(r : Ref(I64)) ->{Mutate(r)} I64 { deref(r) } };
  s = S{ k = 2 }; x = ref(40); s.bump(x) }
