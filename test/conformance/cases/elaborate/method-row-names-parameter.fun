# a method's effect row names one of its own parameters (small-followups item 2)
{ S = struct { k : I64; pub method bump(r : Ref(I64)) ->{Mutate(r)} I64 { r <- deref(r) + self.k; deref(r) } }; 1 }
