# a function returning its reference keeps the reference's effect in its row
{ f : Unit -> Ref(I64) = fn(u : Unit) { ref(1) }; 1 }
