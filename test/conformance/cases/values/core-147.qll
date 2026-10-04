# method uses self
{ Box = fn[A : Type] { struct { value: A; pub method get() { self.value } } }; Box[I64].get(Box[I64]{value = 1}) }
