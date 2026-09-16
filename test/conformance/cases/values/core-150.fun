# method uses Self type
{ Box = fn[A : Type] { struct { value: A; pub method id(other : Self) { other.value } } }; Box[I64].id(Box[I64]{value = 1})(Box[I64]{value = 2}) }
