# a meta applied to a spine, solved to a type mentioning its own binders
{ id = fn[A : Type](a : A) { a }; f = fn(x : I64) { id(fn(T : Type, t : T) { t }) }; 1 }
