# let-bound sig
{ Sig = sig { x : I64 }; f = fn(m : Sig) { m.x + 1 }; f(module { pub x = 41 }) }
