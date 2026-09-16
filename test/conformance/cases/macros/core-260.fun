# a prelude group through an import binder
{ Std = import "std"; order tight : stronger_than(Std.multiplicative); infix (<~>) tight ($a, $b) { $a + $b }; 2 * 3 <~> 4 }
