# block-local macro usable inside its block
{ R = struct { macro mi(_) { Syntax.i64(7) }; pub h = mi(0) }; R.h }
