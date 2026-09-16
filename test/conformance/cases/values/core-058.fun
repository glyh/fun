# an unresumed handler skips the value branch
{ effect E = sig { op : I64 -> I64 }; match (perform E.op(1)) { x => x + 5, effect E.op n => n + 40 } }
