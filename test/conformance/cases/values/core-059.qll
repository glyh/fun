# a perform after resume is handled again, value branch once
{ effect E = sig { op : I64 -> I64 }; match (perform E.op(1) + perform E.op(2)) { x => x + 1000, effect E.op n => resume(n + 50) } }
