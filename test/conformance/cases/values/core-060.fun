# handler value branch
{ effect Exc = sig { raise : I64 -> I64 }; match (41) { x => x + 1, effect Exc.raise n => 0 } }
