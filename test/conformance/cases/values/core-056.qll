# handler resumes once
{ effect Exc = sig { raise : I64 -> I64 }; match (perform Exc.raise(1)) { x => x, effect Exc.raise n => resume(n + 1) } }
