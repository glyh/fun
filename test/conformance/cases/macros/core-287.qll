# decl template preserves typed value
{
       M = module {
         syntax keep : Decl { keep $(d : Decl) => { $d } };
         keep pub answer : I64 = 42
       };
       M.answer
     }
