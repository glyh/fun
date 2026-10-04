# decl template captures public module value
{
       M = module {
         syntax keep : Decl { keep $(d : Decl) => { $d } };
         keep pub answer = 42
       };
       M.answer
     }
