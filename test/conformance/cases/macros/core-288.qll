# decl template captures public struct value
{
       Box = struct {
         value: I64;
         syntax keep : Decl { keep $(d : Decl) => { $d } };
         keep pub answer = 42
       };
       Box.answer
     }
