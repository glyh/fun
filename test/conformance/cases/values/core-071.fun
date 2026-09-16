# handler record payload binding order
{
       Request = struct { value: I64; extra: I64; };
       effect Ask = sig { prompt : Request -> I64 };
       match (perform Ask.prompt(Request{value = 40; extra = 2})) { x => x,
       effect Ask.prompt Request{value; extra} => value - extra
       }
     }
