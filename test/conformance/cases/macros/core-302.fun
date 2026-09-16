# a body inside a macro argument reads generated syntax
{
       syntax make_inc : Decl { make_inc $(n : Id) => { syntax $n { $n $x => $x + 1 } } };
       macro twice(e) { quote($e + $e) };
       run = fn(f) { f(()) };
       twice(run(fn(_) { make_inc inc; inc 5 }))
     }
