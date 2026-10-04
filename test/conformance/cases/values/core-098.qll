# handler same match branch effect
{ effect Ping = sig { hit : I64 -> I64 };
     match (perform Ping.hit(1)) { x => x,
     effect Ping.hit n =>
         if (n == 1) {
           { y = perform Ping.hit(42); y + 1 }
          } else {
            resume(n)
          }
     }
     }
