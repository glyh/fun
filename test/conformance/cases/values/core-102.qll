# handler resumed continuation is deep
{ effect Ping = sig { hit : I64 -> I64 };
     match (if (perform Ping.hit(1) == 41) { perform Ping.hit(2) } else { 0 }) { x => x,
     effect Ping.hit n => resume(n + 40)
     }
     }
