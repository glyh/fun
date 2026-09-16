# handler tuple payload binding order
{ effect Console = sig { log : Tuple(2, I64, I64) -> I64 }; match (perform Console.log((40, 2))) { x => x, effect Console.log (level, message) => level - message } }
