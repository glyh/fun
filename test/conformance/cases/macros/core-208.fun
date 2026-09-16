# the caller's own binding is untouched
{ False = 42; _ = (1 > 2) && (2 > 1); False }
