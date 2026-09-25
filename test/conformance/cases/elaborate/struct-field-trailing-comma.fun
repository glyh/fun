# a trailing comma in a struct field list is a grammar error, not a reader hang
{ R = struct { f : I64, }; 1 }
