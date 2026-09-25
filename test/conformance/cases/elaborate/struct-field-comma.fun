# a comma between struct fields is a grammar error, not a reader hang
{ R = struct { f : I64, g : I64 }; 1 }
