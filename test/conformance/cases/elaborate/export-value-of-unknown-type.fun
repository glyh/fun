# exporting a value whose type is not yet known is not exporting a module
{ f = fn(x) { module { export x } }; 1 }
