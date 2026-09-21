# opening a value whose type is not yet known is not opening a module
{ f = fn(x) { open x; 1 }; 1 }
