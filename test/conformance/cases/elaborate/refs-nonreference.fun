# a reference operation on a value that is not a reference is refused
{ fn() { deref(5) }; 1 }
