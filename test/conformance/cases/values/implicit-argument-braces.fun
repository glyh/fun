# an implicit argument written in braces, the ticket program
{ f = fn[A : Type](x : A) { x }; f{I64}(1) }
