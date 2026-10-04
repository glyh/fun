# a user syntax form shadows type
{ syntax type : Decl { type $(n : Id) => { $n = 7 } }; type x; x }
