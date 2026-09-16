# an imported unit's role does not leak past the region of the open that imported it
{ x = { open (import "ops"); answer }; (x, answer).0 }
