# an imported unit's role is read in the region of the open that imported it
{ x = { open (import "ops"); answer }; x }
