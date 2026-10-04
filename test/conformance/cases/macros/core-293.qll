# a template's syntax form is visible in its own output
{ syntax mk { mk => { syntax inc { inc $x => $x + 1 }; inc 41 } }; mk }
