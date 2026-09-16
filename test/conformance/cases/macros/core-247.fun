# an ungrouped form's trailing hole reads a whole expression
{ syntax inc { inc $x => $x + 1 }; inc 1 * 10 }
