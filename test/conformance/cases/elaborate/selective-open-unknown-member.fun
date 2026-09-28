# an unknown name errors in the export form's shape
{ M = module { pub x = 1 }; open M.{nope}; x }
