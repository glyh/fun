# Syntax char/unit builders
{
       macro char_a(_) { Syntax.char('a') };
       macro unit_value(_) { Syntax.unit(()) };
       if (char_a(0) == 'a') {
         if (unit_value(0) == ()) { 42 } else { 0 }
       } else { 0 }
      }
