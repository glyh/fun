# Syntax let builder
{
       macro answer(_) { quote({ x = 3; x + 4 }) };
       answer(0)
      }
