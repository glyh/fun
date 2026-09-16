# operator RHS macro path
{
       macro answer_body(_) { Syntax.i64(5) };
       syntax answer { answer => answer_body(0) };
       answer
      }
