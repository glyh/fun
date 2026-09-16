# operator macro discards perform operand before elaboration
{
       syntax discard { discard $x => 7 };
       discard perform Missing.get(())
      }
