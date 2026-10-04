# definition-site ref ignores nested use-site shadows
{
       z = 3;
       syntax get_z { get_z => z };
       {
         z = 4;
         {
           z = 5;
           get_z
         }
       }
     }
