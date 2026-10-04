# identifier hole can reference use-site name
{
       x = 4;
       syntax use { use $(name : Id) => $name };
       use x
     }
