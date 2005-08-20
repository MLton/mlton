fun loop1 x =
   if x = 0
      then ()
   else (let
            fun loop2 y =
               if y = 0
                  then ()
               else loop2 (y - 1)
         in loop2 x
         end;
         loop1 (x - 1))

val _ = loop1 13
