fun plus (a, b) = a
   
functor F () =
   struct
      val _ = plus (plus (1, 2), 3)
   end

infix plus

structure S = F ()
