functor F () =
   struct
      val x = y
   end

val y = 13

structure S = F ()
