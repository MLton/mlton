structure S:
   sig
      val f: 'a -> 'a list
   end =
   struct
      fun f x =
         if x = x
            then []
         else [x]
   end
