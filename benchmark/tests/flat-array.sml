structure Main =
   struct
      fun doit n =
         let
            val v = Vector.tabulate (1000000, fn i => (i, i + 1))
            fun loop n =
               if 0 = n
                  then ()
               else
                  let
                     val sum = Vector.foldl (fn ((a, b), c) =>
                                             a + b + c handle Overflow => 0) 0 v
                  in
                     loop (n - 1)
                  end
         in
            loop n
         end
   end

