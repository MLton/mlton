structure Main =
   struct
      val alpha = CharVector.tabulate (26, fn i => chr (ord #"A" + i))
      fun doit n =
         let
            val len = 2017
            val s = CharVector.tabulate (len, fn i =>
                                         String.sub (alpha, i mod 26))
            fun loop n =
               if n < 0
                  then ()
               else
                  if 468705 = CharVector.foldl (fn (c, s) => s + ord c) 0 (String.concat [s, s, s])
                     then loop (n - 1)
                  else raise Fail "bug"
         in loop (n * 10000)
         end
   end
