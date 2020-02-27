structure Main =
   struct
      fun doit n =
         let
            val v = Vector.tabulate (1000000, fn i =>
                                     (Word14.fromInt i,
                                      Word10.fromInt (i + 1),
                                      Word8.fromInt (i + 2)))
            fun loop n =
               if 0 = n
                  then ()
               else
                  let
                     val sum = Vector.foldl (fn ((a, b, c), d) =>
                                             Word14.toLarge a +
                                             Word10.toLarge b +
                                             Word8.toLarge c + d) 0wx0 v
                     val _ = if 0wx20E0F3760 <> sum
                                then raise Fail (LargeWord.toString sum)
                                else ()
                  in
                     loop (n - 1)
                  end
         in
            loop n
         end
   end
val _ = Main.doit 10
