structure Main =
   struct
      fun once () =
         let
            val count = 1000000000
            open TextIO
            val out = openOut "/dev/null"
            fun loop n =
               if n = 0
                  then ()
               else (output1 (out, #"a"); loop (n - 1))
            val _ = loop count
            val _ = closeOut out
         in
            ()
         end

      fun doit n =
         if n = 0
            then ()
         else (once (); doit (n - 1))
   end
