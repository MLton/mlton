
structure Main =
struct
   open CML

   val print = TextIO.print

   fun doit' n =
      RunCML.doit
      (fn () =>
       let
          fun make m () =
             (print (concat ["make: ", Int.toString m, " ",
                             tidToString (getTid ()), "\n"])
              ; sync (timeOutEvt (Time.fromSeconds (Int.toLarge m)))
              ; print (concat ["finish: ", Int.toString m, " ",
                               tidToString (getTid ()), "\n"]))
          fun loop m =
             if m <= 0
                then ()
                else let
                        val _ = spawn (make m)
                     in
                        loop (m - 10)
                     end
       in
          loop n
       end,
       SOME (Time.fromMilliseconds 10))

   fun doit n =
      let
         val x = doit' n
      in
         x
      end
end
