structure TextIO =
   struct
      open TextIO
      fun print s =
         MLton.Thread.atomically
         (fn () => TextIO.print s)
   end

val print = TextIO.print
