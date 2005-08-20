signature CRITICAL =
   sig
      val atomicBegin : unit -> unit
      val atomicEnd : unit -> unit
      val doAtomic : (unit -> unit) -> unit
   end
structure Critical : CRITICAL =
   struct
      structure Thread = MLton.Thread

      val atomicBegin = Thread.atomicBegin
      val atomicEnd = Thread.atomicEnd
      fun doAtomic f = (atomicBegin (); f (); atomicEnd ())
   end

structure Main =
   struct
      structure Signal = MLton.Signal
      structure Itimer = MLton.Itimer
         
      val alrmHandler = fn t => t
      fun setItimer t =
         Itimer.set (Itimer.Real,
                     {value = t,
                      interval = t})
      fun setAlrmHandler h =
         Signal.setHandler (Itimer.signal Itimer.Real, h)

      fun print s =
         Critical.doAtomic (fn () => TextIO.print s)

      fun doit n =
         let
            val () = setAlrmHandler (Signal.Handler.handler alrmHandler)
            val () = setItimer (Time.fromMilliseconds 10)

            fun loop i =
               if i > n 
                  then OS.Process.exit OS.Process.success
                  else let
                          val i' = (Int.toString i) ^ "\n"
                          fun loop' j =
                             if j > i then ()
                             else (print i'
                                   ; loop' (j + 1))
                       in
                          loop' 0
                          ; loop (i + 1)
                       end
         in
            loop 0
         end
   end

val _ = Main.doit 500
