open MLton MLton.Signal

fun timeLimit (t: Time.time, f: unit -> 'a): 'a option =
   let
      val which = Itimer.Real
      val signal = Itimer.signal which
      val res =
         Thread.switch
         (fn cur: 'a option Thread.t =>
          let
             val _ = setHandler (signal,
                                 Handler.handler
                                 (fn _ => Thread.prepare (cur, NONE)))
             val _ =
                Itimer.set (which, {value = t,
                                    interval = Time.zeroTime})
             val t = Thread.new (fn () =>
                                 let val res = SOME (f ()) handle _ => NONE
                                 in Thread.switch (fn _ => Thread.prepare (cur, res))
                                 end)
          in Thread.prepare (t, ())
          end)
      val _ = setHandler (signal, Handler.default)
   in
      res
   end
                 
val _ =
   case timeLimit (Time.fromSeconds 10,
                   let fun loop () = loop ()
                   in loop
                   end) of
      NONE => print "success\n"
    | SOME _ => print "failure\n"
