open MLton

fun timeLimit (t: Time.time, f: unit -> 'a): 'a option =
   let
      val which = Itimer.Real
      val signal = Itimer.whichSignal which
      val res =
	 Thread.switch
	 (fn cur: 'a option Thread.t =>
	  let
	     val _ = Signal.handleWith' (signal, fn _ =>
					 Thread.prepend (cur, fn () => NONE))
	     val _ =
		Itimer.set (which, {value = t,
				    interval = Time.zeroTime})
	     val t = Thread.new (fn () =>
				let val res = SOME (f ()) handle _ => NONE
				in Thread.switch (fn _ => (cur, res))
				end)
	  in (t, ())
	  end)
      val _ = Signal.handleDefault signal
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
