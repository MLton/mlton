structure Timer: TIMER =
   struct
      type cpu_timer = {gc: Time.time,
			sys: Time.time,
			usr: Time.time}

      fun startCPUTimer (): cpu_timer =
	 let
	    val {gc = {utime = gcu, ...},
		 self = {utime = selfu, stime = selfs}, ...} =
	       MLtonRusage.rusage ()
	 in
	    {gc = gcu,
	     sys = selfs,
	     usr = selfu}
	 end

      fun checkCPUTimer ({gc, sys, usr}: cpu_timer) =
	 let
	    val {gc = g, sys = s, usr = u} = startCPUTimer ()
	    val op - = Time.-
	 in
	    {gc = g - gc,
	     sys = s - sys,
	     usr = u - usr}
	 end

      val totalCPUTimer =
	 let val t = startCPUTimer ()
	 in fn () => checkCPUTimer t
	 end

      val checkGCTime = #gc o checkCPUTimer
      val checkCPUTimer = fn t => let val {usr, sys, ...} = checkCPUTimer t
				  in {usr = usr, sys = sys}
				  end

      type real_timer = Time.time

      fun startRealTimer (): real_timer = Time.now ()

      fun checkRealTimer (t: real_timer): Time.time =
	 Time.- (startRealTimer (), t)
	 
      val totalRealTimer =
	 let val t = startRealTimer ()
	 in fn () => checkRealTimer t
	 end
   end
