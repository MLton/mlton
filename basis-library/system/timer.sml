structure Timer: TIMER =
   struct
      type cpu_timer = {gc: Time.time,
			sys: Time.time,
			usr: Time.time}

      fun startCPUTimer (): cpu_timer =
	 let
	    val {gc = {utime = gcu, stime = gcs},
		 self = {utime = selfu, stime = selfs}, ...} =
	       MLton.Rusage.rusage ()
	 in
	    {gc = Time.+ (gcu, gcs),
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
	 let
	    val t = startCPUTimer ()
	 in fn () => checkCPUTimer t
	 end

      type real_timer = Time.time

      fun startRealTimer (): real_timer = Posix.ProcEnv.time ()

      fun checkRealTimer (t: real_timer): Time.time =
	 Time.- (Posix.ProcEnv.time (), t)
	 
      val totalRealTimer =
	 let
	    val t = startRealTimer ()
	 in
	    fn () => checkRealTimer t
	 end
   end
